;;; js2-closure-test.el --- Unit tests for js2-closure
;;; Commentary:
;;; Code:

(require 'ert)
(require 'js2-closure)

(defun get-ast ()
  "Extract `js2-mode' abstract syntax tree from current buffer."
  (js2-mode)
  (js2-reparse)
  js2-mode-ast)

(defun make-ast (source)
  "Extract `js2-mode' abstract syntax tree from SOURCE."
  (with-temp-buffer
    (insert source)
    (get-ast)))

(ert-deftest make-tree ()
  (should (equal (js2--closure-make-tree
                  '((goog dom)
                    (goog dom classlist)
                    (goog events)
                    (goog events EventManager)
                    (goog events EventTarget)
                    (goog net XhrIo)))
                 '((goog nil . ((dom t . ((classlist t)))
                                (events t . ((EventManager t)
                                             (EventTarget t)))
                                (net nil . ((XhrIo t)))))))))

(ert-deftest prune-provides--removes-nested-namespaces ()
  (should (equal (js2--closure-prune-provides
                  '((goog Foo)
                    (goog Foo FooBar)
                    (goog lol Bean)
                    (goog lol Bean dream)
                    (goog lol Bean dream Goth)
                    (goog lol Bean dream Steam punk)))
                 '((goog Foo)
                   (goog lol Bean)))))

(ert-deftest type-in-weird-jsdoc--gets-detected ()
  (let ((js2-closure-require-jsdoc t)
        (js2-closure-provides (js2--closure-make-tree
                               '((goog dom TagName)))))
    (with-temp-buffer
      (insert "
/** @type @const {!goog.dom.TagName} */
var hello;
")
      (js2-reparse)
      (should (equal (js2--closure-determine-requires (get-ast))
                     '("goog.dom.TagName"))))))

(ert-deftest member-tree ()
  (let ((tree (js2--closure-make-tree
               '((goog dom)
                 (goog dom classlist)
                 (goog events)
                 (goog events EventManager)
                 (goog events EventTarget)
                 (goog net XhrIo)))))
    (should (js2--closure-member-tree '(goog dom) tree))
    (should (js2--closure-member-tree '(goog dom classlist) tree))
    (should (js2--closure-member-tree '(goog events EventManager) tree))
    (should (js2--closure-member-tree '(goog events EventTarget) tree))
    (should (js2--closure-member-tree '(goog net XhrIo) tree))
    (should (not (js2--closure-member-tree '(goog) tree)))
    (should (not (js2--closure-member-tree '(goog net) tree)))
    (should (not (js2--closure-member-tree '(blob) tree)))
    (should (not (js2--closure-member-tree '(blob) nil)))
    (should (not (js2--closure-member-tree nil tree)))
    (should (not (js2--closure-member-tree nil nil)))))

(ert-deftest determine-requires--empty-buffer--returns-empty ()
  (should (not (js2--closure-determine-requires (make-ast "")))))

(ert-deftest determine-requires--remove-unused ()
  (let ((js2-closure-remove-unused t)
        (ast (make-ast "goog.require('foo');")))
    (should (not (js2--closure-determine-requires ast)))))

(ert-deftest determine-requires--whitelist ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-whitelist '("foo"))
        (ast (make-ast "goog.require('foo');")))
    (should (equal (js2--closure-determine-requires ast)
                   '("foo")))))

(ert-deftest determine-requires--dont-remove-unused ()
  (let ((js2-closure-remove-unused nil)
        (ast (make-ast "goog.require('foo');")))
    (should (equal (js2--closure-determine-requires ast)
                   '("foo")))))

(ert-deftest determine-requires--function-call ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-provides (js2--closure-make-tree
                               '((goog dom))))
        (ast (make-ast "goog.dom.getElement('foo');")))
    (should (equal (js2--closure-determine-requires ast)
                   '("goog.dom")))))

(ert-deftest determine-requires--reference ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-provides (js2--closure-make-tree
                               '((goog dom))))
        (ast (make-ast "foo = goog.dom.getElement;")))
    (should (equal (js2--closure-determine-requires ast)
                   '("goog.dom")))))

(ert-deftest goog-module-get--gets-required-regardless-of-provides ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-provides (js2--closure-make-tree
                               '((goog module))))
        (ast (make-ast "var MyModule = goog.module.get('foo.MyModule');")))
    (should (equal (js2--closure-determine-requires ast)
                   '("foo.MyModule"
                     "goog.module")))))

(ert-deftest determine-requires--already-required ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-provides (js2--closure-make-tree
                               '((goog dom)
                                 (goog events EventManager))))
        (ast (make-ast "
goog.provide('foo.Bar');
goog.require('goog.dom');
goog.require('goog.events');
foo.Bar = function() {
  goog.dom.getElement('bog');
  new goog.events.EventManager();
};
")))
    (should (equal (js2--closure-determine-requires ast)
                   '("goog.dom"
                     "goog.events.EventManager")))))

(ert-deftest determine-requires--provide-is-ignored ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-provides (js2--closure-make-tree
                               '((foo Bar))))
        (ast (make-ast "
goog.provide('foo.Bar');
new foo.Bar();
")))
    (should (not (js2--closure-determine-requires ast)))))

(ert-deftest determine-requires--dont-include-parent-namespaces ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-provides (js2--closure-make-tree
                               '((goog events)
                                 (goog events EventManager))))
        (ast (make-ast "new goog.events.EventManager();")))
    (should (equal (js2--closure-determine-requires ast)
                   '("goog.events.EventManager")))))

(ert-deftest replace-closure-requires ()
  (let (contents)
    (with-temp-buffer
      (insert "
goog.provide('jart.lol');
goog.provide('jart.lol.Hog');\n
goog.require('black.angel');
goog.require('not.insane');\n\n
jart.lol.funk = function() {};
")
      (js2--closure-replace-closure-requires
       '("a.cat"
         "black.angel"
         "down.with.bill.gates"))
      (setq contents (buffer-substring-no-properties
                      (point-min) (point-max))))
    (should (equal contents "
goog.provide('jart.lol');
goog.provide('jart.lol.Hog');\n
goog.require('a.cat');
goog.require('black.angel');
goog.require('down.with.bill.gates');\n\n
jart.lol.funk = function() {};
"))))

(ert-deftest delete-requires--removes-leading-line ()
  (let (contents)
    (with-temp-buffer
      (insert "
goog.provide('jart.lol');\n
goog.require('black.angel');
goog.require('not.insane');\n\n
jart.lol.funk = function() {};
")
      (js2--closure-delete-requires)
      (setq contents (buffer-substring-no-properties
                      (point-min) (point-max))))
    (should (equal contents "
goog.provide('jart.lol');\n\n
jart.lol.funk = function() {};
"))))

(ert-deftest replace-closure-requires--no-requires--insert-after-provides ()
  (let (contents)
    (with-temp-buffer
      (insert "
goog.provide('jart.lol.Hog');
goog.provide('jart.lol.Mog');\n\n\n
jart.lol.Hog = function() {};
")
      (js2--closure-replace-closure-requires
       '("a.cat"
         "black.angel"
         "down.with.bill.gates"))
      (setq contents (buffer-substring-no-properties
                      (point-min) (point-max))))
    (should (equal contents "
goog.provide('jart.lol.Hog');
goog.provide('jart.lol.Mog');\n
goog.require('a.cat');
goog.require('black.angel');
goog.require('down.with.bill.gates');\n\n\n
jart.lol.Hog = function() {};
"))))

(ert-deftest no-requires-or-provides--inserts-after-fileoverview ()
  (let (contents)
    (with-temp-buffer
      (insert "
/** @fileoverview blah */\n
jart.lol.Hog = function() {};
")
      (js2--closure-replace-closure-requires '("a.cat"))
      (setq contents (buffer-substring-no-properties
                      (point-min) (point-max))))
    (should (equal contents "
/** @fileoverview blah */\n
goog.require('a.cat');\n
jart.lol.Hog = function() {};
"))))

(ert-deftest no-requires-provides-overview--inserts-after-comment ()
  (let (contents)
    (with-temp-buffer
      (insert "// oh yeah\n
jart.lol.Hog = function() {};
")
      (js2--closure-replace-closure-requires '("a.cat"))
      (setq contents (buffer-substring-no-properties
                      (point-min) (point-max))))
    (should (equal contents "// oh yeah\n
goog.require('a.cat');\n
jart.lol.Hog = function() {};
"))))

(ert-deftest extract-jsdocs ()
  (with-temp-buffer
    (insert "
/** @type {!goog.dom.TagName} */
var hello;
")
    (should (equal (js2--closure-extract-jsdocs (get-ast))
                   '("/** @type {!goog.dom.TagName} */")))))

(ert-deftest extract-namespaces--empty ()
  (should (equal (js2--closure-extract-namespaces
                  "/** lol */")
                 '())))

(ert-deftest extract-namespaces--compound-type ()
  (should (equal (js2--closure-extract-namespaces
                  "/** @return {!Array<!foo.Bar>|string} */")
                 '("string" "foo.Bar" "Array"))))

(ert-deftest extract-namespaces--multiline-compound-type ()
  (should (equal (js2--closure-extract-namespaces
                  "/** @return {!Array<!foo.Bar>|\n * string} */")
                 '("string" "foo.Bar" "Array"))))

(ert-deftest extract-namespaces--extends ()
  (should (equal (js2--closure-extract-namespaces
                  "/** @extends foo.Bar */")
                 '("foo.Bar"))))

(ert-deftest extract-namespaces--sloppy-code ()
  (should (equal (js2--closure-extract-namespaces
                  "/**@param{foo} */")
                 '("foo"))))

(ert-deftest extract-namespaces--unknown-tag--ignored ()
  (should (equal (js2--closure-extract-namespaces
                  "/** @paramz {foo} */")
                 '())))

(ert-deftest type-in-jsdoc--gets-detected ()
  (let ((js2-closure-require-jsdoc t)
        (js2-closure-provides (js2--closure-make-tree
                               '((goog dom TagName)))))
    (with-temp-buffer
      (insert "
/** @type {!goog.dom.TagName} */
var hello;
/** @type {!goog.dom.TagName} */
var there;
")
      (js2-reparse)
      (should (equal (js2--closure-determine-requires (get-ast))
                     '("goog.dom.TagName"))))))

(ert-deftest type-in-jsdoc--namespace-is-provided--does-not-get-required ()
  (let ((js2-closure-require-jsdoc t)
        (js2-closure-provides (js2--closure-make-tree
                               '((goog dom TagName)))))
    (with-temp-buffer
      (insert "
goog.provide('goog.dom.TagName');
/** @type {!goog.dom.TagName} */
var hello;
/** @type {!goog.dom.TagName} */
var there;
")
      (js2-reparse)
      (should (equal (js2--closure-determine-requires (get-ast))
                     '())))))

(ert-deftest type-in-jsdoc--doesnt-get-detected-when-feature-is-off ()
  (let ((js2-closure-require-jsdoc nil)
        (js2-closure-provides (js2--closure-make-tree
                               '((goog dom TagName)))))
    (with-temp-buffer
      (insert "
/** @type {!goog.dom.TagName} */
var hello;
/** @type {!goog.dom.TagName} */
var there;
")
      (js2-reparse)
      (should (equal (js2--closure-determine-requires (get-ast))
                     '())))))

(ert-deftest empty-buffer--will-be-operated-on ()
  (should (with-temp-buffer (js2--closure-has-traditional-namespaces))))

(ert-deftest goog-module-buffer--will-not-be-operated-on ()
  (should (not (with-temp-buffer (insert "goog.module('foo');\n")
                                 (js2--closure-has-traditional-namespaces)))))

(ert-deftest es6-module-buffer--will-not-be-operated-on ()
  (should (not (with-temp-buffer (insert "import 'foo';\n")
                                 (js2--closure-has-traditional-namespaces)))))

;;; js2-closure-test.el ends here
