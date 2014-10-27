;;; js2-closure.el --- Google Closure dependency manager

;; Copyright (C) 2014 Google, Inc.
;; Author: Justine Tunney <jart@google.com>
;; Version: 1.4
;; URL: http://github.com/jart/js2-closure
;; Package-Requires: ((js2-mode "20140114"))

;;; Commentary:
;;
;; Do you use Emacs, `js2-mode', and Google's Closure Library?  Do you get
;; frustrated writing your `goog.require` statements by hand?  If that's the
;; case, then this extension is going to make you very happy.
;;
;; js2-closure is able to analyse the JavaScript code in your buffer to
;; determine which imports you need, and then update the `goog.require` list at
;; the top of your buffer.  It works like magic.  It also runs instantaneously,
;; even if you have a big project.

;;; Installation:
;;
;; Install this package from MELPA using `M-x install-package` and type
;; `js2-closure`.  If you aren't already using MELPA, see:
;; http://melpa.milkbox.net/#/getting-started
;;
;; You then need to run a helper script that crawls all your JavaScript sources
;; for `goog.provide` statements, in addition to your Closure Templates (Soy)
;; for `{namespace}` declarations (assuming you're using the Soy to JS
;; compiler).  You must also download the source code to the Closure Library
;; and pass this script the path of the `closure/goog` folder.
;;
;; Here's an example command for regenerating the provides index that you can
;; add to your `~/.bashrc` file:
;;
;;     jsi() {
;;       local github="https://raw.githubusercontent.com"
;;       local script="js2-closure-provides.sh"
;;       bash <(wget -qO- ${github}/jart/js2-closure/master/${script}) \
;;         ~/code/closure-library/closure/goog \
;;         ~/code/my-project/js \
;;         ~/code/my-project/soy \
;;         >~/.emacs.d/js2-closure-provides.el
;;     }
;;
;; That will generate an index file in your `~/.emacs.d` directory.  If you
;; want to store it in a different place, then `js2-closure-provides-file' will
;; need to be customised.
;;
;; This index file will be loaded into Emacs automatically when the timestamp
;; on the file changes.  You need to re-run the script manually whenever new
;; `goog.provide` statements are added or removed.  Automating that part is up
;; to you.

;;; Usage:
;;
;; To use this, you simply run `M-x js2-closure-fix` inside your `js2-mode'
;; buffer.  This will regenerate the list of `goog.require` statements by
;; crawling your source code to see which identifiers are being used.
;;
;; If you want the magic to happen automatically each time you save the buffer,
;; then add the following to your `.emacs` file:
;;
;;     (eval-after-load 'js2-mode
;;       '(add-hook 'before-save-hook 'js2-closure-save-hook))
;;
;; Alternatively, you can use a key binding as follows:
;;
;;     (eval-after-load 'js2-mode
;;       '(define-key js2-mode-map (kbd "C-c C-c") 'js2-closure-fix))
;;
;; This tool was written under the assumption that you're following Google's
;; JavaScript style guide: http://goo.gl/Ny5WxZ

;;; Code:

(require 'js2-mode)

(defcustom js2-closure-remove-unused t
  "Determines if unused goog.require statements should be auto-removed.
You might want to consider using `js2-closure-whitelist' rather than
disabling this feature."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-closure-whitelist
  '("goog.testing.asserts")
  "List of goog.require namespaces that should never be removed."
  :type '(repeat string)
  :group 'js2-mode)

(defcustom js2-closure-provides-file
  (concat user-emacs-directory "js2-closure-provides.el")
  "Filename of generated elisp file listing all provided namespaces."
  :type 'file
  :group 'js2-mode)

(defconst js2-closure-help-url
  "https://github.com/jart/js2-closure"
  "URL of documentation to provide help to lost souls.")

(defvar js2-closure-provides nil
  "Hierarchy of all closure provided namespaces.")

(defvar js2-closure-provides-modified nil
  "Modified timestamp of `js2-closure-provides-file'.")

(defun js2--closure-nested-namespace-p (identifier)
  "Return non-nil if IDENTIFIER has labels after one is capitalized."
  (let (result)
    (while identifier
      (let ((first-char (string-to-char (symbol-name (pop identifier)))))
        (when (and (>= first-char ?A)
                   (<= first-char ?Z))
          (setq result identifier
                identifier nil))))
    result))

(defun js2--closure-prune-provides (list)
  "Remove identifiers from LIST that shouldn't be required.

Nested namespaces such as `goog.Foo.Bar` are provided in the
Closure Library source code on several occasions.  However if you
try to require these namespaces, then `gjslint` will complain,
because it only wants us to require `goog.Foo`."
  (nreverse
   (let (result)
     (dolist (item list result)
       (when (not (js2--closure-nested-namespace-p item))
         (push item result))))))

(defun js2--closure-make-tree (list)
  "Turn sorted LIST of identifiers into a tree."
  (let (result)
    (while list
      (let (sublist
            (name (caar list))
            (is-leaf (null (cdar list))))
        (while (eq name (caar list))
          (let ((item (pop list)))
            (when (cdr item)
              (push (cdr item) sublist))))
        (let ((subtree (js2--closure-make-tree (nreverse sublist))))
          (push (cons name (cons is-leaf subtree)) result))))
    (nreverse result)))

(defun js2--closure-member-tree (identifier tree)
  "Return t if IDENTIFIER is a member of TREE."
  (let ((branch (assq (car identifier) tree)))
    (if (and branch (cdr identifier))
        (js2--closure-member-tree (cdr identifier) (cddr branch))
      (cadr branch))))

(defun js2--closure-make-identifier (node &optional names)
  "Turn NODE (or string) into an ordered list of interned NAMES."
  (cond ((js2-prop-get-node-p node)
         (js2--closure-make-identifier
          (js2-prop-get-node-left node)
          (js2--closure-make-identifier
           (js2-prop-get-node-right node)
           names)))
        ((and (js2-node-p node)
              (js2-prop-node-name node))
         (cons (intern (js2-prop-node-name node)) names))
        ((stringp node)
         (mapcar 'intern (split-string node "\\.")))))

(defun js2--closure-identifier-to-string (identifier)
  "Convert IDENTIFIER into a dotted string."
  (mapconcat 'symbol-name identifier "."))

(defun js2--closure-crawl (ast on-call on-identifier)
  "Crawl `js2-mode' AST and invoke callbacks on nodes.

ON-CALL will be invoked for all `js2-call-node' nodes, passing
the node itself as the first argument.

ON-IDENTIFIER is invoked for all identifiers, passing as an
argument the last `js2-prop-get-node' in the chain of labels
making up that identifier."
  (let (last)
    (js2-visit-ast
     ast
     (lambda (node endp)
       (unless endp
         (when (js2-call-node-p node)
           (funcall on-call node))
         (cond ((and (js2-prop-get-node-p node)
                     (or (not last)
                         (eq last (js2-prop-get-node-left node))))
                (setq last node))
               ((and (not (js2-prop-get-node-p node))
                     last)
                (funcall on-identifier last)
                (setq last nil)))
         t)))
    (when last
      (funcall on-call last))))

(defun js2--closure-determine-requires (ast)
  "Return sorted list of closure namespaces from AST to be imported."
  (let (provides requires references)
    (let ((on-call
           (lambda (node)
             (let ((funk (js2--closure-make-identifier
                          (js2-call-node-target node)))
                   (arg1 (car (js2-call-node-args node))))
               (cond ((and (equal funk '(goog provide))
                           (js2-string-node-p arg1))
                      (let ((item (js2--closure-make-identifier
                                   (js2-string-node-value arg1))))
                        (when (not (member item provides))
                          (push item provides))))
                     ((and (equal funk '(goog require))
                           (js2-string-node-p arg1))
                      (let ((item (js2--closure-make-identifier
                                   (js2-string-node-value arg1))))
                        (when (not (member item requires))
                          (push item requires))))))))
          (on-identifier
           (lambda (node)
             (let ((item (js2--closure-make-identifier node)))
               (while item
                 (cond ((member item provides)
                        (setq item nil))
                       ((member item requires)
                        (when (not (member item references))
                          (push item references))
                        (setq item nil))
                       ((js2--closure-member-tree item js2-closure-provides)
                        (when (not (member item references))
                          (push item references))
                        (setq item nil)))
                 (setq item (butlast item)))))))
      (js2--closure-crawl ast on-call on-identifier))
    (sort (let (result)
            (dolist (item requires)
              (when (or (not js2-closure-remove-unused)
                        (member (js2--closure-identifier-to-string item)
                                js2-closure-whitelist)
                        (member item references))
                (let ((namespace (js2--closure-identifier-to-string item)))
                  (push namespace result))))
            (dolist (item references result)
              (when (member item references)
                (let ((namespace (js2--closure-identifier-to-string item)))
                  (when (not (member namespace result))
                    (push namespace result))))))
          'string<)))

(defun js2--closure-replace-closure-requires (namespaces)
  "Replace the current list of requires with NAMESPACES."
  (save-excursion
    (goto-char 0)
    (if (search-forward-regexp "^goog.require(" nil t)
        (beginning-of-line)
      (progn (search-forward-regexp "^goog.provide(")
             (search-forward-regexp "^$")
             (open-line 1)))
    (while (and namespaces (search-forward-regexp
                            "^goog.require('\\([^']+\\)');" nil t))
      (when (not (string= (match-string 1) (car namespaces)))
        (if (not (string= (match-string 1) (cadr namespaces)))
            (replace-match (car namespaces) t t nil 1)
          (progn (beginning-of-line)
                 (insert (format "goog.require('%s');\n" (car namespaces))))))
      (setq namespaces (cdr namespaces)))
    (forward-line)
    (while (looking-at "^goog.require(")
      (delete-region (point) (save-excursion
                               (forward-line)
                               (point))))
    (while namespaces
      (insert (format "goog.require('%s');\n" (pop namespaces))))))

(defun js2--closure-file-modified (file)
  "Return modified timestamp of FILE."
  (nth 5 (file-attributes file)))

(defun js2--closure-load (file)
  "Load FILE with list of provided namespaces into memory."
  (interactive)
  (when (not (file-exists-p file))
    (error "Empty js2-closure provides (%s) See docs: %s"
           file js2-closure-help-url))
  (load file)
  (when (not js2-closure-provides)
    (error "Empty js2-closure-provides (%s) See docs: %s"
           file js2-closure-help-url))
  (setq js2-closure-provides (js2--closure-make-tree
                              (js2--closure-prune-provides
                               js2-closure-provides)))
  (setq js2-closure-provides-modified (js2--closure-file-modified file))
  (message (format "Loaded %s" file)))

;;;###autoload
(defun js2-closure-fix ()
  "Fix the `goog.require` statements in the current buffer.

This function assumes that all the requires are in one place and
sorted, without indentation or blank lines.  If you don't have
any requires, they'll be added after your provide statements.  If
you don't have those, then this routine will fail.

Effort was also made to avoid needlessly modifying the buffer,
since syntax coloring might take some time to kick back in.

This will automatically load `js2-closure-provides-file' into
memory if it was modified or not yet loaded."
  (interactive)
  (when (or (not js2-closure-provides)
            (time-less-p js2-closure-provides-modified
                         (js2--closure-file-modified
                          js2-closure-provides-file)))
    (js2--closure-load js2-closure-provides-file))
  (js2--closure-replace-closure-requires
   (js2--closure-determine-requires js2-mode-ast)))

;;;###autoload
(defun js2-closure-save-hook ()
  "Global save hook to invoke `js2-closure-fix' if in `js2-mode'.

To use this feature, add it to `before-save-hook'."
  (interactive)
  (when (eq major-mode 'js2-mode)
    (condition-case exc
        (js2-closure-fix)
      ('error
       (message (format "js2-closure-fix failed: [%s]" exc))))))

(provide 'js2-closure)

;;; js2-closure.el ends here
