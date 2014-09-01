EMACS = emacs
SRCS  = js2-closure.el
OBJS  = $(SRCS:.el=.elc)

test: js2-mode.el
	${EMACS} -batch -Q -L . \
		$(addprefix -l ,$(wildcard *-test.el)) \
		-f ert-run-tests-batch-and-exit

README.md: make-readme-markdown.el js2-closure.el
	${EMACS} --script $< <js2-closure.el >$@ 2>/dev/null

js2-mode.el:
	wget -q -O $@ https://raw.githubusercontent.com/mooz/js2-mode/master/js2-mode.el

make-readme-markdown.el:
	wget -q -O $@ https://raw.githubusercontent.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el

.INTERMEDIATE: make-readme-markdown.el js2-mode.el
.PHONY: README.md
