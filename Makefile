.PHONY: test

test:
	@rm -f .test-org-id-locations
	emacs -Q --batch \
		-L . \
		-l ob-coffee.el \
		-l coffee-mode.el \
		-l test-ob-coffee.el \
		--eval "(progn \
								(require 'coffee-mode) (require 'ob) (require 'ob-eval) \
	              (setq org-confirm-babel-evaluate nil) \
	              (org-babel-do-load-languages \
	                'org-babel-load-languages '((emacs-lisp . t) \
	                                            (sh . t) \
	                                            (org . t) \
	                                            (js . t) \
	                                            (coffee . t))))" \
	    -f ob-coffee-test-runall
