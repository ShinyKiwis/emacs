;; Treesitter configuration

(setq treesit-language-source-alist
      '((ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml       "https://github.com/ikatyang/tree-sitter-yaml")
	(vue        "https://github.com/ikatyang/tree-sitter-vue")
	(css "https://github.com/tree-sitter/tree-sitter-css")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")))

(setq major-mode-remap-alist
      '((ruby-mode . ruby-ts-mode)
        (python-mode . python-ts-mode)
        (js-mode . js-ts-mode)
	(dockerfile-mode . dockerfile-ts-mode)))

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.dockerignore\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; Handle vue-ts-mode
(require 'vue-ts-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode))

(setq treesit-font-lock-level 4)
(provide 'plugins/treesitter)
