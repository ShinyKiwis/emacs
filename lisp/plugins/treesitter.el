;; Treesitter and syntax-related configuration

;; Setup Flycheck
(use-package flycheck
  :ensure t)

;; Treesitter
(setq treesit-language-source-alist
      '((ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (vue        "https://github.com/ikatyang/tree-sitter-vue")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (c "https://github.com/tree-sitter/tree-sitter-c" "v0.23.6")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")))

(setq major-mode-remap-alist
      '((ruby-mode . ruby-ts-mode)
        (python-mode . python-ts-mode)
        (js-mode . js-ts-mode)
        (c++-mode . c++-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)))

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.dockerignore\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"  . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; Handle YAML-mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :init
  (setq yaml-indent-offset 2))

;; Handle js-mode
(setq js-indent-level 2)

;; Handle vue-ts-mode
(require 'vue-ts-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode))

;; Handle HAML-mode
(use-package haml-mode
  :ensure t)

(setq treesit-font-lock-level 4)
(provide 'plugins/treesitter)
