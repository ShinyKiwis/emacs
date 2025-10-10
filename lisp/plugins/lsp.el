;; LSP configuration
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless)))
  :custom
  (lsp-completion-provider :none)
  (lsp-signature-auto-activate nil)
  :hook ((js-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-completion-mode . my/lsp-mode-setup-completion))
  :bind (:map lsp-mode-map
              ("C-c s" . lsp-signature-activate))
  :commands lsp)
(setq lsp-headerline-breadcrumb-enable nil)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package which-key
    :config
    (which-key-mode))

(add-hook 'sql-mode-hook 'lsp)
(setq lsp-sqls-workspace-config-path nil)
(setq lsp-sqls-workspace-config-path "workspace")

;; Ruby Mode
(use-package inf-ruby
  :ensure t
  :hook ((compilation-filter . inf-ruby-auto-enter-and-focus)))

(use-package robe
  :ensure t
  :hook ((ruby-mode . robe-mode)
         (ruby-ts-mode . robe-mode)))

;; Clojure
(use-package cider
  :ensure t)

(provide 'plugins/lsp)
