(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless)))
  :custom
  (lsp-completion-provider :none)
  :hook ((ruby-mode . lsp)
         (js-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands lsp)
(setq lsp-headerline-breadcrumb-enable nil)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package which-key
    :config
    (which-key-mode))

(add-hook 'sql-mode-hook 'lsp)
(setq lsp-sqls-workspace-config-path nil)
(setq lsp-sqls-workspace-config-path "workspace")

(provide 'lsp)
