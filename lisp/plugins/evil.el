;; Evil-related configuration
(defun my/split-window-right()
  "Split window vertically, balance and move to new window"
  (interactive)
  (select-window (split-window-right))
  (balance-windows))

(defun my/split-window-below()
  "Split window horizontally, balance and move to new window"
  (interactive)
  (select-window (split-window-below))
  (balance-windows))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)

  ;; Set leader for all states you care about
  (dolist (state '(normal visual motion))
    (evil-set-leader state (kbd "SPC")))

  ;; Define bindings globally in all states
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>sv") 'my/split-window-right
    (kbd "<leader>sh") 'my/split-window-below
    (kbd "<leader>se") 'balance-windows
    (kbd "<leader>sf") (lambda () (interactive) (enlarge-window-horizontally 999))
    (kbd "<leader>sm") (lambda () (interactive) (shrink-window-horizontally 999))
    (kbd "<leader>ss") 'window-swap-states
    (kbd "<leader>ff") 'projectile-find-file
    (kbd "<leader>fb") 'consult-buffer
    (kbd "<leader>fv") 'my/consult-ripgrep-from-selection
    (kbd "<leader>fw") 'affe-grep
    (kbd "<leader>q") 'flymake-show-buffer-diagnostics))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package anzu
  :init
  (global-anzu-mode +1))

(use-package evil-anzu
  :after (evil anzu))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(provide 'plugins/evil)
