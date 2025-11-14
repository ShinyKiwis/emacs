;; Version-Control configuration
(use-package magit
  :ensure t)
(setq magit-completing-read-function #'completing-read-default)
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer buffer '(display-buffer-same-window))))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (setq-default fringes-outside-margins t))

(provide 'plugins/vcs)
