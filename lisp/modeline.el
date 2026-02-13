(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-vcs-max-length 40)
  (setq doom-modeline-position-line-format nil)
  (setq doom-modeline-enable-buffer-position nil)
  (setq doom-modeline-buffer-encoding nil))

(provide 'modeline)
