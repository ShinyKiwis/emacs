(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-vcs-max-length 40)
  (setq doom-modeline-position-line-format nil)
  (setq doom-modeline-enable-buffer-position nil)
  (setq doom-modeline-buffer-encoding nil))

(setq mode-line-right-align-edge 'right-fringe)

;; Config display-time
(setq display-time-format "%H:%M %m-%d"
      display-time-default-load-average nil
      display-time-mail-string ""
      display-time-use-mail-icon nil)

(display-time-mode 1)

(provide 'modeline)
