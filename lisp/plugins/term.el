;; Terminal configuration
(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "M-q") #'vterm-send-next-key)
  :custom
  (vterm-timer-delay 0.01))

(provide 'plugins/term)
