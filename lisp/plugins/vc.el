;; Version-Control configuration
(use-package magit
  :ensure t)
(setq magit-completing-read-function #'completing-read-default)

(provide 'plugins/vc)
