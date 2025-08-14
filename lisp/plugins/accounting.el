;; Accounting configuration
(use-package hledger-mode
  :ensure t
  :mode ("\\.journal\\'" . hledger-mode)
  :config
  (setq hledger-jfile "~/Documents/org/finance/index.journal")
  (setq hledger-currency-string "₫")
  (define-key hledger-mode-map (kbd "C-c C-r") #'hledger-run-command))

;; This is important to make hledger works!
(defun my-hledger-mode-setup ()
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil))

(add-hook 'hledger-mode-hook #'my-hledger-mode-setup)

(defun hledger-completion-accounts ()
  (when-let ((bounds (and (boundp 'hledger-accounts-cache)
                          (bounds-of-thing-at-point 'symbol))))
    (list (car bounds) (point) hledger-accounts-cache)))

(add-hook 'hledger-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions 'hledger-completion-accounts)))

(define-key hledger-mode-map (kbd "C-c =") 'hledger-increment-entry-date)
(define-key hledger-mode-map (kbd "C-c -") 'hledger-decrement-entry-date)

(provide 'plugins/accounting)
