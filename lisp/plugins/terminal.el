;; Terminal configuration
(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "M-q") #'vterm-send-next-key)
  :custom
  (vterm-timer-delay 0.01))
(advice-add #'vterm--redraw :around (lambda (fun &rest args) (let ((cursor-type cursor-type)) (apply fun args))))

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-l")
    (lambda ()
      (interactive)
      (cond
       ((and (boundp 'evil-local-mode) evil-local-mode
             (eq evil-state 'insert))
        (vterm-send-C-l))
       ((and (boundp 'evil-local-mode) evil-local-mode
             (eq evil-state 'normal))
        (windmove-right))))))

(provide 'plugins/terminal)
