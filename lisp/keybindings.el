;; Keybindings
;; Function Keys:
;; F1  - Evaluate entire Emacs Lisp buffer
;; F5  - Toggle between light and dark themes
;; F8  - Open help command menu
(global-set-key (kbd "<f8>") 'help-command)
(define-key emacs-lisp-mode-map (kbd "<f1>") #'eval-buffer)

;; Vim-like keybinding
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)

;; Default-binding for move-text
(use-package move-text
  :ensure t
  :init
  ;; Default bindings:
  ;; M-<up>   -> move-text-up
  ;; M-<down> -> move-text-down
  (move-text-default-bindings))

(provide 'keybindings)
