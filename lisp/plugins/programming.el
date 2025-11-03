(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
(provide 'plugins/programming)

(defun my/vterm-new ()
  "Create a new vterm buffer with a unique name."
  (interactive)
  (let ((vterm-buffer-name (generate-new-buffer-name "*vterm*")))
    (vterm)))

(global-set-key (kbd "C-c t") #'my/vterm-new)
