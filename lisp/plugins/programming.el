(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(defun my/vterm-new ()
  "Create a new vterm buffer with a unique name."
  (interactive)
  (let ((vterm-buffer-name (generate-new-buffer-name "*vterm*")))
    (vterm)))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (rjsx-mode . emmet-mode)
         (js-jsx-mode . emmet-mode)
         (vue-ts-mode . emmet-mode)
         (html-mode . emmet-mode)))

(use-package gdscript-mode
  :vc (:url "git@github.com:godotengine/emacs-gdscript-mode.git"))

(global-set-key (kbd "C-c t") #'my/vterm-new)
(provide 'plugins/programming)
