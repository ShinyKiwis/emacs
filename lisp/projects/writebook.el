(defun my/start-wb-perspective ()
  "Switch to the wb perspective if it exists; otherwise set it up."
  (interactive)
  (let* ((persp-name "wb")
         (default-directory "~/Code/opensource/writebook/")
         (existing (gethash persp-name (perspectives-hash))))
    (persp-switch persp-name)
    (unless existing
      ;; Only run once when creating the perspective
      (find-file "README.md")
      (dolist (spec
               '(("*rails-server*" . "bin/dev")
                 ("*rails-console*" . "bin/rails console")
                 ("*rails-db*" . "bin/rails dbconsole")))
        (let ((buf (generate-new-buffer (car spec))))
          (with-current-buffer buf
            (vterm-mode)
            (vterm-send-string (cdr spec))
            (vterm-send-return))
          (persp-add-buffer buf))))
    (message "Switched to perspective: %s" persp-name)))

(defun my/setup-wb-buffer-keys ()
  (when (string= (persp-name (persp-curr)) "wb")
    (local-set-key (kbd "M-1") (lambda () (interactive) (switch-to-buffer "*rails-server*")))
    (local-set-key (kbd "M-2") (lambda () (interactive) (switch-to-buffer "*rails-console*")))
    (local-set-key (kbd "M-3") (lambda () (interactive) (switch-to-buffer "*rails-db*")))
    (local-set-key (kbd "M-4") (lambda () (interactive) (switch-to-buffer "*vite-dev*")))))

(add-hook 'buffer-list-update-hook #'my/setup-wb-buffer-keys)

(provide 'writebook)
