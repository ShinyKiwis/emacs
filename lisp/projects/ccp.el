(defun my/start-ccp-perspective ()
  "Switch to the CCP perspective if it exists; otherwise set it up."
  (interactive)
  (let* ((persp-name "ccp")
         (default-directory "~/Projects/chatwoot/")
         (existing (gethash persp-name (perspectives-hash))))
    (persp-switch persp-name)
    (unless existing
      ;; Only run once when creating the perspective
      (find-file "README.md")
      (dolist (spec
               '(("*rails-server*" . "bin/rails server")
                 ("*rails-console*" . "bin/rails console")
                 ("*vite*" . "bin/vite dev")
                 ("*sidekiq*" . "bundle exec sidekiq -C config/sidekiq.yml")
                 ("*console*" . nil)))
        (let ((buf (generate-new-buffer (car spec))))
          (with-current-buffer buf
            (vterm-mode)
            (vterm-send-string (cdr spec))
            (vterm-send-return))
          (persp-add-buffer buf))))
    (message "Switched to perspective: %s" persp-name)))

(defun my/setup-ccp-buffer-keys ()
  (when (string= (persp-name (persp-curr)) "ccp")
    (local-set-key (kbd "M-1") (lambda () (interactive) (switch-to-buffer "*rails-server*")))
    (local-set-key (kbd "M-2") (lambda () (interactive) (switch-to-buffer "*rails-console*")))
    (local-set-key (kbd "M-3") (lambda () (interactive) (switch-to-buffer "*vite*")))
    (local-set-key (kbd "M-4") (lambda () (interactive) (switch-to-buffer "*sidekiq*")))
    (local-set-key (kbd "M-5") (lambda () (interactive) (switch-to-buffer "*console*")))))

(add-hook 'buffer-list-update-hook #'my/setup-ccp-buffer-keys)

(provide 'ccp)
