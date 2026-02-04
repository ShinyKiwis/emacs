(defun my/start-lxb-perspective ()
  "Switch to the lxb perspective if it exists; otherwise set it up."
  (interactive)
  (let* ((persp-name "lxb")
         (default-directory "~/Projects/lixibox/")
         (existing (gethash persp-name (perspectives-hash)))) ; ← check if already created
    (persp-switch persp-name)
    (unless existing
      ;; Only run once when creating the perspective
      (find-file "README.md")
      (dolist (spec
               '(("*lxb-rails-server*" . "bundle exec rails server -p 3001")
                 ("*lxb-webpack*" . "NODE_OPTIONS=--openssl-legacy-provider bin/webpack-dev-server")
                 ("*lxb-rails-console*" . "bin/rails console")
                 ("*lxb-sidekiq*" . "bundle exec sidekiq")
                 ("*lxb-console*" . nil)))
        (let ((buf (generate-new-buffer (car spec))))
          (with-current-buffer buf
            (vterm-mode)
            (vterm-send-string (cdr spec))
            (vterm-send-return))
          (persp-add-buffer buf))))
    (message "Switched to perspective: %s" persp-name)))

(defun my/setup-lxb-buffer-keys ()
  (when (string= (persp-name (persp-curr)) "lxb")
    (local-set-key (kbd "M-1") (lambda () (interactive) (switch-to-buffer "*lxb-rails-server*")))
    (local-set-key (kbd "M-2") (lambda () (interactive) (switch-to-buffer "*lxb-webpack*")))
    (local-set-key (kbd "M-3") (lambda () (interactive) (switch-to-buffer "*lxb-rails-console*")))
    (local-set-key (kbd "M-4") (lambda () (interactive) (switch-to-buffer "*lxb-sidekiq*")))
    (local-set-key (kbd "M-5") (lambda () (interactive) (switch-to-buffer "*lxb-console*")))))

(add-hook 'buffer-list-update-hook #'my/setup-lxb-buffer-keys)

(provide 'lxb)
