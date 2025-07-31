;; Perspective and Projectile configuration
(use-package projectile
  :init
  (projectile-mode +1))

(use-package projectile-rails
  :after projectile
  :hook
  (projectile-mode . projectile-rails-global-mode)
  :config
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c p"))
  :init
  (persp-mode))

(defun my/start-luniva-perspective ()
  "Switch to the Luniva perspective if it exists; otherwise set it up."
  (interactive)
  (let* ((persp-name "lnv")
         (default-directory "~/Code/personal/luniva/")
         (existing (gethash persp-name (perspectives-hash)))) ; ← check if already created
    (persp-switch persp-name)
    (unless existing
      ;; Only run once when creating the perspective
      (find-file "README.md")
      (dolist (spec
               '(("*rails-server*" . "bin/rails server")
                 ("*rails-console*" . "bin/rails console")
                 ("*rails-db*" . "bin/rails dbconsole")
                 ("*vite-dev*" . "bin/vite dev")))
        (let ((buf (generate-new-buffer (car spec))))
          (with-current-buffer buf
            (vterm-mode)
            (vterm-send-string (cdr spec))
            (vterm-send-return))
          (persp-add-buffer buf))))
    (message "Switched to perspective: %s" persp-name)))

(defun my/setup-luniva-buffer-keys ()
  (when (string= (persp-name (persp-curr)) "lnv")
    (local-set-key (kbd "M-1") (lambda () (interactive) (switch-to-buffer "*rails-server*")))
    (local-set-key (kbd "M-2") (lambda () (interactive) (switch-to-buffer "*rails-console*")))
    (local-set-key (kbd "M-3") (lambda () (interactive) (switch-to-buffer "*rails-db*")))
    (local-set-key (kbd "M-4") (lambda () (interactive) (switch-to-buffer "*vite-dev*")))))

(add-hook 'buffer-list-update-hook #'my/setup-luniva-buffer-keys)

(defun my/select-project-layout ()
  "Prompt to select and launch a project-specific perspective layout."
  (interactive)
  (let* ((projects
          '(("luniva" . my/start-luniva-perspective)
            ))
         (choice (completing-read "Select project: " (mapcar #'car projects))))
    (when-let ((fn (cdr (assoc choice projects))))
      (funcall fn))))(defun my/select-project-layout ()
  "Prompt to select and launch a project-specific perspective layout."
  (interactive)
  (let* ((projects
          '(("luniva" . my/start-luniva-perspective)
            ))
         (choice (completing-read "Select project: " (mapcar #'car projects))))
    (when-let ((fn (cdr (assoc choice projects))))
      (funcall fn))))
(global-set-key (kbd "C-c s p") #'my/select-project-layout)

;; Consult - Perspective configuration
(defvar consult--source-perspective
  (list :name     "Perspective"
        :narrow   ?s
        :category 'buffer
        :state    #'consult--buffer-state
        :default  t
        :items    #'persp-get-buffer-names))

(unless (member consult--source-perspective consult-buffer-sources)
  (push consult--source-perspective consult-buffer-sources))

(provide 'plugins/projects)
