;; Perspective and Projectile configuration
(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("C-~"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-window-height 30)
  (setq popper-reference-buffers
        '("\\*rails-server\\*"
          "\\*rails-console\\*"
          "\\*rails-db\\*"
          "\\*vite\\*"
          "\\*sidekiq\\*"
          "\\*console\\*"))
  (popper-mode +1)
  (popper-echo-mode -1)) 

(use-package projectile
  :init
  (projectile-mode +1))

(use-package projectile-rails
  :after projectile
  :hook (projectile-mode . projectile-rails-global-mode)
  :bind (:map projectile-rails-mode-map
              ("C-c r" . projectile-rails-command-map)))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c p"))
  :init
  (persp-mode))

;; Load project files, this folder is ignored, different machine may have different projects
(require 'imgongo)
(require 'writebook)

;; CCP project
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

(defun my/select-project-layout ()
  "Prompt to select and launch a project-specific perspective layout."
  (interactive)
  (let* ((projects '(("ccp" . my/start-ccp-perspective)))
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
