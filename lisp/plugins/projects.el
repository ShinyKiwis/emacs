;; Load project files, this folder is ignored, different machine may have different projects
(require 'imgongo)
(require 'writebook)

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
	  "\\*vite-dev\\*"
	  "\\*console\\*"
          ))
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

(defun my/select-project-layout ()
  "Prompt to select and launch a project-specific perspective layout."
  (interactive)
  (let* ((projects
          '(("wb" . my/start-wb-perspective)
            ("imgongo" . my/start-imgongo-perspective)))
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
