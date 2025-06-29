;; Projectile and project-related setup
(use-package projectile
  :init
  (projectile-mode +1))
(use-package projectile-rails
  :after projectile
  :hook (projectile-mode . projectile-rails-global-mode))
(define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map)

(provide 'projects)
