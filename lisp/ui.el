;; UI configuration
(defvar my/theme-dark 'doom-moonlight)
(defvar my/theme-light 'modus-operandi-tinted)
(defvar my/current-theme my/theme-dark)

(defun my/toggle-theme()
  "Toggle between light and dark themes."
  (interactive)
  (disable-theme my/current-theme)
  (setq my/current-theme
	(if (eq my/current-theme my/theme-light)
	    my/theme-dark
	  my/theme-light))
  (load-theme my/current-theme))

(global-set-key (kbd "<f5>") #'my/toggle-theme)

(add-hook 'server-after-make-frame-hook
          (lambda ()
            (set-face-attribute 'default nil :font "Inconsolata Nerd Font Mono" :height 160)
            (load-theme my/current-theme t)))

;; Nyan for fun!
(use-package nyan-mode
  :ensure t
  :config
  (setq nyan-minimum-window-width 100))

(provide 'ui)
