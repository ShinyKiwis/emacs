;; Core configuration
(setq inhibit-startup-message t)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(global-eldoc-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 15)
(electric-pair-mode 1)
(file-name-shadow-mode 1)

;; Smooth scrolling
(setq scroll-margin 10
      scroll-conservatively 101
      scroll-step 1
      scroll-preserve-screen-position t
      fast-but-imprecise-scrolling t)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Ignoring case for completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(setq-default truncate-lines t)
(setq window-min-height 1)
(setq window-min-width 1)

;; Clean up backup files and auto saves into single folder
(defvar my/backup-dir (expand-file-name "~/.emacs-backup/"))
(unless (file-exists-p my/backup-dir)
  (make-directory my/backup-dir t))

(setq backup-directory-alist
      `(("." . ,my/backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,my/backup-dir t)))

(setq backup-by-copying t      ; Avoid symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; Use version numbers on backups

(recentf-mode 1)
(setq recentf-max-menu-items 15
      recentf-max-saved-items 50)

;; Dired config
(setq dired-dwim-target t)

(provide 'core)
