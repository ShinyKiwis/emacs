;; Load paths
(add-to-list 'load-path "~/.config/emacs/themes/modus-themes")
(add-to-list 'load-path "~/.config/emacs/packages/vue-ts-mode")
(add-to-list 'load-path "~/.config/emacs/lisp")

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
(setq scroll-margin 10
      scroll-conservatively 101
      scroll-step 1
      scroll-preserve-screen-position t)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
(setq-default truncate-lines t)
(setq window-min-height 1)
(setq window-min-width 1)

;; Move backup files into single folder
(setq backup-directory-alist
      `(("." . "~/.config/emacs/backups")))
(setq backup-by-copying t) ;; Avoid symlinks
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
;; Move auto saves into single folder
(setq auto-save-file-name-transforms
      `((".*" "~/.config/emacs/auto-saves/" t)))

(recentf-mode 1)
(setq recentf-max-menu-items 15)
(setq ecentf-max-saved-items 50)

(defun my/split-window-right()
  "Split window vertically, balance and move to new window"
  (interactive)
  (select-window (split-window-right))
  (balance-windows))

(defun my/split-window-below()
  "Split window horizontally, balance and move to new window"
  (interactive)
  (select-window (split-window-below))
  (balance-windows))

;; F-key binding
(global-set-key (kbd "<f5>") #'my/toggle-theme)
(define-key emacs-lisp-mode-map (kbd "<f1>") #'eval-buffer)

;; Vim keybinding
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)

;; Theme setup
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

(add-hook 'server-after-make-frame-hook
          (lambda ()
            (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 130)
            (load-theme my/current-theme t)))

;; Package initialization
(require 'package)

;; Set package sources
(setq package-archives '(
			 ("org" . "https://orgmode.org/elpa/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package vertico
  :custom
  ((vertico-count 15)
   (vertico-resize t)
   (vertico-cycle t))
  :init
  (vertico-mode)
  :config
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous))

(use-package affe
  :ensure t)

(use-package consult
  :ensure t)
(consult-customize consult--source-buffer :hidden t :default nil)

(defvar consult--source-perspective
  (list :name     "Perspective"
        :narrow   ?s
        :category 'buffer
        :state    #'consult--buffer-state
        :default  t
        :items    #'persp-get-buffer-names))

(push consult--source-perspective consult-buffer-sources)

(use-package hotfuzz
  :after vertico)

(use-package orderless
  :custom
  (completion-styles '(hotfuzz orderless basic partial-completion))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles hotfuzz)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

(defun my/consult-ripgrep-from-selection ()
  "Run `consult-ripgrep` with the selected region as input."
  (interactive)
  (let ((query (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 "")))
    (consult-ripgrep nil query)))

;; Load config files
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)

  ;; Set leader for all states you care about
  (dolist (state '(normal visual motion))
    (evil-set-leader state (kbd "SPC")))

  ;; Define bindings globally in all states
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>sv") 'my/split-window-right
    (kbd "<leader>sh") 'my/split-window-below
    (kbd "<leader>se") 'balance-windows
    (kbd "<leader>sf") (lambda () (interactive) (enlarge-window-horizontally 999))
    (kbd "<leader>ff") 'projectile-find-file
    (kbd "<leader>fb") 'consult-buffer
    (kbd "<leader>fv") 'my/consult-ripgrep-from-selection
    (kbd "<leader>fw") 'affe-grep
    (kbd "<leader>q") 'flymake-show-buffer-diagnostics)

(use-package anzu
  :init
  (global-anzu-mode +1))

(use-package evil-anzu
  :after (evil anzu))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package corfu
  :init
  (global-corfu-mode)

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.01)
  (corfu-auto-prefix 0.01)
  (corfu-quit-no-match 'separator)

  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET" . corfu-insert)))

(add-hook 'corfu-mode-hook
          (lambda ()
            (setq-local completion-styles '(basic)
                        completion-category-overrides nil
                        completion-category-defaults nil)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package magit
  :ensure t)
(setq magit-completing-read-function #'completing-read-default)

(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "M-q") #'vterm-send-next-key)
  :custom
  (vterm-timer-delay 0.01))
(dolist (mode '(org-mode-hook
		term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0)))))

(require 'modeline)
(require 'treesitter)
(require 'lsp)
(require 'projects)
(require 'note-taking)

(use-package emacs
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     "4b88b7ca61eb48bb22e2a4b589be66ba31ba805860db9ed51b4c484f3ef612a7"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "fd22a3aac273624858a4184079b7134fb4e97104d1627cb2b488821be765ff17"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
