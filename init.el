;; Load paths
(add-to-list 'load-path "~/.config/emacs/themes/modus-themes")
(add-to-list 'load-path "~/.config/emacs/packages/vue-ts-mode")

;; Load configuration, no need for recursively load for now
(add-to-list 'load-path "~/.config/emacs/lisp")
(add-to-list 'load-path "~/.config/emacs/lisp/plugins")

(require 'core)
(require 'keybindings)
(require 'ui)
(require 'packages)
(require 'modeline)

(require 'plugins/accounting)
(require 'plugins/completion)
(require 'plugins/evils)
(require 'plugins/lsp)
(require 'plugins/vcs)
(require 'plugins/note-taking)
(require 'plugins/projects)
(require 'plugins/terminal)
(require 'plugins/treesitter)

(dolist (mode '(org-mode-hook
		term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

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
 '(package-selected-packages
   '(affe cape corfu diff-hl doom-themes ef-themes evil-anzu
	  evil-collection evil-leader evil-surround flx fringe-helper
	  hledger-mode hotfuzz lsp-ui magit marginalia nerd-icons
	  ob-mermaid orderless org-modern org-pdftools
	  org-super-agenda org-web-tools pdf-view-restore perspective
	  projectile-rails shrink-path vertico vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
