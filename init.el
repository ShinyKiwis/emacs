;; Load paths
(add-to-list 'load-path "~/.config/emacs/themes/modus-themes")
(add-to-list 'load-path "~/.config/emacs/packages/vue-ts-mode")

;; Load configuration, no need for recursively load for now
(add-to-list 'load-path "~/.config/emacs/lisp")
(add-to-list 'load-path "~/.config/emacs/lisp/plugins")
(add-to-list 'load-path "~/.config/emacs/lisp/projects")

;; Store Emacs customization settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load the custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

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
(require 'plugins/workflow)
(require 'plugins/programming)
(require 'plugins/entertainment)

(dolist (mode '(org-mode-hook
		term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(use-package emacs
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p))
