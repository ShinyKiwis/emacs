;; GPTel configuration
(use-package gptel
  :config
  (setq gptel-model 'gpt-4.1)
  (setq gptel-default-mode 'markdown-mode)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot")))

(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
(add-hook 'gptel-post-response-functions 'gptel-end-of-response)

;; GPTel tools
;; + to generate longer summary
;; M-RET to continue in chat buffer
(require 'posframe)
(require 'gptel-quick)

(use-package gptel-agent
  :vc ( :url "https://github.com/karthink/gptel-agent"
        :rev :newest)
  :config (gptel-agent-update))

(use-package gptel-magit
  :ensure t
  :hook (magit-mode . gptel-magit-install))

(use-package agent-shell
  :custom
  (agent-shell-show-context-usage-indicator t)
  (agent-shell-show-usage-at-turn-end t)
  :config
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)

  ;; Configure *agent-shell-diff* buffers to start in Emacs state
  (add-hook 'diff-mode-hook
	    (lambda ()
	      (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
		(evil-emacs-state))))

  ;; Global leader binding to toggle agent-shell
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>as") 'agent-shell-toggle)

  ;; Leader binding for help/usage, available only in agent-shell-mode
  (evil-define-key '(normal visual motion) agent-shell-mode-map
    (kbd "<leader>ah") 'agent-shell-help-menu))

;; Copilot configuration for auto code completion
(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  (setq copilot-indent-offset-warning-disable t)
  (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion))

(use-package eca
  :ensure t
  :config
  (setq eca-custom-command '("~/.config/emacs/eca/eca" "server")))

(provide 'plugins/llm)
