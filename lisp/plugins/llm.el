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
