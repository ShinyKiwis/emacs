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

;; Copilot configuration for auto code completion
(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(provide 'plugins/llm)
