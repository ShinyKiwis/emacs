;; Consult configuration
(use-package consult
  :ensure t)
(consult-customize consult--source-buffer :hidden t :default nil)

(defun my/consult-ripgrep-from-selection ()
  "Run `consult-ripgrep` with the selected region as input."
  (interactive)
  (let ((query (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 "")))
    (consult-ripgrep nil query)))

(provide 'plugins/consult)
