;; Completion related plugins including:
;; Vertico
;; Affe
;; Cape
;; Marginalia
;; Orderless
;; Savehist
;; Hotfuzz

(use-package vertico
  :custom
  ((vertico-count 15)
   (vertico-resize t)
   (vertico-cycle t))
  :init
  (vertico-mode)
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous))

(use-package affe
  :ensure t)

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

(provide 'plugins/completion)
