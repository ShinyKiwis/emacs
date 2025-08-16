(setq mode-line-format nil)
(kill-local-variable 'mode-line-format)
(force-mode-line-update)

(setq mode-line-right-align-edge 'right-fringe)
(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		(:eval (my-modeline-evil-state))
		" "
		(:eval (my-modeline-buffer-file-name))
		mode-line-format-right-align
		(:eval (my-modeline-org-clock))
		" "
		(:eval (my-modeline-git-branch))
		" "
		(:eval (my-modeline-clock))
		" "
		(:eval (my-modeline-major-mode))
		mode-line-end-spaces
		))

;; Buffer Name
(defun my-modeline-buffer-file-name ()
  "Show path relative to Git root if available; shorten intermediate dirs if too long."
  (if buffer-file-name
      (let* ((git-root (locate-dominating-file buffer-file-name ".git"))
             (path (if git-root
                       (file-relative-name buffer-file-name git-root)
                     (abbreviate-file-name buffer-file-name)))
             (modified (when (buffer-modified-p) "*"))
             (available-width (- (window-width) 30))
             (full (concat path modified)))
        (if (< (string-width full) available-width)
            (propertize full 'face 'mode-line-buffer-id)
          (let* ((parts (split-string path "/"))
                 (abbreviated (concat
                               (mapconcat
                                (lambda (p)
                                  (if (or (eq p (car (last parts)))
                                          (string-match-p "\\." p))
                                      p
                                    (substring p 0 1)))
                                parts "/")
                               modified)))
            (propertize abbreviated 'face 'mode-line-buffer-id))))
    (propertize (concat (buffer-name)
                        (when (buffer-modified-p) "*"))
                'face 'mode-line-buffer-id)))

;; Evil Modeline
(defface my-modeline-evil-normal
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for Evil normal state.")
(defface my-modeline-evil-insert
  '((t (:inherit success :weight bold)))
  "Face for Evil insert state.")
(defface my-modeline-evil-replace
  '((t (:inherit error :weight bold)))
  "Face for Evil replace state.")
(defface my-modeline-evil-visual
  '((t (:inherit warning :weight bold)))
  "Face for Evil visual state.")
(defface my-modeline-evil-motion
  '((t (:inherit custom-group-tag :weight bold)))
  "Face for Evil motion state.")
(defface my-modeline-evil-operator
  '((t (:weight bold)))
  "Face for Evil operator state.")

(defun my-modeline-evil-state()
  "Return the current Evil state for the custom modeline."
  (let* ((icon " ")
	 (state-name (upcase (symbol-name evil-state)))
	 (face (pcase evil-state
		 ('normal 'my-modeline-evil-normal)
		 ('insert 'my-modeline-evil-insert)
		 ('replace 'my-modeline-evil-replace)
		 ('visual 'my-modeline-evil-visual)
		 ('motion 'my-modeline-evil-motion)
		 ('operator 'my-modeline-evil-operator)
		 (_ 'mode-line))))
    (propertize (concat icon state-name) 'face face)))

;; Git
;;; Branch Name
(defface my-modeline-git-branch
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for Git branch name.")
(defun my-modeline-git-branch ()
  "Return the current Git branch or remote tracking branch for modeline."
  (when-let ((top (magit-toplevel)))
    (let* ((default-directory top)
           (branch (magit-get-current-branch)))
      (cond
       ;; Local branch — show just the name
       (branch
        (propertize (concat " " branch)
                    'face 'my-modeline-git-branch))

       ;; Detached HEAD — try to resolve remote branch
       (t
        (let ((ref (magit-git-string "symbolic-ref" "-q" "HEAD")))
          (if ref
              ;; Symbolic ref (like refs/remotes/origin/main)
              (let ((name (string-remove-prefix "refs/" ref)))
                (propertize (concat " " name)
                            'face 'my-modeline-git-branch))
            ;; Fully detached HEAD (commit hash)
            (let ((short (magit-git-string "rev-parse" "--short" "HEAD")))
              (propertize (format " HEAD → %s" short)
                          'face 'my-modeline-git-branch)))))))))
(setq auto-revert-check-vc-info t)

;; Clock
(defun my-modeline-clock()
  "Return the current time as a formatted string like 'Fri 27 Jun 00:27."
  (format-time-string "%a %d %b %H:%M"))

;; Org-Clock
(defun my-modeline-org-clock ()
  "Return the org-mode clock string if clocking and there's enough space."
  (when (and (fboundp 'org-clocking-p) (org-clocking-p)
	     (> (window-total-width) 90))
    (propertize (org-clock-get-clock-string)
		'face 'org-date)))

;; Major Mode
(defun my-modeline-major-mode ()
  "Return the name of the current major mode."
  (propertize (format-mode-line mode-name) 'face 'shadow))

(provide 'modeline)
