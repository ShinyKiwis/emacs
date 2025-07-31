;; Org and Note-taking configuration
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

(use-package org
  :ensure t)

(use-package org-agenda
  :ensure nil)

(use-package org-modern
  :ensure t)
(with-eval-after-load 'org (global-org-modern-mode))
(modify-all-frames-parameters
 '((right-divider-width . 10)
   (internal-border-width . 10)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;; Mermaid Support
(use-package ob-mermaid
  :ensure t)
(setq org-image-actual-width nil)

(setq
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellipsis "…")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   (python . t)
   (emacs-lisp . t)
   (mermaid . t)
   (shell . t)))
(setq org-babel-python-command "python3")

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; Setup for org agenda
(setq org-agenda-files
    (append
	(list "~/Documents/org/inbox.org"
	    "~/Documents/org/tasks.org"
	    "~/Documents/org/calendar.org")
	(directory-files-recursively "~/Documents/org/projects/" "\\.org$")))
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "CANCELED" "BLOCKED")))

(use-package org-super-agenda
  :hook (after-init . org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
	'((:auto-category t)))
  (setq org-super-agenda-keep-order nil)
  (org-super-agenda-mode))

(setq org-agenda-custom-commands
      '(("u" "Super view"
         ((agenda ""
                  ((org-super-agenda-groups
                    '((:name "Today"
                             :time-grid t)))))
          (alltodo ""
                   ((org-agenda-overriding-header "Projects")
                    (org-super-agenda-groups
                     '((:auto-category t)
                       (:discard (:anything t))))))))
        ("d" "Daily Agenda"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-overriding-header "Daily Agenda")))))))
(setq org-agenda-prefix-format
      '((agenda . "  %c%-4t% s")
        (todo   . "  ")
        (tags   . "  ")
        (search . "  ")))
(setq org-archive-location "archive/%s_archive::")

;; Enable logging into LOGBOOK drawer instead of task body
(setq org-log-into-drawer t)
(setq org-log-done 'time)

(use-package org-web-tools
  :ensure t
  :bind (("C-c w" . org-web-tools-insert-link-for-url)
         ("C-c W" . org-web-tools-read-url-as-org)))

(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;; Support for SQLi scroll down
(add-hook 'sql-interactive-mode-hook
  (lambda ()
    (setq-local comint-move-point-for-output t)
    (setq-local comint-scroll-to-bottom-on-output t)
    (setq-local comint-scroll-to-bottom-on-input t)))

(provide 'plugins/note-taking)
