(use-package pdf-tools
  :defer t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  :bind
  (:map pdf-view-mode-map)
  ("l"  . image-forward-hscroll)
  ("h"  . image-backward-hscroll)
  ("j" . pdf-view-next-page)
  ("k" . pdf-view-previous-page))
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

(use-package org
  :ensure t)

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
   (shell . t)))
(setq org-babel-python-command "python3")


(provide 'note-taking)
