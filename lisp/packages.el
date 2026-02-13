;; Packages configuration
(require 'package)

;; Set package sources
(setq package-archives '( ("org" . "https://orgmode.org/elpa/")
                          ("gnu" . "https://elpa.gnu.org/packages/")
                          ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Load $PATH for emacs
(use-package exec-path-from-shell
	     :ensure t)

(when (or (memq window-system '(mac ns x))
          (daemonp))
  (exec-path-from-shell-initialize))

(provide 'packages)
