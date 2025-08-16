;; Setup Password Store for pass
(use-package password-store
  :ensure t)
(use-package auth-source-pass
    :config  (auth-source-pass-enable))
(setq auth-source-debug t)

;; Manually compile mu on debian
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(use-package mu4e
  :ensure nil
  :defer 20
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)
  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir-list '("~/Mail"))
  (setq mu4e-show-images t)
  (setq mu4e-compose-context-policy 'ask-if-none)
  (setq mu4e-compose-reply-recipients 'ask)
  (setq mu4e-compose-reply-ignore-address '("no-?reply"))
  ;; Configure the function to use for sending email
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; Setup multiple accounts
  (setq mu4e-contexts
        (list
         ;; Personal account
         (make-mu4e-context
          :name "Personal"
          :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "nguyenhua.hcm@gmail.com")
                  (user-full-name    . "Nguyen Hua Hoang")
		  (smtpmail-smtp-user . "nguyenhua.hcm@gmail.com")
		  (smtpmail-smtp-server . "smtp.gmail.com")
		  (smtpmail-smtp-service . 465)
		  (smtpmail-stream-type . ssl)
                  (mu4e-drafts-folder  . "/gmail_nguyenhua/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/gmail_nguyenhua/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/gmail_nguyenhua/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/gmail_nguyenhua/[Gmail]/Trash")))

	 ;; Personal Deploy account
         (make-mu4e-context
          :name "Deploy"
          :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "nguyenhua.deploy@gmail.com")
                  (user-full-name    . "Nguyen Hua Hoang")
		  (smtpmail-smtp-user . "nguyenhua.deploy@gmail.com")
		  (smtpmail-smtp-server . "smtp.gmail.com")
		  (smtpmail-smtp-service . 465)
		  (smtpmail-stream-type . ssl)
                  (mu4e-drafts-folder  . "/gmail_nguyenhua_deploy/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/gmail_nguyenhua_deploy/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/gmail_nguyenhua_deploy/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/gmail_nguyenhua_deploy/[Gmail]/Trash")))))

  ;; Run mu4e in the background to sync mail periodically
  (mu4e t))

(provide 'plugins/workflow)
