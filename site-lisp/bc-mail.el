;;; bc-mail.el --- email client based on mu4e -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package mu4e
  :defer t
  :ensure nil
  :init

  ;; context - different mail accounts
  (setq mu4e-contexts
        `(
          ,(make-mu4e-context
            :name "Gmail"
            :enter-func (lambda () (mu4e-message "Entering Gmail..."))
            :match-func ;; use maildir to match
            (lambda (msg)
              (when msg
                (string-match-p
                 "~/mail/gmail"
                 (mu4e-message-contact-field-matches msg :maildir))))
            :vars '((user-mail-address . "junyi.yi.hou@gmail.com")
                    (user-full-name . "Junyi Hou")
                    (mu4e-compose-signature . nil)
                    (mu4e-sent-folder . "/gmail/sent")
                    (mu4e-drafts-folder . "/gmail/drafts")
                    (mu4e-refile-folder . "/gmail/all")
                    (mu4e-trash-folder . "/gmail/bin")))

          ,(make-mu4e-context
            :name "Berkeley"
            :enter-func (lambda () (mu4e-message "Entering Gmail..."))
            :match-func ;; use maildir to match
            (lambda (msg)
              (when msg
                (string-match-p
                 "~/mail/berkeley"
                 (mu4e-message-contact-field-matches msg :maildir))))
            :vars '((user-mail-address . "junyi.hou@berkeley.edu")
                    (user-full-name . "Junyi Hou")
                    (mu4e-compose-signature . nil)))
          ))

  (setq mu4e-context-policy 'ask
        mu4e-get-mail-command "mbsync -c ~/.emacs.d/etc/mbsyncrc gmail")

  ;; sending
  ;; don't save message to Sent Messages, IMAP takes care of this
  ;; (setq mu4e-sent-messages-behavior 'delete)

  (setq mu4e-maildir "~/mail"
        mu4e-show-images t
        mu4e-view-prefer-html t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  )

(provide 'bc-mail)
;;; bc-mail.el ends here
