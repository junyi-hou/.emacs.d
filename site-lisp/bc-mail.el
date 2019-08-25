;;; bc-mail.el --- email client based on mu4e -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package mu4e
  :defer t
  :ensure nil
  :init
  ;; receiving
  (setq mu4e-maildir "~/mail"
        mu4e-sent-folder nil
        mu4e-drafts-folder nil
        mu4e-sent-folder nil
        mu4e-sent-folder nil
        mu4e-get-mail-command "mbsync -c ~/.emacs.d/etc/mbsyncrc -a"
        mu4e-html2text-command nil
        mu4e-update-interval 120
        mu4e-headers-auto-update t
        mu4e-compose-signature-auto-include nil)

  ;; sending
  
  ;; composing
  )

(provide 'bc-mail)
;;; bc-mail.el ends here
