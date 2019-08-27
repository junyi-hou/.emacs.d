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
            :leave-func (lambda () (mu4e-message "Leaving Gmail..."))
            :match-func
            (lambda (msg)
              (when msg
                (mu4e-message-contact-field-matches
                 msg
                 '(:to :from :cc :bcc)
                 "junyi.yi.hou@gmail.com")))
            :vars '((user-mail-address . "junyi.yi.hou@gmail.com")
                    (user-full-name . "Junyi Hou")
                    (mu4e-compose-signature . nil)
                    (mu4e-sent-folder . "/gmail/sent")
                    (mu4e-drafts-folder . "/gmail/drafts")
                    (mu4e-refile-folder . "/gmail/all")
                    (mu4e-trash-folder . "/gmail/bin")))

          ,(make-mu4e-context
            :name "Berkeley"
            :enter-func (lambda () (mu4e-message "Entering bmail..."))
            :leave-func (lambda () (mu4e-message "Leaving bmail..."))
            :match-func
            (lambda (msg)
              (when msg
                (mu4e-message-contact-field-matches
                 msg
                 '(:to :from :cc :bcc)
                 "junyi.hou@berkeley.edu")))
            :vars '((user-mail-address . "junyi.hou@berkeley.edu")
                    (user-full-name . "Junyi Hou")
                    (mu4e-compose-signature . nil)
                    (mu4e-sent-folder . "/berkeley/sent")
                    (mu4e-drafts-folder . "/berkeley/drafts")
                    (mu4e-refile-folder . "/berkeley/all")
                    (mu4e-trash-folder . "/berkeley/trash")))
          ))

  ;; general settings
  (setq mu4e-maildir "~/.emacs.d/var/emails"
        mu4e-attachment-dir "~/downloads/"
        mu4e-update-interval 300
        mu4e-hide-index-messages t
        mu4e-get-mail-command "mbsync -c ~/.emacs.d/etc/mbsyncrc -a"

        ;; display
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        mu4e-view-prefer-html nil
        mu4e-use-fancy-chars t
        mu4e-headers-auto-update t
        mu4e-change-filenames-when-moving t
        mu4e-compose-in-new-frame nil
        mu4e-headers-fields '((:human-date . 8)
                              (:from . 20)
                              (:subject . nil)))
  ;; use imagemagick
    (imagemagick-register-types)

  ;; evil-related settings
  (dolist (mode '(mu4e-main-mode mu4e-headers-mode mu4e-view-mode))
    (evil-set-initial-state mode 'motion))

  :general
  (:keymaps '(mu4e-main-mode-map mu4e-headers-mode-map mu4e-view-mode-map)
   :states 'motion
   ";" 'mu4e-context-switch
   "U" 'mu4e-update-mail-and-index
   "SPC" nil
   "" nil)

  (:keymaps '(mu4e-main-mode-map mu4e-headers-mode-map mu4e-view-mode-map)
   :states 'motion
   :prefix "SPC"
   "j" 'mu4e~headers-jump-to-maildir
   "q" 'mu4e-quit
   "s" 'mu4e-headers-search
   "S" 'mu4e-headers-search-bookmark
   ";" 'mu4e-context-switch
   "U" 'mu4e-update-mail-and-index)

  (:keymaps 'mu4e-headers-mode-map
   :states 'motion
   "j" 'mu4e-headers-next
   "k" 'mu4e-headers-prev
   "J" (lambda () (interactive) (mu4e-headers-next 3))
   "K" (lambda () (interactive) (mu4e-headers-prev 3))
   "C-e" 'mu4e-headers-next-unread
   "C-y" 'mu4e-headers-prev-unread
   "RET" 'mu4e-headers-view-message
   "d" 'mu4e-headers-mark-for-delete
   "u" 'mu4e-headers-mark-for-unread
   "a" 'mu4e-headers-mark-for-refile
   "X" 'mu4e-mark-execute-all
   "r" 'revert-buffer)

  (:keymaps 'mu4e-view-mode-map
   :states 'motion
   "q" 'kill-buffer-and-window
   "d" 'mu4e-view-mark-for-delete
   "u" 'mu4e-view-mark-for-unread
   "a" 'mu4e-view-mark-for-refile
   "o" 'mu4e-view-open-attachment-emacs))

;; (use-package mu4e-alert
;;   :hook
;;   (after-init . #'mu4e-alert-enable-mode-line-display)
;;   :init
;;   (setq mu4e-alert-interesting-mail-query
;;         (concat
;;          "flag:unread"
;;          " AND NOT maildir:"
;;          "\"/all\"")))

(provide 'bc-mail)
;;; bc-mail.el ends here
