;;; bc-mail.el --- email client based on mu4e -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package notmuch
  :ensure nil
  :defer t
  ;; notmuch-poll is not autoloaded
  :commands notmuch-poll
  :init
  (setenv "NOTMUCH_CONFIG" (no-littering-expand-etc-file-name "notmuch.conf"))

  ;; sending mails
  (setq auth-sources `(,(concat
                         (no-littering-expand-var-file-name "maildir/authinfo.gpg"))))

  (setq message-default-headers "Cc: \nBcc: \n"
        message-kill-buffer-on-exit t)

  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        message-send-mail-function 'message-smtpmail-send-it)

  (dolist (mode '(notmuch-hello-mode notmuch-search-mode notmuch-show-mode notmuch-tree-mode))
    (evil-set-initial-state mode 'motion))

  (defun bc-mail-prompt-for-sender ()
    "Prompt for a sender address, using `ivy-read'."
    (let* ((name (notmuch-user-name))
           (addrs (cons (notmuch-user-primary-email)
                        (notmuch-user-other-email)))
           (address
            (ivy-read
             (concat "Sender address for " name ": ")
             addrs
             :action 'identity)))
      (message-make-from name address)))

  (defalias 'notmuch-mua-prompt-for-sender #'bc-mail-prompt-for-sender)

  (defun bc-mail-update-and-search (query)
    "Poll from the server and start searching QUERY"
    (interactive "sSearching Mail: ")
    (notmuch-poll)
    (notmuch-search query))

  (defun bc-mail-flag ()
    "Flagged selected threads."
    (interactive)
    (notmuch-search-tag '("+flagged")))

  (defun bc-mail-unflag ()
    "Unflagged selected threads."
    (interactive)
    (notmuch-search-tag '("-flagged")))

  (defun bc-mail-update-and-new ()
    "Open unread email list."
    (interactive)
    (bc-mail-update-and-search "tag:unread and tag:inbox"))

  (defun bc-mail-update-and-open-inbox ()
    "Open inbox"
    (interactive)
    (bc-mail-update-and-search "tag:inbox"))

  (defun bc-mail-sync ()
    "Syncing the maildir."
    (interactive)
    (message "%s" (shell-command-to-string "syncmail.sh")))

  (defun bc-mail-seminar-list ()
    "Search for seminar information"
    (interactive)
    (bc-mail-update-and-search "tag:seminar"))

  :general
  (:keymaps '(motion normal visual emacs insert)
  :prefix "SPC"
  :non-normal-prefix "s-SPC"
  "ms" 'bc-mail-update-and-search
  "mi" 'bc-mail-update-and-open-inbox
  "mu" 'bc-mail-update-and-new
  "mt" 'bc-mail-seminar-list
  "mn" (lambda () (interactive) (notmuch-mua-new-mail t)))

  (:keymaps 'notmuch-hello-mode-map
   :states 'motion
   "q" 'notmuch-bury-or-kill-this-buffer
   "s" 'notmuch-search)

  (:keymaps 'notmuch-search-mode-map
   :states '(motion visual)
   "q" 'notmuch-bury-or-kill-this-buffer
   "s" 'notmuch-search
   "S" 'notmuch-search-filter

   "a" 'notmuch-search-archive-thread
   "A" (lambda () (interactive)
         (notmuch-search-archive-thread 'unarchive))
   "f" 'bc-mail-flag
   "F" 'bc-mail-unflag

   "-" 'notmuch-search-remove-tag
   "+" 'notmuch-search-add-tag
   "RET" 'notmuch-search-show-thread
   "t" (lambda () (interactive)
         (notmuch-search-show-thread)
         (notmuch-tree-from-show-current-query))
   "T" 'notmuch-tree-from-search-current-query)

  (:keymaps 'notmuch-show-mode-map
   :states 'motion
   "q" 'notmuch-bury-or-kill-this-buffer
   "s" 'notmuch-search
   "t" 'notmuch-tree-from-show-current-query
   "gr" 'notmuch-show-refresh-view
   "<tab>" 'notmuch-show-next-button
   "<backtab>" 'notmuch-show-previous-button

   "f" 'notmuch-show-forward-message
   "F" 'notmuch-show-forward-open-messages
   "r" 'notmuch-show-reply-sender
   "R" 'notmuch-show-reply

   "C-y" 'notmuch-show-previous-message
   "C-e" 'notmuch-show-next-message
   "A" 'notmuch-show-archive-thread-then-next
   "a" 'notmuch-show-archive-message-then-next-or-next-thread

   "M-j" 'notmuch-show-advance
   "M-k" 'notmuch-show-rewind)

  (:keymaps 'notmuch-tree-mode-map
   :states 'motion
   "q" 'notmuch-tree-quit
   "s" 'notmuch-search
   "M-j" 'notmuch-tree-next-message
   "M-k" 'notmuch-tree-prev-message
   "S" 'notmuch-search-from-tree-current-query
   "RET" 'notmuch-tree-show-message
   "a" 'notmuch-tree-archive-message-then-next
   "A" 'notmuch-tree-archive-thread)

  ;; fool-proving
  (:keymaps '(notmuch-search-mode-map
             notmuch-show-mode-map
             notmuch-hello-mode-map)
   :states '(motion visual)
   :prefix "SPC"
   "q" 'notmuch-bury-or-kill-this-buffer
   "r" (lambda () (interactive)
         (bc-mail-sync)
         (notmuch-poll-and-refresh-this-buffer)))

  (:keymaps 'notmuch-tree-mode-map
   :states '(motion visual)
   :prefix "SPC"
   "q" 'notmuch-tree-quit
   "r" (lambda () (interactive)
         (bc-mail-sync)
         (notmuch-poll-and-refresh-this-buffer))))

(use-package org-notmuch
  ;; require to emerge `app-emacs/org-mode' with `contrib' flag
  :straight (org-notmuch :type built-in)
  :after notmuch)

(provide 'bc-mail)
;;; bc-mail.el ends here
