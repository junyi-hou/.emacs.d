;;; bc-pass.el --- GNU pass emacs integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package password-store
  :defer t
  :init
  ;; for CLI pinentry
  (defun pinentry-emacs (desc prompt ok error)
    "Use emacs as frontend of pinentry for gpg"
    (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
      str))

  (start-process-shell-command "gpg-agent" nil "gpg-agent --deamon")

  )

(provide 'bc-pass)
;;; bc-pass.el ends here
