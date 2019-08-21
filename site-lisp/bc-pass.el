;;; bc-pass.el --- GNU pass emacs integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package password-store
  :defer t
  :init
  ;; use emacs minibuffer as pinentry frontend
  (defun pinentry-emacs (desc prompt ok error)
    "Use emacs as frontend of pinentry for gpg"
    (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
      str))

  (setq password-store-password-length 16)

  ;; when generate passwords, copy them automatically
  (advice-add
   #'password-store--run-generate
   :after
   (lambda (entry &rest args)
     (password-store-copy entry))))


(provide 'bc-pass)
;;; bc-pass.el ends here
