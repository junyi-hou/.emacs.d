;;; bc-pass.el --- GNU pass emacs integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package password-store
  :commands password-store--run-generate
  :init
  ;; use emacs minibuffer as pinentry frontend
  (defun pinentry-emacs (desc prompt ok error)
    "Use emacs as frontend of pinentry for gpg"
    (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
      str))

  (setq password-store-password-length 16)

  ;; better password-store-generate -- warn if overriding
  (defun bc-password-store-generate (entry &optional password-length)
    "Generate a new password for ENTRY with PASSWORD-LENGTH, checking for duplicates first."
    (interactive (list (read-string "Password entry: ")))
    (let ((password-length (or password-length password-store-password-length)))
      ;; (message (concat "~/.password-store/" entry ".gpg"))
      (when (file-exists-p (concat "~/.password-store/" entry ".gpg"))
        (unless (y-or-n-p (concat entry "already exists, override?"))
          (user-error "Aborting")))
      (password-store--run-generate entry password-length t)))

  ;; when generate passwords, copy them automatically
  (advice-add
   #'password-store--run-generate
   :after
   (lambda (entry &rest args)
     (password-store-copy entry)))

  (defun bc-pass--get-entry (entry type)
    "Retrieve secret TYPE from ENTRY."
    (password-store-get (concat entry "/" type)))

  (defun bc-pass-qutebroswer ()
    "A qutebroswer userscript to retrieve secrets from gnu pass."
    (let* ((candidates (seq-filter
                        (lambda (folder-name)
                          (not
                           (string=
                            "."
                            (substring-no-properties folder-name 0 1))))
                        (directory-files "~/.password-store/")))
           (entry (ivy-read "account: " candidates :action 'identity)))
       (concat (bc-pass--get-entry entry "account")
               " "
               (bc-pass--get-entry entry "passwd"))))
  )

(provide 'bc-pass)
;;; bc-pass.el ends here
