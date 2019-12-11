;;; bc-pass.el --- GNU pass emacs integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package epa
  :init
  (setq epg-pinentry-mode 'loopback
        auth-sources `(,(concat no-littering-var-directory "authinfo.gpg"))
        password-cache t
        password-cache-expiry 1800))

(use-package password-store
  :defer t
  :init
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

  :general
  (:keymaps '(normal visual motion emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "pc" 'password-store-copy
   "pg" 'bc-password-store-generate))

(provide 'bc-pass)
;;; bc-pass.el ends here
