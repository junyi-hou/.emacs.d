;;; bc-pkgmgmt.el --- package management -*- lexical-binding: t; -*-


;;; Commentary:

;;; Code:

;; bootstrapping straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; do not litter my .emacs.d
(use-package no-littering
  :config
  (setq-default
   no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory)
   no-littering-var-directory (expand-file-name "var/" user-emacs-directory)
   auto-save-file-name-transforms  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
   backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup/")))
   custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror))
(use-package auto-compile
  :config (auto-compile-on-load-mode))

(provide 'bc-pkgmgmt)
;;; bc-pkgmgmt.el ends here
