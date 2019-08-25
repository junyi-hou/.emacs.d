;;; bc-pkgmgmt.el --- package management -*- lexical-binding: t; -*-


;;; Commentary:

;;; Code:

;; package manager
(require 'package)

(setq-default package-archives
              '(("org" . "http://orgmode.org/elpa/")
                ("melpa" . "http://melpa.org/packages/")
                ("elpy" . "https://jorgenschaefer.github.io/packages/")
                ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

;; package loader
(unless (package-installed-p 'quelpa-use-package)
  (package-refresh-contents)
  (package-install 'quelpa-use-package))

(eval-when-compile
  (require 'use-package)
  (require 'quelpa-use-package))

;; use package settings
(setq use-package-always-ensure t
      quelpa-update-melpa-p nil)

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
(use-package auto-package-update
  :config
  (setq auto-package-update-prompt-before-update t
        auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(setq load-prefer-newer t)

(provide 'bc-pkgmgmt)
;;; bc-pkgmgmt.el ends here
