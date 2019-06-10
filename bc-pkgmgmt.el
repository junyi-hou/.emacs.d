;;; bc-pkgmgmt.el -- package management -*- lexical-binding: t; -*-


;;; Commentary:

;;; Code:

;; package manager
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

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
(setq use-package-always-ensure t)
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
