;;; init.el -- Emacs config
;;; Commentary:
;;  turning on or off a functionality by requiring different components

;;; Code:

;; set load path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; fix potential credential issues
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; package loader and manager
(require 'bc-pkgmgmt)

(require 'bc-core)
(require 'bc-remote)
(require 'bc-theme)
(require 'bc-evil)
(require 'bc-pass)
(require 'bc-os)
(require 'bc-config)

(require 'bc-eshell)
(require 'bc-vcs)
(require 'bc-ivy)
(require 'bc-project)
(require 'bc-style)
(require 'bc-org)
(require 'bc-dired)
;; (require 'bc-mail)

;; ide features
(require 'bc-flymake)
(require 'bc-company)
(require 'bc-lsp)

(require 'bc-pyim)

;;; language
(require 'bc-ide-lisp)
(require 'bc-ide-gentoo)
(require 'bc-ide-python)
(require 'bc-ide-stata)
;; (require 'bc-ide-guile)
(require 'bc-text)
;; (require 'bc-ide-matlab)

;;; init.el ends here
