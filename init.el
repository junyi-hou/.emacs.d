;;; init.el -- Emacs config
;;; Commentary:
;;  turning on or off a functionality by requiring different components

;;; Code:

;; temp fix bug 34341
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; set load path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; package loader and manager
(require 'bc-pkgmgmt)

(require 'bc-core)
(require 'bc-exwm)
(require 'bc-theme)

(require 'bc-evil)
(require 'bc-vcs)
(require 'bc-ivy)
(require 'bc-eshell)
(require 'bc-project)
(require 'bc-style)
(require 'bc-org)
(require 'bc-dired)

(require 'bc-ide-lisp)
(require 'bc-ide-gentoo)
;; (require 'bc-ide-python)
;; (require 'bc-ide-rust)
;; (require 'bc-ide-latex)
;; (require 'bc-ide-matlab)

;;; init.el ends here
