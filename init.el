;;; init.el -- Emacs config
;;; Commentary:
;;  turning on or off a functionality by requiring different components

;;; Code:

;; set load path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; package loader and manager
(require 'bc-pkgmgmt)

(require 'bc-core)
(require 'bc-theme)

(require 'bc-evil)
(require 'bc-ivy)
(require 'bc-project)
(require 'bc-vcs)
(require 'bc-eshell)
(require 'bc-style)
(require 'bc-org)
(require 'bc-dired)

(require 'bc-ide-lisp)
(require 'bc-ide-python)
(require 'bc-ide-rust)
(require 'bc-ide-latex)
(require 'bc-ide-matlab)


;;; init.el ends here
