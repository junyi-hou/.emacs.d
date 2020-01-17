;;; init.el -- Emacs config
;;; Commentary:
;;  turning on or off a functionality by requiring different components

;;; Code:

;; set load path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require 'bc-core)

(require 'bc-remote)
(require 'bc-theme)
(require 'bc-evil)
(require 'bc-pass)
(require 'bc-exwm)
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
(require 'bc-lisp)
(require 'bc-gentoo)
(require 'bc-python)
(require 'bc-stata)
(require 'bc-scheme)
(require 'bc-text)

;;; init.el ends here
