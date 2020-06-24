;;; init.el -- Emacs config
;;; Commentary:
;;  turning on or off a functionality by requiring different components

;;; Code:

;; set load path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require 'gatsby:core)

(require 'gatsby:tree-sitter)
(require 'gatsby:theme)
(require 'gatsby:evil)
(require 'gatsby:pass)
(require 'gatsby:exwm)

(require 'gatsby:eshell)
(require 'gatsby:vcs)
(require 'gatsby:selectrum)
(require 'gatsby:project)
(require 'gatsby:style)
(require 'gatsby:org)
(require 'gatsby:dired)
;; (require 'gatsby:mail)

;; ide features
(require 'gatsby:flymake)
(require 'gatsby:completion)
(require 'gatsby:lsp)

(require 'gatsby:pyim)

;;; language
(require 'gatsby:lisp)
(require 'gatsby:rust)
(require 'gatsby:julia)
(require 'gatsby:python)
(require 'gatsby:stata)
(require 'gatsby:text)

;;; init.el ends here
