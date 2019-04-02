;;; ide-lisp.el -- setup ide features (auto-completion, code-jump, linter) for lisp

;;; Commentary:

;;; Code:

(require 'dev-autocomplete)
(require 'dev-linter)
(require 'dev-jump)

;; load stuffs
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (company-mode 1)
                                  (dumb-jump-mode 1)
                                  (flycheck-mode 1)))

(provide 'ide-lisp)
;;; ide-lisp.el ends here
