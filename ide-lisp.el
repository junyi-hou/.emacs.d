;;; ide-lisp.el -- setup ide features (auto-completion, code-jump, linter) for lisp

(require 'dev-autocomplete)
(require 'dev-linter)
(require 'dev-jump)

;; load stuffs
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'dumb-jump-mode)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

(provide 'ide-lisp)
;;; ide-lisp.el ends here
