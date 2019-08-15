;;; bc-ide-lisp.el --- setup ide features (auto-completion, code-jump, linter) for lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local tab-width 2)))

(provide 'bc-ide-lisp)
;;; bc-ide-lisp.el ends here
