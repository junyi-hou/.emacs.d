;;; bc-ide-lisp.el -- setup ide features (auto-completion, code-jump, linter) for lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-lsp)

(eval-after-load 'dash '(dash-enable-font-lock)) ; syntax highlight for dash

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq tab-width 2)
                                  (company-mode 1)
                                  (flymake-mode 1)
                                  (dumb-jump-mode 1)))

(provide 'bc-ide-lisp)
;;; bc-ide-lisp.el ends here
