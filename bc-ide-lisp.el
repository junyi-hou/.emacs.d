;;; bc-ide-lisp.el -- setup ide features (auto-completion, code-jump, linter) for lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-lsp)

;; elisp does not have a lsp
;; only thing, for now, that I miss is jumping to other places
(use-package dumb-jump
  :general
  (:keymaps 'emacs-lisp-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "jd" 'dumb-jump-jump))

(eval-after-load 'dash '(dash-enable-font-lock)) ; syntax highlight for dash

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq tab-width 2)
                                  (company-mode 1)
                                  (flymake-mode 1)))

(provide 'bc-ide-lisp)
;;; bc-ide-lisp.el ends here
