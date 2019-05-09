;;; bc-ide-lisp.el -- setup ide features (auto-completion, code-jump, linter) for lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-lsp)

;; elisp does not have a lsp
;; only thing, for now, whatt I miss is jumping to definitions
(use-package dumb-jump
  :defer t
  :config
  (setq dumb-jump-selector 'ivy
        dumb-jump-prefer-searcher 'ag)
  :general
  (:keymaps 'emacs-lisp-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "jd" 'dumb-jump-go))

(eval-after-load 'dash '(dash-enable-font-lock)) ; syntax highlight for dash

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq tab-width 2)
                                  (company-mode 1)
                                  (dumb-jump-mode 1)))

(provide 'bc-ide-lisp)
;;; bc-ide-lisp.el ends here
