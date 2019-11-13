;;; bc-ide-lisp.el --- setup ide features (auto-completion, code-jump, linter) for lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package elisp-mode
  :straight (:type built-in)
  :init
  ;; fix tab width
  (defun bc-lisp--set-tab-width ()
    (setq-local tab-width 2))

  :hook
  (emacs-lisp-mode . bc-lisp--set-tab-width))

(use-package helpful
  :init
  (evil-set-initial-state 'helpful-mode 'motion)
  :general
  (:keymaps 'emacs-lisp-mode-map
   :states '(visual normal motion)
   :prefix "SPC"
   "rh" 'helpful-at-point
   "hf" 'helpful-callable
   "hv" 'helpful-variable))

(provide 'bc-ide-lisp)
;;; bc-ide-lisp.el ends here
