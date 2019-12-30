;;; bc-lisp.el --- setup ide features (auto-completion, code-jump, linter) for lisp -*- lexical-binding: t; -*-

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

(use-package ielm
  :straight (:type built-in)
  :hook (ielm-mode . company-mode)
  :commands (bc-lisp-start-or-pop-to-repl)
  :init

  (defun bc-lisp-start-or-pop-to-repl ()
    "Pop to the associated REPL, if such REPL does not exist, start one."
    (interactive)
    (if bc-comint-repl-buffer
        (bc-comint--pop-to-repl)
      (bc-core-split-window)
      (other-window 1)
      (bc-comint--start-repl 'ielm)))

  (defun bc-lisp-associate-repl ()
    "Associate current buffer to a REPL"
    (interactive)
    (bc-comint-associate-repl 'inferior-emacs-lisp-mode))

  (defun bc-lisp-eval-sexp-or-region ()
    (interactive)
    (if (region-active-p)
        (bc-comint--eval-region 'ielm-return (region-beginning) (region-end))
      (bc-comint--eval-last-sexp 'ielm-return)))

  :general

  (:keymaps '(emacs-lisp-mode-map
              lisp-interaction-mode-map)
   :states '(motion normal visual)
   :prefix "SPC"
   "rr" 'bc-lisp-eval-sexp-or-region
   "rh" 'helpful-symbol
   "rz" 'bc-comint-associate-repl
   "ro" 'bc-lisp-start-or-pop-to-repl)

  (:keymaps 'inferior-emacs-lisp-mode-map
   :states '(normal motion visual)
   "SPC" 'nil)

  (:keymaps 'inferior-emacs-lisp-mode-map
   :states 'insert
   "<return>" 'ielm-return)

  (:keymaps 'inferior-emacs-lisp-mode-map
   :states '(normal motion visual)
   :prefix "SPC"
   "q" 'bc-comint-exit-repl))

(use-package aggressive-indent
:hook
(emacs-lisp-mode . aggressive-indent-mode))

(use-package helpful
  :init
  (evil-set-initial-state 'helpful-mode 'motion))

(use-package easy-escape
  :hook ((emacs-lisp-mode lisp-mode) . easy-escape-minor-mode))

(provide 'bc-lisp)
;;; bc-lisp.el ends here
