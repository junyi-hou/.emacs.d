;;; gatsby:lisp.el --- setup ide features (auto-completion, code-jump, linter) for lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby:core)
(require 'gatsby:comint)

(use-package elisp-mode
  :straight (:type built-in)
  :init
  ;; fix tab width
  (defun gatsby:lisp--set-tab-width ()
    (setq-local tab-width 2))

  :hook
  (emacs-lisp-mode . gatsby:lisp--set-tab-width))

(use-package ielm
  :straight (:type built-in)
  :hook (ielm-mode . company-mode)
  :commands (gatsby:lisp-start-or-pop-to-repl)
  :init

  (defun gatsby:lisp-start-or-pop-to-repl ()
    "Pop to the associated REPL, if such REPL does not exist, start one."
    (interactive)
    (if gatsby:comint-repl-buffer
        (gatsby:comint--pop-to-repl)
      (gatsby:core-split-window)
      (other-window 1)
      (gatsby:comint--start-repl 'ielm)))

  (defun gatsby:lisp-associate-repl ()
    "Associate current buffer to a REPL"
    (interactive)
    (gatsby:comint-associate-repl 'inferior-emacs-lisp-mode))

  (defun gatsby:lisp-eval-sexp-or-region ()
    (interactive)
    (if (region-active-p)
        (gatsby:comint--eval-region 'ielm-return (region-beginning) (region-end))
      (gatsby:comint--eval-last-sexp 'ielm-return)))

  :general
  (:keymaps '(emacs-lisp-mode-map
              lisp-interaction-mode-map)
   :states '(motion normal visual)
   :prefix "SPC"
   "rr" 'gatsby:lisp-eval-sexp-or-region
   "rh" 'helpful-symbol
   "rz" 'gatsby:lisp-associate-repl
   "ro" 'gatsby:lisp-start-or-pop-to-repl)

  (:keymaps 'inferior-emacs-lisp-mode-map
   :states '(normal motion visual)
   "SPC" 'nil)

  (:keymaps 'inferior-emacs-lisp-mode-map
   :states 'insert
   "<return>" 'ielm-return)

  (:keymaps 'inferior-emacs-lisp-mode-map
   :states '(normal motion visual)
   :prefix "SPC"
   "q" 'gatsby:comint-exit-repl))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package helpful
  :init
  (evil-set-initial-state 'helpful-mode 'motion))

(use-package easy-escape
  :hook ((emacs-lisp-mode lisp-mode ielm-mode) . easy-escape-minor-mode))

(provide 'gatsby:lisp)
;;; gatsby:lisp.el ends here