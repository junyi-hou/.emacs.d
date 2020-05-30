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

  (defconst gatsby:lisp-hs-block-start-keywords
    (rx (* space) "(" (or
                       ;; definitions should always get folded
                       "defun" "defmacro" "defcustom" "defconst" "defvar" "defvar-local"
                       ;; I also want to fold :keymaps in my use-pacakge definitions
                       ":keymaps" "evil-define-motion"))
    "Regexp of forms that should be folded by `hs-minor-mode'.
All forms that start at the `beginning-of-line' will be folded. Other forms should be folded only when it matches these keywords.")

  (defun gatsby:lisp--setup-hs-block-start-regexp ()
    "Fold only definitions in elisp mode"
    (setq hs-block-start-regexp
          (concat "^\\((\\|" gatsby:lisp-hs-block-start-keywords "\\)")))

  :hook
  (emacs-lisp-mode . gatsby:lisp--set-tab-width)
  (emacs-lisp-mode . gatsby:lisp--setup-hs-block-start-regexp))

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
  (emacs-lisp-mode . aggressive-indent-mode)
  (scheme-mode . aggressive-indent-mode))

(use-package helpful
  :init
  (evil-set-initial-state 'helpful-mode 'motion))

(use-package easy-escape
  :hook ((emacs-lisp-mode lisp-mode ielm-mode) . easy-escape-minor-mode))

(use-package geiser
  ;; provide code-completion and documentation
  :hook
  (scheme-mode . geiser-mode)
  :init
  (require 'gatsby:comint)
  ;; use guile
  (setq scheme-program-name "guile"
        geiser-default-implementation 'guile
        geiser-repl-mode-map (make-sparse-keymap))

  (setq-local tab-width 2)

  ;;; ===============================
  ;;  REPL settings
  ;;; ===============================

  (defun gatsby:scheme-start-or-pop-to-repl ()
    "Pop to `gatsby:scheme-repl-buffer'.  If `gatsby:scheme-repl-buffer' is nil, start a new repl."
    (interactive)
    (if gatsby:comint-repl-buffer
        (gatsby:comint--pop-to-repl)
      (gatsby:comint--start-repl 'run-geiser geiser-default-implementation)))

  (defun gatsby:scheme-associate-repl ()
    "Associate current buffer to a REPL"
    (interactive)
    (gatsby:comint-associate-repl 'geiser-repl-mode))

  (defun gatsby:scheme-eval-sexp-or-region ()
    (interactive)
    (if (region-active-p)
        (gatsby:comint--eval-region 'geiser-repl--maybe-send (region-beginning) (region-end))
      (gatsby:comint--eval-last-sexp 'geiser-repl--maybe-send)))

  :general

  (:keymaps 'geiser-repl-mode-map
   :states '(normal motion visual)
   "SPC" 'nil)

  (:keymaps 'geiser-repl-mode-map
   :states 'insert
   "<return>" 'geiser-repl--maybe-send)

  (:keymaps 'geiser-repl-mode-map
   :states '(normal motion visual)
   :prefix "SPC"
   "q" 'gatsby:comint-exit-repl)

  (:keymaps 'scheme-mode-map
   :states '(motion normal visual)
   :prefix "SPC"
   "rr" 'gatsby:scheme-eval-sexp-or-region
   "rh" 'geiser-doc-symbol-at-point
   "rz" 'gatsby:scheme-associate-repl
   "ro" 'gatsby:scheme-start-or-pop-to-repl))

(provide 'gatsby:lisp)
;;; gatsby:lisp.el ends here
