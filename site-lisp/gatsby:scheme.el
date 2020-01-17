;;; gatsby:scheme.el --- IDE feature for GNU guile -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby:core)
(require 'gatsby:comint)

(use-package aggressive-indent
  :hook
  (scheme-mode . aggressive-indent-mode))

(use-package geiser
  ;; provide code-completion and documentation
  :hook (scheme-mode . geiser-mode)
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

(provide 'gatsby:scheme)
;;; gatsby:scheme.el ends here
