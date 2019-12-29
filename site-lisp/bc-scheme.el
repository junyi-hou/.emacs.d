;;; bc-scheme.el --- IDE feature for GNU guile -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package geiser
  ;; provide code-completion and documentation
  :hook (scheme-mode . geiser-mode)
  :init
  (require 'bc-comint)
  ;; use guile
  (setq scheme-program-name "guile"
        geiser-default-implementation 'guile
        geiser-repl-mode-map (make-sparse-keymap))

  (setq-local tab-width 2)

  ;;; ===============================
  ;;  REPL settings
  ;;; ===============================

  (defun bc-scheme-start-or-pop-to-repl ()
    "Pop to `bc-scheme-repl-buffer'.  If `bc-scheme-repl-buffer' is nil, start a new repl."
    (interactive)
    (if bc-comint-repl-buffer
        (bc-comint--pop-to-repl)
      (bc-comint--start-repl 'geiser-run geiser-default-implementation)))

  (defun bc-scheme-associate-repl ()
    "Associate current buffer to a REPL"
    (interactive)
    (bc-comint-associate-repl 'geiser-repl-mode))

  (defun bc-scheme-eval-sexp-or-region ()
    (interactive)
    (if (region-active-p)
        (bc-comint--eval-region 'geiser-repl--maybe-send (region-beginning) (region-end))
      (bc-comint--eval-last-sexp 'geiser-repl--maybe-send)))

  :general

  (:keymaps 'geiser-repl-mode-map
   :states 'insert
   "<return>" 'geiser-repl--maybe-send)

  (:keymaps 'scheme-mode-map
   :states '(motion normal visual)
   :prefix "SPC"
   "rr" 'bc-scheme-eval-sexp-or-region
   "rh" 'geiser-doc-symbol-at-point
   "rz" 'bc-comint-associate-repl
   "ro" 'bc-scheme-start-or-pop-to-repl))

(provide 'bc-scheme)
;;; bc-scheme.el ends here
