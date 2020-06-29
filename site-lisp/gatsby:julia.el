;;; gatsby:julia.el --- julia mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package julia-mode
  :defer t
  :init

  (defconst gatsby:julia-version "1.4")

  (defun gatsby:julia-repl ()
    "Start or switch to a local julia repl"
    (interactive)
    (gatsby:jupyter-start-or-switch-to-repl (concat "julia" "-" gatsby:julia-version)))

  (defun gatsby:julia-eval-region-or-line ()
    "If region is active, eval current region, otherwise eval current line."
    (interactive)
    (let* ((region (if (use-region-p)
                       (list (region-beginning) (region-end))
                     (list (line-beginning-position) (line-end-position))))
           (string (apply #'buffer-substring-no-properties region)))
      (jupyter-eval-string string)
      (deactivate-mark)))

  :general
  (:states '(motion normal visual)
   :keymaps 'julia-mode-map
   :prefix "SPC"
   "ro" 'gatsby:julia-repl
   "rr" 'gatsby:julia-eval-region-or-line
   "rh" 'gatsby:lsp-help-at-point
   "rz" 'jupyter-repl-associate-buffer))

(use-package eglot-jl
  :straight (eglot-jl :repo "non-jedi/eglot-jl" :host github)
  :after (julia-mode eglot)
  :hook
  (julia-mode . eglot-ensure)
  (julia-mode . eglot-jl-init))

(provide 'gatsby:julia)
;;; gatsby:julia.el ends here
