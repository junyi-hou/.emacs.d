;;; gatsby:stata.el --- stata settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package stata-mode
  :straight (stata-mode
             :repo "junyi-hou/stata-mode"
             :host github)
  :init
  (require 'gatsby:jupyter)

  (defalias 'gatsby:stata-local-repl
    (lambda () (interactive) (gatsby:jupyter-start-or-switch-to-repl "stata"))
    "Open a jupyter repl for stata interpreter.")

  (defalias 'gatsby:stata-reconnect
    #'jupyter-repl-restart-kernel
    "Reconnect to the current REPL.")

  :general
  (:states '(motion normal visual)
   :keymaps 'stata-mode-map
   :prefix "SPC"
   "rb" 'jupyter-eval-buffer
   "rf" 'jupyter-eval-defun
   "rr" 'jupyter-eval-line-or-region
   "ro" 'gatsby:stata-local-repl

   "rz" 'jupyter-repl-associate-buffer
   "rZ" 'gatsby:stata-reconnect))

(provide 'gatsby:stata)
;;; gatsby:stata.el ends here
