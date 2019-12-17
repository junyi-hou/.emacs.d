;;; bc-ide-stata.el --- stata settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package stata-mode
  :straight (stata-mode
             :repo "junyi-hou/stata-mode"
             :host github)
  :init
  (require 'bc-jupyter)

  (when (and (featurep 'ob-async)
             (featurep 'jupyter))
    (add-to-list 'ob-async-no-async-languages-alist "jupyter-stata"))

  (defalias 'bc-stata-local-repl
    (lambda () (interactive) (bc-jupyter-start-or-switch-to-repl "stata"))
    "Open a jupyter repl for stata interpreter.")

  (defalias 'bc-stata-reconnect
    #'jupyter-repl-restart-kernel
    "Reconnect to the current REPL.")

  :general
  (:states '(motion normal visual)
   :keymaps 'stata-mode-map
   :prefix "SPC"
   "rb" 'jupyter-eval-buffer
   "rf" 'jupyter-eval-defun
   "rr" 'jupyter-eval-line-or-region
   "rc" 'bc-python-eval-class

   "ro" 'bc-stata-local-repl

   "rz" 'jupyter-repl-associate-buffer
   "rZ" 'bc-stata-reconnect))

(provide 'bc-ide-stata)
;;; bc-ide-stata.el ends here