;;; bc-ide-python.el --- provide IDE features for editing python files -*- lexical-binding: t; -*-

;;; Commentary:

;; This module adds on top of the default python.el:
;; use `jupyter' for REPL and `eglot' for all language server stuffs

;;; Code:

;;load pkgs

(use-package python
  :defer t
  :config
  (require 'bc-jupyter)
  (setq-local tab-width 4)

  (defalias 'bc-python-local-repl
    (lambda () (interactive)
      (bc-jupyter-start-or-switch-to-repl "python"))
    "Open a jupyter repl for python interpreter.")

  (defalias 'bc-python-reconnect
    #'jupyter-repl-restart-kernel
    "Reconnect to the current REPL.")

  (defalias 'bc-python-remote-repl
    nil
    "Open a remote jupyter repl for python interpreter, TODO: implement this")

  (defalias 'bc-python--send
    (lambda (string) (bc-jupyter--send (bc-python--dedent string)))
    "Send string using `bc-jupyter--send' with `bc-python--dedent' to processing STRING first.")

  (defun bc-python-eval-class ()
    "Eval the python class at `point'."
    (interactive)
    (let ((beg (save-excursion
                 (word-search-backward "class")))
          (end (condition-case nil
                   (save-excursion
                     (- (word-search-forward "class") 5))
                 (error (point-max)))))
      (jupyter-eval-region beg end)))

  :general
  (:states '(motion normal visual)
   :keymaps 'python-mode-map
   :prefix "SPC"
   "rb" 'jupyter-eval-buffer
   "rf" 'jupyter-eval-defun
   "rr" 'jupyter-eval-line-or-region
   "rc" 'bc-python-eval-class

   "ro" 'bc-python-local-repl

   "rz" 'jupyter-repl-associate-buffer
   "rZ" 'bc-python-reconnect))


(provide 'bc-ide-python)
;;; bc-ide-python ends here
