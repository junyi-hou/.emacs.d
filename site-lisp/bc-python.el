;;; bc-python.el --- provide IDE features for editing python files -*- lexical-binding: t; -*-

;;; Commentary:

;; This module adds on top of the default python.el:
;; use `jupyter' for REPL and `eglot' for all language server stuffs

;;; Code:

;;load pkgs

(use-package python
  :defer t
  :init
  (defun bc-python--set-indent-width ()
    (setq-local tab-width 4)
    (setq python-indent-offset 4))

  (defun bc-python--get-python-version ()
    "Infer python versions from shebang.  If there is no shebang, promote the user for python's version."
    (interactive)
    (unless jupyter-current-client
      (let* ((shebang (save-excursion (goto-char 1) (thing-at-point 'line)))
             (py-exec (when (string-match "#!/usr/bin/env\s+\\(python.?\\)" shebang)
                        (match-string 1 shebang))))
        (message (or py-exec
                     (ivy-read "Cannot infer python interpreter, please select: "
                               '("python2" "python3")
                               :action 'identity))))))

  :hook
  (python-mode . bc-python--set-indent-width)

  :config
  (require 'bc-jupyter)
  (defconst bc-python-remote '("junyi" "10.10.10.106" "9127"))

  (set-face-attribute 'nobreak-space nil :underline nil)

  (defalias 'bc-python-local-repl
    (lambda () (interactive)
      (bc-jupyter-start-or-switch-to-repl (bc-python--get-python-version)))
    "Open a jupyter repl for python interpreter.")

  (defalias 'bc-python-reconnect
    #'jupyter-repl-restart-kernel
    "Reconnect to the current REPL.")

  (defalias 'bc-python-remote-repl
    (lambda () (interactive)
      (bc-jupyter-start-or-switch-to-repl "python" bc-python-remote))
    "Open a jupyter repl for python interpreter at remote/DIRECTORY.")

  (defalias 'bc-python--send
    (lambda (string) (bc-jupyter--send (bc-python--dedent string)))
    "Send string using `bc-jupyter--send' with `bc-python--dedent' to processing STRING first.")

  (defun bc-python-eval-class ()
    "Eval the python class at `point'."
    (interactive)
    (let ((beg (save-excursion
                 (re-search-backward "^class[[:blank:]]+")))
          (end (condition-case nil
                   (save-excursion
                     (- (re-search-forward "^\\(class\\|def\\)") 5))
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
   "rO" 'bc-python-remote-repl

   "rz" 'jupyter-repl-associate-buffer
   "rZ" 'bc-python-reconnect))


(provide 'bc-python)
;;; bc-python ends here
