;;; gatsby:python.el --- provide IDE features for editing python files -*- lexical-binding: t; -*-

;;; Commentary:

;; This module adds on top of the default python.el:
;; use `jupyter' for REPL and `eglot' for all language server stuffs

;;; Code:

;;load pkgs

(use-package python
  :defer t
  :init
  (defun gatsby:python--set-indent-width ()
    (setq-local tab-width 4)
    (setq python-indent-offset 4))

  (defun gatsby:python--get-python-version ()
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
  (python-mode . gatsby:python--set-indent-width)

  :config
  (require 'gatsby:jupyter)
  (defconst gatsby:python-remote '("junyi" "10.10.10.106" "9127"))

  (set-face-attribute 'nobreak-space nil :underline nil)

  (defalias 'gatsby:python-local-repl
    (lambda () (interactive)
      (gatsby:jupyter-start-or-switch-to-repl (gatsby:python--get-python-version)))
    "Open a jupyter repl for python interpreter.")

  (defalias 'gatsby:python-reconnect
    #'jupyter-repl-restart-kernel
    "Reconnect to the current REPL.")

  (defalias 'gatsby:python-remote-repl
    (lambda () (interactive)
      (gatsby:jupyter-start-or-switch-to-repl "python" gatsby:python-remote))
    "Open a jupyter repl for python interpreter at remote/DIRECTORY.")

  (defalias 'gatsby:python--send
    (lambda (string) (gatsby:jupyter--send (gatsby:python--dedent string)))
    "Send string using `gatsby:jupyter--send' with `gatsby:python--dedent' to processing STRING first.")

  (defun gatsby:python-eval-class ()
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
   "rc" 'gatsby:python-eval-class

   "ro" 'gatsby:python-local-repl
   "rO" 'gatsby:python-remote-repl

   "rz" 'jupyter-repl-associate-buffer
   "rZ" 'gatsby:python-reconnect))

(provide 'gatsby:python)
;;; gatsby:python ends here
