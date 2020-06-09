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
        (or py-exec
            (completing-read "Cannot infer python interpreter, please select: "
                             '("python2" "python3"))))))

  :hook
  (python-mode . gatsby:python--set-indent-width)
  (python-mode . tree-sitter-hl-mode)

  :config
  (require 'gatsby:jupyter)
  ;; (defconst gatsby:python-remote '("junyi" "10.10.10.106" "9127"))

  (set-face-attribute 'nobreak-space nil :underline nil)

  (defun gatsby:python-local-repl ()
    "Open a jupyter repl for python interpreter."
    (interactive)
    (gatsby:jupyter-start-or-switch-to-repl (gatsby:python--get-python-version)))

  (defalias 'gatsby:python-reconnect
    #'jupyter-repl-restart-kernel
    "Reconnect to the current REPL.")

  (defun gatsby:python--dedent-string (string)
    "Dedent STRING."
    (let ((strings (split-string string "\n")))
      (if (cdr strings)
          ;; multiline, need dedent
          (let ((indent (if (string-match "^[\t ]+" (car strings))
                            (length (match-string 0 (car strings)))
                          0)))
            (string-join (mapcar (lambda (s)
                                   (condition-case _
                                       (substring s indent)
                                     (error ""))) strings) "\n"))
        string)))

  (defun gatsby:python-eval-region-or-line ()
    "If region is active, eval current region, otherwise eval current line."
    (interactive)
    (let* ((region (if (use-region-p)
                       (list (region-beginning) (region-end))
                     (list (line-beginning-position) (line-end-position))))
           (string (gatsby:python--dedent-string (apply
                                                  #'buffer-substring-no-properties
                                                  region))))
      (jupyter-eval-string string)
      (deactivate-mark)))

  (defun gatsby:python--decorated-p ()
    "Return position of \"@\" if line-at-point is part of a python decorator."
    (cond ((string= "@" (buffer-substring (line-beginning-position)
                                          (1+ (line-beginning-position))))
           (line-beginning-position))
          ((and (string= ")" (buffer-substring (1- (line-end-position))
                                               (line-end-position)))
                (progn (goto-char (line-end-position))
                       (evil-jump-item)
                       (string= "@" (buffer-substring (line-beginning-position)
                                                      (1+ (line-beginning-position))))))
           (line-beginning-position))
          (t nil)))

  (defun gatsby:python-eval-block ()
    "Eval the python function or class containing `point'."
    (interactive)
    (let ((beg (save-excursion
                 (when (re-search-backward "^\\(def\\|class\\)[[:blank:]]+" nil 'no-error)
                   (line-move -1)
                   (while (gatsby:python--decorated-p)
                     (line-move -1))
                   (point))))
          (end (save-excursion
                 (condition-case _
                     (re-search-forward "^\\(\\(def\\|class\\)[[:blank:]]+\\|@\\)")
                   (error (goto-char (point-max))))
                 (point))))
      (jupyter-eval-region beg end)))

  :general
  (:states '(motion normal visual)
   :keymaps 'python-mode-map
   :prefix "SPC"
   "rb" 'jupyter-eval-buffer
   "rr" 'gatsby:python-eval-region-or-line
   "rf" 'gatsby:python-eval-block
   "ro" 'gatsby:python-local-repl
   "rh" 'gatsby:lsp-help-at-point

   "rz" 'jupyter-repl-associate-buffer
   "rZ" 'gatsby:python-reconnect))

(provide 'gatsby:python)
;;; gatsby:python ends here
