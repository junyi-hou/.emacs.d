;;; dev-repl.el -- REPL support for various languages using eshell and the CLI frontend -*- lexical-binding: t; -*-

;;; Commentary:

;; As for now, using terminal Jupyter frontend
;; it does not support graph/figure/etc., so will have to use alternative
;; arrangements

;;; Code:

(require 'dash)

(defgroup dev-repl nil
  "Custom group for dev-repl")

(defcustom dev-repl-jupyter-local-kernel "~/.virtualenv/nvimpy/"
  "The venv where local Jupyter kernel locates."
  :type 'string
  :group 'dev-repl)

(defcustom dev-repl-jupyter-remote-kernel "ssh"
  "The venv where remote Jupyter kernel locates."
  :type 'string
  :group 'dev-repl)

(defvar dev-repl--temporary-file-directory
  (if (file-remote-p default-directory)
      (concat (file-remote-p default-directory) "/.tmp")
    temporary-file-directory)
  "Location to store tmp file.")

;; functions

(defun dev-repl--split ()
  "Split the current window side-by-side if its wide enough, otherwise split it top-and-bottom.  Then switch to that window."
  (interactive)
  (if (< (window-body-width) 90)
      (progn
        (split-window-below)
        (windmove-down 1))
    (progn
      (split-window-right)
      (windmove-right 1))))

(defun dev-repl-open-repl (kernel &optional remote)
  "Open a CLI repl for KERNEL base on eshell.  If REMOTE is non-nil, use `dev-repl-jupyter-remote-kernel`, otherwise use `dev-repl-jupyter-local-kernel`."
  (interactive)
  (let* ((kernel-loc (or (and remote dev-repl-jupyter-remote-kernel)
                         (and dev-repl-jupyter-local-kernel)))
         (jupyter (concat kernel-loc "bin/jupyter console --simple-prompt --kernel=" kernel)))
    (dev-repl--split)
    (eshell)
    (rename-buffer (concat "*REPL[" kernel "]*"))
    (goto-char (point-max))
    (insert jupyter)
    (eshell-send-input)))

(defun dev-repl--dedent (string)
  "Remove common indentations of STRING."
  (let* ((lines (split-string string "\n"))
         (indentation (if (string-match "^[\t ]+" (car lines))
                          (match-end 0)
                        0))
         (new-lines (-map (lambda (x)
                            (if (> (length x) indentation)
                                (substring x indentation (length x))
                              ""))
                          lines)))
    (string-join new-lines "\n")))


(defun dev-repl--generate-tmp-file (string)
  "Generate a temp file from STRING, return the tmp file."
    (let* ((temporary-file-directory dev-repl--temporary-file-directory)
           (temp-file-name (make-temp-file "codeblock-")))
    (with-temp-file temp-file-name
      (insert (dev-repl--dedent string))
      (delete-trailing-whitespace))
    temp-file-name))

(defun dev-repl--send-string (kernel fn string)
  "Send STRING to KERNEL interpreter.

For now, the function writes STRING into a temp file by calling `dev-repl--generate-tmp-file`, then runs the temp file in the interpreter by using the language-dependent function FN."
  (let* ((tmp-file (dev-repl--generate-tmp-file string))
         (buffer (current-buffer)))
    (switch-to-buffer-other-window kernel)
    (goto-char (point-max))
    (funcall fn tmp-file)
    (eshell-send-input)
    (switch-to-buffer-other-window buffer)))

(provide 'dev-repl)
;;; dev-repl.el ends here
