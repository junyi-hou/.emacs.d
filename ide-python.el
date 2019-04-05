;;; ide-python.el -- provide IDE features for editing python files -*- lexical-binding: t; -*-

;;; Commentary:
;; This module adds on top of the default python.el:
;; linting and autocompletion support using flycheck and company;
;; REPL support using jupyter
;; jump to definition using dumb-jump
;; code formatting using py-papf

;;; Code:

;;load pkgs
(require 'dev-autocomplete)
(require 'dev-linter)
(require 'dev-jump)
(require 'dev-repl)

(use-package company-jedi
  :after company
  :commands company-jedi)

(use-package py-yapf
  :commands py-yapf-enable-on-save)

;; customizable variables

(defcustom dev-python-default-venv "~/.virtualenv/nvimpy/"
  "Default virtual environment to use."
  :type 'string)

;; internal variables

(defvar dev-python-repl nil
  "Name of the python REPL buffer, nil means no such buffer.")

(defvar dev-python-venv nil
  "Whether we are in a virtualenv.")

(defvar dev-python-exec-path-no-venv nil
  "Save PATH variable when venv is deactivated.")

(defvar dev-python-eshell-path-no-venv nil
  "Save `eshell-path` variable when venv is deactivated.")

;; functions

(defalias 'dev-python--send-string
  (lambda (string)
    (dev-repl--send-string
     dev-python-repl
     (lambda (x) (insert (concat "exec(open('" x "').read())")))
     string))
  "Send STRING to python interpreter.")

(defun dev-python--disable-venv ()
  "Disable virtualenv if it is loaded."
  (when dev-python-venv
    (setenv "PATH" dev-python-exec-path-no-venv)
    (setq exec-path dev-python-eshell-path-no-venv
          exec-path-no-venv nil
          eshell-path-no-venv nil)))

(defun dev-python--enable-venv (&optional venv)
  "Enable virtualenv.  If no VENV is given, value in `dev-python-default-venv` will be used."
  (interactive)
  (dev-python--disable-venv)
  (let* ((venv (or venv dev-python-default-venv))
         (bin (expand-file-name "bin/" (file-name-as-directory venv))))
    ;; set dev-python-venv variable
    (setq dev-python-venv
          (file-name-nondirectory (directory-file-name venv)))
    ;; save current variables
    (setq dev-python-exec-path-no-venv (getenv "PATH")
          dev-python-eshell-path-no-venv exec-path)
    ;; set environment variables
    (setenv "PATH" (concat bin path-separator (getenv "PATH")))
    (push bin exec-path))
  (message "entering python venv"))

(defun dev-python--open-remote-repl ()
  "Open a REPL using jupyter kernel in a remote server."
  (interactive)
  (dev-repl-open-repl "python3")
  (setq dev-python-repl "*REPL[python3]*"))

(defun dev-python--open-repl-or-switch-to-repl ()
  "Open a REPL using jupyter kernel.  If there is already a python REPL opened, switch to that REPL."
  (interactive)
  (if dev-python-repl
      (switch-to-buffer-other-window dev-python-repl)
    ;; else if the repl does not exists
    (progn
      (dev-repl-open-repl "python3")
      (setq dev-python-repl "*REPL[python3]*"))))

(defun dev-python--run-line-or-visual ()
  "Send current line or visually selected region to interpreter."
  (interactive)
  (if (evil-visual-state-p)
      (dev-python--send-string
       (buffer-substring (region-beginning) (region-end)))
    ;; else: send current line
    (dev-python--send-string (thing-at-point 'line t))))

(defun dev-python--get-pos (l c)
  "Return the buffer position at line L and column C."
  (save-excursion
    (goto-char (point-min))
    (goto-line l)
    (move-to-column c)
    (point)))

(defun dev-python--get-vline ()
  "Return visually selected region, expended to include all of the lines."
  (interactive)
  (let* ((begin-line (line-number-at-pos (region-beginning)))
         (end-line   (line-number-at-pos (region-end))))
    (deactivate-mark)
    (let* ((begin-pos (dev-python--get-pos begin-line 0))
           (end-pos (dev-python--get-pos end-line (window-body-width))))
      (buffer-substring begin-pos end-pos))))

(defun dev-python--run-buffer-or-visual-line ()
  "Send buffer or visually selected region to interpreter."
  (interactive)
  (if (evil-visual-state-p)
      (progn
        (dev-python--send-string (dev-python--get-vline))
        (deactivate-mark))
    ;; else: send the whole buffer
    (dev-python--send-string (buffer-string))))

(defun dev-python--hook ()
  "Initiate venv, autocomplete and linters."

   ;; load venv
   (dev-python--enable-venv)

   ;; setup auto complete
   (add-to-list (make-local-variable 'company-backends) 'company-jedi)
   (company-mode 1)

   ;; setup linter
   (flycheck-mode 1)

   ;; rainbow parentheses
   (rainbow-delimiters-mode 1))

;; settings

;; key bindings
(general-define-key
 :states '(motion normal visual)
 :keymaps 'python-mode-map
 :prefix "SPC"
 "rr" 'dev-python--run-buffer-or-visual-line
 "rl" 'dev-python--run-line-or-visual
 "ro" 'dev-python--open-repl-or-switch-to-repl
 "rO" 'dev-python--open-remote-repl)

(add-hook 'python-mode-hook #'dev-python--hook)
(add-hook 'python-mode-hook #'py-yapf-enable-on-save)

(provide 'ide-python)
;;; ide-python ends here

; LocalWords:  venv linter keymaps SPC
