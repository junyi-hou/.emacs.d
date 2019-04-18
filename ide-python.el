;;; ide-python.el -- provide IDE features for editing python files -*- lexical-binding: t; -*-

;;; Commentary:
;; This module adds on top of the default python.el:
;; linting and autocompletion support using flycheck and company;
;; REPL support using jupyter;
;; jump to definition using dumb-jump;
;; code formatting using py-papf

;;; Code:

;;load pkgs
(require 'dev-autocomplete)
(require 'dev-linter)
(require 'dev-jump)
(require 'dev-jupyter)

(use-package company-jedi
  :after company
  :commands company-jedi)

(use-package py-yapf
  :commands py-yapf-enable-on-save)

;; customizable variables

(defcustom dev-python-default-venv "~/.virtualenv/nvimpy/"
  "Default virtual environment to use."
  :type 'string
  :group 'development)

;; internal variables

(defvar dev-python-venv nil
  "Whether we are in a virtualenv.")

(defvar dev-python-exec-path-no-venv nil
  "Save PATH variable when venv is deactivated.")

(defvar dev-python-eshell-path-no-venv nil
  "Save `eshell-path` variable when venv is deactivated.")

;; functions
(defalias 'dev-python-open-or-switch-to-repl
  (lambda () (interactive)
    (dev-jupyter-open-or-switch-to-repl "python"))
  "Open a jupyter repl for python interpreter.")

(defalias 'dev-python-open-remote-repl
  (lambda () (interactive)
    (dev-jupyter-open-remote-repl "python"))
  "Open a remote jupyter repl for python interpreter")

(defalias 'dev-python-send-line-or-region
  (lambda () (interactive)
    (dev-jupyter-send-line-or-region "python"))
  "Send current line or visually selected region to python interpreter.")

(defalias 'dev-python-send-buffer-or-region
  (lambda () (interactive)
    (dev-jupyter-send-buffer-or-region "python"))
  "Send buffer or visually selected region to python interpreter.")

(defalias 'dev-python-attach-buffer
  (lambda () (interactive)
    (dev-jupyter--attach-buffer "python"))
  "Attach current code buffer to an existing python REPL")


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
    ;; save current variables
    (setq dev-python-exec-path-no-venv (getenv "PATH")
          dev-python-eshell-path-no-venv exec-path)
    ;; set environment variables
    (setenv "PATH" (concat bin path-separator (getenv "PATH")))
    (push bin exec-path))
  (message "Entering python virtual environment"))

(defun dev-python--hook ()
  "Initiate venv, autocomplete and linters."
   ;; load venv
   (dev-python--enable-venv)
   ;; setup auto complete
   (make-local-variable 'company-backends)
   (setq company-backends '((company-files company-capf company-yasnippet company-jedi)))
   (company-mode 1)
   ;; setup linter
   (flycheck-mode 1))

;; settings

;; key bindings
(general-define-key
 :states '(motion normal visual)
 :keymaps 'python-mode-map
 :prefix "SPC"
 "rr" 'dev-python-send-buffer-or-region
 "rl" 'dev-python-send-line-or-region
 "ro" 'dev-python-open-or-switch-to-repl
 "rO" 'dev-python-open-remote-repl
 "rc" 'dev-python-attach-buffer
 "rh" 'jupyter-inspect-at-point)

(add-hook 'python-mode-hook #'dev-python--hook)
(add-hook 'python-mode-hook #'py-yapf-enable-on-save)

(provide 'ide-python)
;;; ide-python ends here

; LocalWords:  venv linter keymaps SPC virtualenv
