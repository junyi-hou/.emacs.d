;;; ide-python.el -- provide IDE features for editing python files

;;; Commentary:
;; This module adds on top of the default python.el:
;; linting and autocompletion support using flycheck and company;
;; REPL support using jupyter
;; jump to definition using dumb-jump

;;; Code:

;;load pkgs
(require 'dev-autocomplete)
(require 'dev-linter)
(require 'dev-jump)
(require 'dev-jupyter)

(use-package company-jedi
  :after company
  :commands company-jedi)


;; customizable variables

(defgroup dev-python nil
  "Settings related to creating python IDE"
  :group 'development)

(defcustom dev-python-default-venv "~/.virtualenv/nvimpy/"
  "Default virtual environment to use."
  :type 'string
  :group 'development)

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
  (dev-jupyter-connect-kernel "python3")
  (setq dev-python-repl "*jupyter-repl[python3]*"))

(defun dev-python--open-repl-or-switch-to-repl ()
  "Open a REPL using jupyter kernel.  If there is already a python REPL opened, switch to that REPL."
  (interactive)
  (if dev-python-repl
      (switch-to-buffer-other-window dev-python-repl)
    ;; else if the repl does not exists
    (jupyter-run-repl "python3" "python3")
    (setq dev-python-repl "*jupyter-repl[python3]*")))

(defun dev-python--run-line-or-visual ()
  "Send current line or visually selected region to interpreter."
  (interactive)
  (if (evil-visual-state-p)
      (dev-jupyter-send-code
       (buffer-substring (region-beginning) (region-end))
       dev-python-repl)
    ;; else: send current line
    (dev-jupyter-send-code (thing-at-point 'line t) dev-python-repl)))

(defun dev-python--run-buffer-or-visual ()
  "Send buffer or visually selected region to interpreter."
  (interactive)
  (if (evil-visual-state-p)
      (progn
        (dev-jupyter-send-code (dev-jupyter--get-vline) dev-python-repl)
        (deactivate-mark))
    ;; else: send the whole buffer
    (dev-jupyter-send-code (buffer-string) dev-python-repl)))

;; settings

(add-hook
 'python-mode-hook
 (lambda ()
   ;; load venv
   (dev-python--enable-venv)

   ;; setup auto complete
   (eval-after-load 'company
     (eval-after-load 'company-jedi
       (add-to-list 'company-backends 'company-jedi)))
   (company-mode)

   ;; setup linter
   (flycheck-mode)

   ;; keymaps
   (general-define-key
    :states '(motion normal visual)
    :keymaps python-mode-map
    :prefix "SPC"
    "rr" 'dev-python--run-buffer-or-visual
    "rl" 'dev-python--run-line-or-visual
    "ro" 'dev-python--open-repl-or-switch-to-repl
    "rO" 'dev-python--open-remote-repl)))


(provide 'ide-python)
;;; ide-python ends here
