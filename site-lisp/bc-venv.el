;;; bc-venv.el --- python virtual env setting -*- lexical-binding: t; -*-

;;; Commentary:
;;; TODO: how to correctly inherit local variable?
;;; TODO: need to understand better `inherit-local'

;;; Code:

(use-package inherit-local
  :defer t)

(defgroup bc-venv nil
  "Customizable group for using python virtual environment."
  :group 'baby-carrots
  :prefix "bc-venv-")

(defcustom bc-venv-default-py2-venv
  "~/.virtualenv/env2/"
  "Default virtualenv for python 2."
  :group 'bc-venv
  :type 'string)

(defcustom bc-venv-default-py3-venv
  "~/.virtualenv/nvimpy/"
  "Default virtualenv for python 3."
  :group 'bc-venv
  :type 'string)

(defvar-local bc-venv-current-venv nil
  "Whether we are in a virtualenv.")

(defvar-local bc-venv-exec-path-no-venv nil
  "Save PATH variable when venv is deactivated.")

(defvar-local bc-venv-eshell-path-no-venv nil
  "Save `eshell-path` variable when venv is deactivated.")

;; functions

(defun bc-venv--disable-venv ()
  "Disable virtualenv if it is loaded."
  (when bc-venv-current-venv
    (setenv "PATH" bc-venv-exec-path-no-venv)
    (inherit-local-permanent exec-path bc-venv-eshell-path-no-venv)
    (inherit-local-permanent bc-venv-exec-path-no-venv nil)
    (inherit-local-permanent bc-venv-eshell-path-no-venv nil)))

(defun bc-venv--enable-venv (&optional venv)
  "Enable virtualenv.  If the virtualenv is explicitly given in VENV, use it, otherwise if a .venv folder is present, use it, otherwise fall back to `bc-venv-default-py3-venv'."
  (interactive)
  (bc-venv--disable-venv)
  (let* ((local-venv (concat (cdr (project-current nil)) ".venv/"))
         (venv (or
                venv
                (when (file-directory-p local-venv) local-venv)
                bc-venv-default-py3-venv))
         (bin (expand-file-name "bin/" (file-name-as-directory venv))))
    ;; save current variables
    (inherit-local-permanent bc-venv-exec-path-no-venv (getenv "PATH"))
    (inherit-local-permanent bc-venv-eshell-path-no-venv exec-path)
    ;; set environment variables
    (make-local-variable 'process-environment)
    (inherit-local-permanent process-environment
                (copy-sequence process-environment))
    (setenv "PATH" (concat bin path-separator (getenv "PATH")))
    (make-local-variable 'exec-path)
    (inherit-local-permanent exec-path
                             (cons bin exec-path))
    (inherit-local-permanent bc-venv-current-venv venv)
    (message "Entering python virtual environment")))


(defun bc-venv--get-python-version (&optional py2-venv py3-venv)
  "A hacky way to determined which python environment to start.  If a comment \"# -*- python2 -*-\" is found, return PY2-VENV, otherwise return PY3-VENV.  If these variables are omitted, fall back to `bc-venv-default-py2-venv' and `bc-venv-default-py2-venv'."
  (let ((py2-venv (or py2-venv bc-venv-default-py2-venv))
        (py3-venv (or py3-venv bc-venv-default-py3-venv)))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "# -*- python2 -*-" nil t)
          py2-venv
        py3-venv))))

;; settings

(advice-add 'generate-new-buffer :around
            (defun around-generate (orig name)
              (if (eq (aref name 0) ?\s)
                  (let ((buf (funcall orig name)))
                    (inherit-local-inherit-child buf)
                    buf)
                (funcall orig name))))

(provide 'bc-venv)
;;; bc-venv.el ends here
