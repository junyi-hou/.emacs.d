;;; bc-jupyter.el -- jupyter frontend for REPL -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; load pkgs

(require 'bc-ivy)
(require 'bc-eshell)

(use-package jupyter
  :commands (jupyter-run-repl jupyter-connect-repl jupyter-repl-ret jupyter-repl-history-previous jupyter-repl-history-next)
  :config
  (setq-default jupyter-eval-short-result-max-lines 0))


;; customizable variables

(defcustom bc-jupyter-default-venv "~/.virtualenv/nvimpy/"
  "Default virtual environment to use."
  :type 'string
  :group 'baby-carrots)

;; internal variables

(defvar bc-jupyter-venv nil
  "Whether we are in a virtualenv.")

(defvar bc-jupyter-exec-path-no-venv nil
  "Save PATH variable when venv is deactivated.")

(defvar bc-jupyter-eshell-path-no-venv nil
  "Save `eshell-path` variable when venv is deactivated.")

;; functions

(defun bc-jupyter--disable-venv ()
  "Disable virtualenv if it is loaded."
  (when bc-jupyter-venv
    (setenv "PATH" bc-jupyter-exec-path-no-venv)
    (setq exec-path bc-jupyter-eshell-path-no-venv
          bc-jupyter-exec-path-no-venv nil
          bc-jupyter-eshell-path-no-venv nil)))

(defun bc-jupyter--enable-venv (&optional venv)
  "Enable virtualenv.  If no VENV is given, value in `dev-python-default-venv` will be used."
  (interactive)
  (bc-jupyter--disable-venv)
  (let* ((venv (or venv bc-jupyter-default-venv))
         (bin (expand-file-name "bin/" (file-name-as-directory venv))))
    ;; save current variables
    (setq bc-jupyter-exec-path-no-venv (getenv "PATH")
          bc-jupyter-eshell-path-no-venv exec-path)
    ;; set environment variables
    (setenv "PATH" (concat bin path-separator (getenv "PATH")))
    (push bin exec-path))
  (message "Entering python virtual environment"))

(defun bc-jupyter--start-repl (kernel &optional remote)
  "Initiate a REPL for KERNEL and attach it to the current buffer.
If REMOTE is provided, start an remote kernel and connect to it."
  (bc-jupyter--enable-venv)
  (if remote
      (let ((connection-file (bc-jupyter--start-remote-kernel kernel remote)))
        (jupyter-connect-repl connection-file
                              kernel
                              (current-buffer)))
    (jupyter-run-repl kernel kernel (current-buffer))))

(defun bc-jupyter--start-remote-kernel (kernel &optional remote)
  "Start a remote KERNEL, return the connection file.
If REMOTE is given, use it, otherwise use `bc-default-remote' instead."
  (interactive)
  (let* ((code-buffer (current-buffer))
         (remote (or remote bc-default-remote)))
    (bc-eshell--open (concat remote "home/junyi/.virtualenv/nvimpy/bin/"))
    (insert
     (concat
      "./jupyter kernel --kernel="
      kernel
      " --KernelManager.connection_file=\"/home/junyi/"
      kernel
      ".json\""))
    (eshell-send-input)
    (switch-to-buffer code-buffer)
    (concat remote "home/junyi/" kernel ".json")))

(defun bc-jupyter-start-or-switch-to-repl (kernel &optional remote)
  "Switch to REPL associated the current buffer.  If there is no REPL associated with the current buffer, start one according to KERNEL type.  If REMOTE is not nil, open a remote kernel by calling `bc-jupyter--start-remote-kernel'."
  (interactive)
  (unless (boundp 'jupyter-current-client)
    (let* ((connection-file (when remote
                                (bc-jupyter--start-remote-kernel kernel))))
      (bc-jupyter--start-repl kernel remote)))
  (jupyter-repl-pop-to-buffer))


(defun bc-jupyter--send (string)
  "Send STRING to associated REPL."
  (let* ((buffer (current-buffer))
         (pos (if (evil-visual-state-p)
                  (region-end)
                (point))))
    (if jupyter-current-client
        (jupyter-with-repl-buffer jupyter-current-client
          (goto-char (point-max))
          (insert string)
          (jupyter-repl-ret))
      (error "No REPL associated with current buffer"))
    (switch-to-buffer buffer)
    (when (evil-visual-state-p)
      (deactivate-mark))
    (goto-char pos)))

(defun bc-jupyter-eval-buffer-or-region ()
  "If in visual state, evaluate the current region; otherwise evaluate the current buffer."
  (interactive)
  (if (evil-visual-state-p)
      (jupyter-eval-region (region-beginning) (region-end))
    (jupyter-eval-buffer (current-buffer))))

;; settings

(general-define-key
 :keymaps 'jupyter-repl-mode-map
 :states '(insert normal visual motion emacs)
 (kbd "<up>") 'jupyter-repl-history-previous
 (kbd "<down>") 'jupyter-repl-history-next
 "C-u" 'jupyter-repl-backward-cell
 "C-d" 'jupyter-repl-forward-cell)

(general-define-key
 :keymaps 'jupyter-repl-mode-map
 :states '(normal visual motion)
 "A" (lambda () (interactive)
       (goto-char (point-max))
       (evil-insert 1))
 "G" 'jupyter-repl-forward-cell
 "gg" 'jupyter-repl-backward-cell)


(add-hook 'jupyter-repl-mode-hook #'company-mode)

(provide 'bc-jupyter)
;;; bc-jupyter.el ends here
