;;; bc-jupyter.el -- jupyter frontend for REPL -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; load pkgs

(require 'bc-ivy)
(require 'bc-eshell)

(use-package jupyter
  :commands (jupyter-run-repl jupyter-connect-repl jupyter-repl-ret jupyter-repl-history-previous jupyter-repl-history-next))


;; customizable variables

;; functions

(defun bc-jupyter-start-repl (kernel &optional remote)
  "Initiate a REPL for KERNEL and attach it to the current buffer.
If REMOTE is provided, start an remote kernel and connect to it."
  (if remote
      (jupyter-connect-repl (bc-jupyter--start-remote-kernel kernel remote)
                            kernel
                            (current-buffer))
    (jupyter-run-repl kernel kernel (current-buffer))))

(defun bc-jupyter--start-remote-kernel (kernel &optional remote)
  "Start a remote KERNEL, return the connection file.
If REMOTE is given, use it, otherwise use `bc-default-remote' instead."
  (interactive)
  (let* ((code-buffer (current-buffer))
         (remote (or remote bc-default-remote)))
    (bc-eshell--open (remote "home/junyi/.virtualenv/nvimpy/bin/"))
    (insert
     (concat
      "jupyter kernel --kernel="
      kernel
      " --KernelManager.connection_file=\"/home/junyi/"
      kernel
      ".json\""))
    (eshell-send-input)
    (switch-to-buffer code-buffer)
    (concat remote "home/junyi/" kernel ".json")))

(defun bc-jupyter--send (string &optional process-string)
  "Send STRING to associated REPL.  If PROCESS-STRING is not nil, apply it to the STRING first."
  (let* ((process-string (or process-string (lambda (x) x)))
         (string (funcall process-string string)))
    (jupyter-repl-pop-to-buffer)
    (insert string)
    (jupyter-repl-ret)))


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
