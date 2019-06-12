;;; bc-jupyter.el --- jupyter frontend for REPL -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; load pkgs

(require 'bc-ivy)
(require 'bc-eshell)

(use-package jupyter
  :defer t
  :commands (jupyter-run-repl jupyter-connect-repl jupyter-repl-ret jupyter-repl-history-previous jupyter-repl-history-next)

  :general
  (:keymaps
   'jupyter-repl-mode-map
   :states
   '(insert normal visual motion emacs)
   (kbd "<up>") 'jupyter-repl-history-previous
   (kbd "<down>") 'jupyter-repl-history-next
   "C-u" 'jupyter-repl-backward-cell
   "C-d" 'jupyter-repl-forward-cell)

  (:keymaps
   'jupyter-repl-mode-map
   :states
   '(normal visual motion)
   "A" (lambda () (interactive)
         (goto-char (point-max))
         (evil-insert 1))
   "G" 'jupyter-repl-forward-cell
   "gg" 'jupyter-repl-backward-cell
   "SPC" nil)

  (:keymaps 'jupyter-repl-mode-map
   :states '(normal visual motion insert)
   :prefix "C-c"
   "C-c" 'jupyter-repl-interrupt-kernel
   "C-l" 'bc-jupyter-clear-buffer)

  (:keymaps
   'jupyter-repl-mode-map
   :states
   '(normal visual motion)
   :prefix
   "SPC"
   "q" 'kill-buffer-and-window))

(defun bc-jupyter--start-repl (kernel &optional remote)
  "Initiate a REPL for KERNEL and attach it to the current buffer.
If REMOTE is provided, start an remote kernel and connect to it."
   
  (if remote
      (let ((connection-file (bc-jupyter--start-remote-kernel kernel remote)))
        (jupyter-connect-repl connection-file
                              kernel
                              (current-buffer)))
    (jupyter-run-repl kernel kernel (current-buffer))))

(defun bc-jupyter--start-remote-kernel (kernel &optional location)
  "Start a remote KERNEL, save the connection file to a default location.  If LOCATION is given, use it, otherwise use `bc-default-remote' instead."
  (interactive)
  (let* ((code-buffer (current-buffer))
         (location (or (when (stringp location) location) bc-default-remote)))
    (bc-eshell--open (concat location "home/junyi/.virtualenv/nvimpy/bin/"))
    (insert
     (concat
      "./jupyter-console --kernel="
      kernel
      " -f=/home/junyi/" kernel ".json"))
    (eshell-send-input)
    (rename-buffer (concat "*jupyter-remote-" kernel "-kernel*" ))
    (switch-to-buffer code-buffer)
    (concat location "home/junyi/" kernel ".json")))

(defun bc-jupyter-start-or-switch-to-repl (kernel &optional remote)
  "Switch to REPL associated the current buffer.  If there is no REPL associated with the current buffer, start one according to KERNEL type.  If REMOTE is not nil, open a remote kernel by calling `bc-jupyter--start-remote-kernel'."
  (interactive)
  (condition-case nil
      (jupyter-repl-pop-to-buffer)
    (error (let* ((code-buffer (current-buffer)))
             (setq-local jupyter-current-client nil)
             (bc-jupyter--start-repl kernel remote)
             (jupyter-repl-pop-to-buffer)
             (switch-to-buffer-other-window code-buffer)))))


(defun bc-jupyter--send (string)
  "Send STRING to associated REPL."
  (let* ((buffer (current-buffer)))
    (if jupyter-current-client
        (jupyter-with-repl-buffer jupyter-current-client
          (goto-char (point-max))
          (insert (concat string "\n"))
          (jupyter-repl-ret))
      (error "No REPL associated with current buffer"))
    (switch-to-buffer buffer)
    (when (evil-visual-state-p)
      (deactivate-mark))))

(defun bc-jupyter-eval-buffer-or-region ()
  "If in visual state, evaluate the current region; otherwise evaluate the current buffer."
  (interactive)
  (if (evil-visual-state-p)
      (jupyter-eval-region (region-beginning) (region-end))
    (jupyter-eval-buffer (current-buffer)))
  (deactivate-mark))

(defalias 'jupyter-eval-line-or-region
  (lambda () (interactive) (jupyter-eval-line-or-region) (deactivate-mark))
  "Exit visual state after run `jupyter-eval-line-or-region'.")


(defun bc-jupyter-reconnect (kernel)
  "Try to reconnect to the KERNEL.  For a local kernel, use built-in `jupyter-repl-restart-kernel', for a remote kernel, close the current REPL and start a new one using the connection file."
  (interactive)
  (let* ((local (ignore-errors (jupyter-repl-restart-kernel) t)))
    (unless local  ; restart remote kernel
      (let* ((connection-file (concat bc-default-remote "home/junyi" kernel ".json")))
        (if jupyter-current-client
            (progn
              (jupyter-with-client-buffer jupyter-current-client
                (kill-buffer-and-window))
              (setq-local jupyter-current-client nil)
              (jupyter-connect-repl
               connection-file
               kernel
               (current-buffer))))))))

(defun bc-jupyter-clear-buffer ()
  "Eshell version of `cls'."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (jupyter-send-input)))


;; settings

(defun bc-jupyter--hook ()
  "Set up proper `company-backends' for jupyter repl."
  (setq company-backends
        '((company-capf company-yasnippet company-files)
          (company-dabbrev company-abbrev)))
  (company-mode 1))

(add-hook 'jupyter-repl-mode-hook #'bc-jupyter--hook)



(provide 'bc-jupyter)
;;; bc-jupyter.el ends here
