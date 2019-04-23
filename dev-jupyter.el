;;; dev-jupyter.el -- jupyter frontend for REPL -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; load pkgs

(require 'dev-ivy)
(require 'dev-eshell)

(use-package jupyter
  :commands (jupyter-run-repl jupyter-connect-repl jupyter-repl-ret jupyter-repl-history-previous jupyter-repl-history-next))


;; customizable variables

(defcustom dev-jupyter-remote-kernel-cmd-locaton
  "home/junyi/.virtualenv/nvimpy/bin/"
  "The cmd for launch remote kernel."
  :type 'string
  :group 'development)

;; variable for internal use

(defvar-local dev-jupyter-repl nil
  "the REPL buffer associated with a buffer")


;; functions

(defun dev-jupyter--set-repl-name (kernel &optional connect buffer)
  "Set repl name based on KERNEL type.  If the connection file given in CONNECT is a remote file, modify the repl name accordingly.  If BUFFER is not nil, return the buffer name (e.g., with prefix *jupyter-repl), else only return the kernel name."
  (if (and connect (string-match-p "/ssh:" connect))
      (if buffer
          (concat "*jupyter-repl[remote-" kernel "]*")
        (concat "remote-" kernel))
    (if buffer
        (concat "*jupyter-repl[" kernel "]*")
      kernel)))

(defun dev-jupyter--open-repl (kernel code-buffer &optional connect)
  "Start a process for jupyter KERNEL and associate it with CODE-BUFFER by setting buffer-local variable `dev-jupyter-repl'.  If CONNECT is not nil, connect to existing kernel defined by connect instead."
  (interactive)
  (let ((repl-name (dev-jupyter--set-repl-name kernel connect t))
        (kernel-name (dev-jupyter--set-repl-name kernel connect nil)))
  (if connect
      (jupyter-connect-repl connect kernel-name code-buffer)
    (jupyter-run-repl kernel kernel-name code-buffer))
  (with-current-buffer code-buffer
    (setq-local dev-jupyter-repl repl-name))))

(defun dev-jupyter-open-or-switch-to-repl (kernel &optional connect)
  "Open jupyter repl for KERNEL.  Optionally supply a connection file via CONNECT, then connect to the kernel defined by the connection file."
  (interactive)
  (if (get-buffer (or dev-jupyter-repl ""))
      (switch-to-buffer-other-window dev-jupyter-repl)
    (let* ((code-buffer (current-buffer)))
      (dev-jupyter--open-repl kernel code-buffer connect)
      ;; display repl buffer
      (switch-to-buffer-other-window dev-jupyter-repl)
      ;; go back to code buffer
      (switch-to-buffer-other-window code-buffer))))

(defun dev-jupyter-open-remote-repl (kernel)
  "Launch a KERNEL on the REMOTE machine in the background.  Open the REPL by connected to the opened kernel."
  (interactive)
  (if (get-buffer (or dev-jupyter-repl ""))
      (switch-to-buffer-other-window dev-jupyter-repl)
    (let* ((remote-bin (concat
                        dev-default-remote-machine
                        (file-name-directory
                         dev-jupyter-remote-kernel-cmd-locaton)))
           (code-buffer (current-buffer)))
      (dev-eshell--open remote-bin)
      (rename-buffer (concat "*eshell[remote-" kernel "-backend]*"))
      (insert (concat "./jupyter kernel --kernel=" kernel))
      (eshell-send-input)
      (evil-normal-state)
      (sit-for 1)
      (let* ((beg (search-backward-regexp "\/run/user\/.*?"))
             (end (line-end-position))
             (connection-file
              (concat dev-default-remote-machine
                      (buffer-substring-no-properties beg end))))
        (dev-jupyter--open-repl kernel code-buffer connection-file)
        (switch-to-buffer code-buffer)
        ;; open repl buffer
        (switch-to-buffer-other-window dev-jupyter-repl)
        ;; go back to code buffer
        (switch-to-buffer-other-window code-buffer)))))

(defun dev-jupyter--send-string (string kernel)
  "Send STRING to the KERNEL repl in BUFFER.  Go back to current buffer."
  (let* ((repl-buffer dev-jupyter-repl)
         (code-buffer (current-buffer)))
    (unless dev-jupyter-repl
      (dev-jupyter--attach-buffer kernel))
    (switch-to-buffer-other-window repl-buffer)
    (goto-char (point-max))
    (insert string)
    (jupyter-repl-ret)
    (switch-to-buffer-other-window code-buffer)
    (when (region-active-p)
      (let ((new-cursor (region-end)))
        (deactivate-mark)
        (goto-char new-cursor)))))

(defun dev-jupyter-send-line-or-region (kernel)
  "Send current line or visually selected region to KERNEL."
  (interactive)
  (if (evil-visual-state-p)
      (dev-jupyter--send-string
       (buffer-substring (region-beginning) (region-end)) kernel)
    ;; else: send current line
    (dev-jupyter--send-string (thing-at-point 'line t) kernel)))

(defun dev-jupyter-send-buffer-or-region (kernel)
  "Send buffer or visually selected region to KERNEL."
  (interactive)
  (if (evil-visual-state-p)
      (dev-jupyter--send-string
       (buffer-substring (region-beginning) (region-end)) kernel)
    ;; else: send the whole buffer
    (dev-jupyter--send-string (buffer-string) kernel)))

(defun dev-jupyter--list-active-repl (kernel)
  "List all currently actively jupyter repl of type KERNEL."
  (require 'dash)
  (-non-nil (mapcar (lambda (buffer)
            (when (string-match-p
                 (concat "\*jupyter-repl\\[\\(remote-\\)?" kernel)
                 (buffer-name buffer))
                (buffer-name buffer)))
          (buffer-list))))

(defun dev-jupyter--attach-buffer (kernel)
  "Attach current buffer to a jupyter repl of KERNEL.  If there is only one such repl, connect to it by setting `dev-jupyter-repl' to that repl.  If there are more than one active repl for kernel type, call an ivy read buffer and choose one from it."
  (interactive)
  (let ((active-repl (dev-jupyter--list-active-repl kernel)))
    (cond ((eq (length active-repl) 1)
           (progn
             (setq-local dev-jupyter-repl (car active-repl))
             (jupyter-repl-associate-buffer (car active-repl))))

          ((> (length active-repl) 1)
           (ivy-read
            (concat "connecting "
                    (file-name-nondirectory (buffer-file-name))
                    " to:")
            active-repl
            :action (lambda (repl)
                      (setq-local dev-jupyter-repl repl)
                      (jupyter-repl-associate-buffer repl))))

          (t (error (concat "No active REPL for " kernel))))))

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

(provide 'dev-jupyter)
;;; dev-jupyter.el ends here
