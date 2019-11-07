;;; bc-jupyter.el --- jupyter frontend for REPL -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package jupyter
  :defer t

  :init
  (setq jupyter-repl-echo-eval-p t
        jupyter-repl-maximum-size 12000
        jupyter-repl-history-maximum-length 300)

  ;; hotfix https://github.com/dzop/emacs-jupyter/issues/172
  (jupyter-tramp-file-name-p "~/.bashrc")

  ;; functions
  (defun bc-jupyter--start-repl (kernel &optional remote)
    "Initiate a REPL for KERNEL and attach it to the current buffer.
If REMOTE is provided, start an remote kernel and connect to it.  Furthermore, if REMOTE is non-nil, prompt the user to set directory."
      (if remote
          (let* ((directory
                  (ivy-read
                   "directory: "
                   #'read-file-name-internal
                   :initial-input
                   (apply
                    #'format
                    (cons "/ssh:%s@%s#%s:/home/junyi/Documents/Research" remote))))
                 (connection-file
                  (bc-jupyter--start-remote-kernel kernel remote directory)))
            (jupyter-connect-repl connection-file
                                  kernel
                                  (current-buffer)))
        (jupyter-run-repl kernel kernel (current-buffer))))

  (defun bc-jupyter--uniquify-process-name ()
    "Uniquify name of the jupyter kernel process name.  Return the process name"
    (let* ((base-name "jupyter-remote-kernel")
           (name-regexp (format "[[:blank:]]\*%s-?\\([[:digit:]]+\\)?\*" base-name))
           (existing-remote-kernel-number
            (car (seq-sort
                  #'>
                  (mapcar (lambda (buffer)
                            (let ((buffer-name (buffer-name buffer)))
                              (if (string-match name-regexp buffer-name)
                                  (if (match-string 1 buffer-name)
                                      (string-to-number (match-string 1 buffer-name))
                                    0)
                                -1)))
                          (buffer-list))))))
      (format "%s%s"
              base-name
              (if (> existing-remote-kernel-number -1)
                  (format "-%d" (1+ existing-remote-kernel-number))
                ""))))

  (defun bc-jupyter--get-remote-connection-file (remote jupyter-remote-kernel-buffer)
    "Return the connection file from JUPYTER-REMOTE-KERNEL-BUFFER located in REMOTE."
    (with-current-buffer jupyter-remote-kernel-buffer
      (goto-char 1)
      (when (re-search-forward ":[[:blank:]]\\([/_\.0-9a-zA-Z-]+\.json\\)")
        (apply #'format (cons "/ssh:%s@%s#%s:%s" (append remote (match-string 1)))))))

  (defun bc-jupyter--start-remote-kernel (kernel remote &optional directory)
    "Start a remote KERNEL at REMOTE.  If DIRECTORY is non-nil, cd to the DIRECTORY on the REMOTE.  Return the tramp location of the connection file.
REMOTE is a list of the user name, the server address, and the port number."
    (let* ((remote-dir (append remote (if directory `(,directory) `(""))))
           (default-directory
             (apply #'format (cons "/ssh:%s@%s#%s:%s" remote-dir)))
           (remote-process-name (bc-jupyter--uniquify-process-name))
           (remote-process-buffer (format " *%s*" remote-process-name)))
      ;; start kernel
      (start-file-process
       remote-process-name
       remote-process-buffer
       (format "/home/%s/.local/bin/jupyter-console" (car remote))
       "--kernel"
       kernel)
      (bc-jupyter--get-remote-connection-file remote remote-process-buffer)))

  (defun bc-jupyter-start-or-switch-to-repl (kernel &optional remote)
    "Switch to REPL associated the current buffer.  If there is no REPL associated with the current buffer, start one according to KERNEL type.  If REMOTE is non-nil, open a remote kernel by calling `bc-jupyter--start-remote-kernel'."
    (interactive)
    (if jupyter-current-client
        (jupyter-repl-pop-to-buffer)
      (let ((code-buffer (current-buffer)))
        (bc-jupyter--start-repl kernel remote)
        (jupyter-repl-pop-to-buffer)
        (switch-to-buffer-other-window code-buffer))))

  (defun bc-jupyter--pop-repl (&rest _)
    "Pop repl buffer, then go back to the code buffer."
    (let* ((code-buffer (current-buffer)))
      (jupyter-repl-pop-to-buffer)
      (switch-to-buffer-other-window code-buffer)))

  (advice-add #'jupyter-eval-line-or-region :before #'bc-jupyter--pop-repl)

  (defun bc-jupyter-kill-repl ()
    "Kill current repl, save workspace to log folder, reset names of all code buffers associated with it."
    (interactive)
    ;; (bc-jupyter-log--save-workspace)
    (kill-buffer-and-window))

  (defun bc-jupyter-clear-buffer ()
    "Jupyter REPL version of `cls'."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (jupyter-send-input)))

  (defun bc-jupyter--deactivate-mark (&rest _)
    "Deactivate mark, use &rest to satisfies the number of arguments"
    (deactivate-mark))

  (advice-add #'jupyter-eval-region :after #'bc-jupyter--deactivate-mark)

  :general
  (:keymaps
   'jupyter-repl-mode-map
   :states
   '(insert normal visual motion emacs)
   (kbd "<up>") 'jupyter-repl-history-previous
   (kbd "<down>") 'jupyter-repl-history-next
   "C-y" 'jupyter-repl-backward-cell
   "C-e" 'jupyter-repl-forward-cell)

  (:keymaps
   'jupyter-repl-mode-map
   :states
   '(normal visual motion)
   "A" (lambda () (interactive)
         (goto-char (point-max))
         (evil-insert 1))
   "SPC" nil)

  (:keymaps 'jupyter-repl-mode-map
   :states '(normal visual motion insert)
   :prefix "C-c"
   "C-c" 'jupyter-repl-interrupt-kernel)

  (:keymaps 'jupyter-repl-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "q" 'bc-jupyter-kill-repl))

(provide 'bc-jupyter)
;;; bc-jupyter.el ends here
