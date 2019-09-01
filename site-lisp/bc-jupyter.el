;;; bc-jupyter.el --- jupyter frontend for REPL -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

(use-package jupyter
  :commands (jupyter-run-repl jupyter-connect-repl jupyter-repl-ret jupyter-repl-history-previous jupyter-repl-history-next)

  :init
  (setq jupyter-repl-echo-eval-p t
        jupyter-repl-maximum-size 12000
        jupyter-repl-history-maximum-length 300)

  ;; functions
  (defun bc-jupyter--start-repl (kernel &optional remote)
    "Initiate a REPL for KERNEL and attach it to the current buffer.
If REMOTE is provided, start an remote kernel and connect to it."
    (if remote
        (let ((connection-file (bc-jupyter--start-remote-kernel kernel remote)))
          (jupyter-connect-repl connection-file
                                kernel
                                (current-buffer)))
      (jupyter-run-repl kernel kernel (current-buffer))))

  (defun bc-jupyter--start-remote-kernel (kernel remote)
    "Start a remote KERNEL at REMOTE.  Return the tramp location of the connection file."
    ;; TODO: implement this
    )

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

  (defun bc-jupyter--pop-repl (&rest args)
    "Pop repl buffer, then go back to the code buffer."
    (let* ((code-buffer (current-buffer)))
      (jupyter-repl-pop-to-buffer)
      (switch-to-buffer-other-window code-buffer)))

  (defun bc-jupyter--list-attached-buffer (repl)
    "List all code buffer that are attached to the REPL buffer"
    (seq-filter
     (lambda (buffer)
       (with-current-buffer buffer
         (and (bc-evil--is-user-buffer)
              jupyter-current-client
              (string-equal (buffer-name (oref jupyter-current-client buffer)) repl))
         ))
     (buffer-list)))

  (defun bc-jupyter--strip-repl-identifier (&optional code-buffer)
    "Remove the repl identifier in CODE-BUFFER.  If CODE-BUFFER is not given, use `current-buffer' instead."
    (with-current-buffer (or code-buffer (current-buffer))
      (substring (buffer-name) 0 (string-match-p "<repl-\\([0-9]+\\)>" (buffer-name)))))

  (defun bc-jupyter-kill-repl ()
    "Kill current repl, save workspace to log folder, reset names of all code buffers associated with it."
    (interactive)
    (bc-jupyter-log--save-workspace)
    (mapc
     (lambda (buffer)
       (with-current-buffer buffer
         (rename-buffer (bc-jupyter--strip-repl-identifier))))
     (bc-jupyter--list-attached-buffer (buffer-name)))
    (kill-buffer-and-window))

  (defun bc-jupyter--rename-code-buffer (fun &rest args)
    "Rename code buffer to reflect the repl it attaches to."
    (let* ((code-buffer (current-buffer)))
      (apply fun args)
      (with-current-buffer code-buffer
        (let* ((repl-name
                (buffer-name (oref (buffer-local-value 'jupyter-current-client code-buffer) buffer)))
               (repl-no (if (string-match "<\\([0-9]+\\)>" repl-name) (match-string 1 repl-name) "1"))
               (code-buffer-name (bc-jupyter--strip-repl-identifier)))
          (rename-buffer (concat code-buffer-name "<repl-" repl-no ">" ))))))

  (defun bc-jupyter-clear-buffer ()
    "Jupyter REPL version of `cls'."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (jupyter-send-input)))

  (defun bc-jupyter-log--save-workspace ()
    "Save current workspace to PROJECT_ROOT/log/{kernel}-{datetime}."
    (let* ((proj-root (project-current))
           (log-dir (concat (cdr proj-root) "log"))
           (log-dir-p (file-directory-p log-dir)))
      (cond
       (proj-root
        (progn
          (unless log-dir-p
            (make-directory log-dir))
          (goto-char (point-min))
          (let* ((time (format-time-string "%Y%m%d-%H%M"))
                 (kernel (plist-get
                          (oref jupyter-current-client kernel-info)
                          :implementation))
                 (file-name (expand-file-name (concat kernel "-" time) log-dir))
                 (content (bc-jupyter-log--get-cell-content)))
            (with-temp-buffer
              (insert content)
              (write-file file-name)))))
       ((y-or-n-p "Cannot determine project root, quit REPL without saving?")
        nil)
       (t (user-error "Cannot determine project root")))))

  (defun bc-jupyter-log--get-cell-content (&optional formatted)
    "Format jupyter repl workspace cell by cell.

HACK: won't export the last cell, as `jupyter-repl-cell-finalized-p' is nil in that cell."
    (let* ((formatted (or formatted "")))
      (jupyter-repl-next-cell)
      (if (not (jupyter-repl-cell-finalized-p))
          ;; last cell
          formatted
        (let* ((code (jupyter-repl-cell-code))
               (output (jupyter-repl-cell-output))
               (count (number-to-string(jupyter-repl-cell-count)))
               (out
                (if (string-empty-p code)
                    formatted
                  (concat
                   formatted "In [" count "]\n" code "\nOut [" count "]" output "\n"))))
          (bc-jupyter-log--get-cell-content out)))))

  (advice-add #'jupyter-eval-region :after #'deactivate-mark)
  (advice-add #'jupyter-eval-line-or-region :before #'bc-jupyter--pop-repl)
  (advice-add #'jupyter-repl-associate-buffer :around #'bc-jupyter--rename-code-buffer)
  (advice-add #'bc-jupyter--start-repl :around #'bc-jupyter--rename-code-buffer)

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
