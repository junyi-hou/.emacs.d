;;; bc-jupyter.el --- jupyter frontend for REPL -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO: multiple buffers could use same repl, what is the optimal way to do that?
;; TODO: add logging support - when quit, save the repl buffer to PROJ_ROOT/log/date.log file
;; FIXME: the following function needs formatting (in/out)
;; (defun bc-jupyter-logger ()
;;   "Save current workspace to PROJECT_ROOT/log/{kernel}-{datetime}."
;;   (let* ((proj-root (project-current))
;;          (log-dir (file-directory-p (concat (cdr proj-root) "log"))))
;;     (if proj-root
;;       (progn
;;         (unless log-dir
;;           (make-directory log-dir))
;;         (let* ((time (format-time-string "%Y%m%d-%H%M"))
;;                (file-name (expand-file-name (concat "jupyter-" time) log-dir)))
;;           (save-excursion
;;             (widen)
;;             (write-region 1 (point-max) file-name))))
;;       (user-error "Cannot determine project root"))))

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

  (defun bc-jupyter-eval-buffer-or-region ()
    "If in visual state, evaluate the current region; otherwise evaluate the current buffer."
    (interactive)
    (bc-jupyter--pop-repl)
    (if (evil-visual-state-p)
        (jupyter-eval-region (region-beginning) (region-end))
      (jupyter-eval-buffer (current-buffer)))
    (deactivate-mark))

  (defun bc-jupyter-clear-buffer ()
    "Eshell version of `cls'."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (jupyter-send-input)))

  (advice-add #'jupyter-eval-region :after #'deactivate-mark)
  (advice-add #'jupyter-eval-line-or-region :before #'bc-jupyter--pop-repl)

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
   "G" 'jupyter-repl-forward-cell
   "gg" 'jupyter-repl-backward-cell
   "SPC" nil)

  (:keymaps 'jupyter-repl-mode-map
   :states '(normal visual motion insert)
   :prefix "C-c"
   "C-c" 'jupyter-repl-interrupt-kernel)

  (:keymaps 'jupyter-repl-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "q" 'kill-buffer-and-window))

(provide 'bc-jupyter)
;;; bc-jupyter.el ends here
