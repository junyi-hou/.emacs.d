;;; gatsby:jupyter.el --- jupyter frontend for REPL -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package jupyter
  :defer t
  :init
  (defconst gatsby:jupyter-default-header-args
    '((:async . "yes")
      (:session . "master")))

  (defconst gatsby:jupyter-available-kernels
    (split-string
     (shell-command-to-string "jupyter kernelspec list | grep -P -o '^\s+([a-z]+|python)'")
     "\n" 'omit-nulls "[[:blank:]]+"))

  (with-eval-after-load 'org
    (mapc (lambda (kernel)
            (set (intern (format "org-babel-default-header-args:jupyter-%s" kernel))
                 (append gatsby:jupyter-default-header-args `((:kernel . ,kernel)))))
          gatsby:jupyter-available-kernels))

  (with-eval-after-load 'ob-async
    (setq ob-async-no-async-languages-alist
          `(,@(mapcar (lambda (kernel) (format "jupyter-%s" kernel))
                      gatsby:jupyter-available-kernels))))

  (unless (executable-find "jupyter")
    (user-error "Cannot find jupyter executable, please check jupyter is installed"))

  (setq jupyter-repl-echo-eval-p t
        jupyter-repl-maximum-size 12000
        jupyter-repl-history-maximum-length 1000)

  ;; functions

  ;; TODO: highlight indent breaks jupyter
  ;; advice jupyter-send-input to remove all highlight-indent related stuff

  ;; there is good enough built-in support for remote-kernel
  ;; (i.e., when the file edited is a remote file, start repl in the remote directory)
  ;; so I can delete the home-made patch of starting remote kernel.
  (defun gatsby:jupyter-start-or-switch-to-repl (kernel)
    "Switch to REPL associated the current buffer.  If there is no REPL associated with the current buffer, start one according to KERNEL type."
    (interactive)
    (if jupyter-current-client
        (condition-case _
            (jupyter-repl-pop-to-buffer)
          (error
           (progn
             (setq-local jupyter-current-client nil)
             (gatsby:jupyter-start-or-switch-to-repl kernel))))
      (let ((code-buffer (current-buffer)))
        (jupyter-run-repl kernel kernel (current-buffer))
        (jupyter-repl-pop-to-buffer)
        (switch-to-buffer-other-window code-buffer))))

  (defun gatsby:jupyter--pop-repl (&rest _)
    "Pop repl buffer, then go back to the code buffer."
    (let* ((code-buffer (current-buffer)))
      (jupyter-repl-pop-to-buffer)
      (switch-to-buffer-other-window code-buffer)))

  (advice-add #'jupyter-eval-line-or-region :before #'gatsby:jupyter--pop-repl)

  (defun gatsby:jupyter-kill-repl ()
    "Kill current repl, save workspace to log folder, reset names of all code buffers associated with it."
    (interactive)
    ;; (gatsby:jupyter-log--save-workspace)
    (kill-buffer-and-window))

  (defun gatsby:jupyter-goto-last-prompt ()
    "Goto current prompt and continue editting."
    (interactive)
    (goto-char (point-max))
    (evil-insert 1))

  (defun gatsby:jupyter-clear-buffer ()
    "Jupyter REPL version of `cls'."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (jupyter-send-input)))

  (defun gatsby:jupyter--deactivate-mark (&rest _)
    "Deactivate mark, use &rest to satisfies the number of arguments"
    (deactivate-mark))

  (advice-add #'jupyter-eval-region :after #'gatsby:jupyter--deactivate-mark)

  :config
  (set-face-extend 'jupyter-repl-traceback t)

  :general
  (:keymaps 'jupyter-repl-mode-map
   :states 'insert
   "<up>" 'jupyter-repl-history-previous-matching
   "<down>" 'jupyter-repl-history-next-matching)

  (:keymaps 'jupyter-repl-mode-map
   :states '(normal visual motion)
   "A" 'gatsby:jupyter-goto-last-prompt
   "/" 'evil-search-forward
   "?" 'evil-search-backward
   "<up>" 'jupyter-repl-backward-cell
   "<down>" 'jupyter-repl-forward-cell
   "SPC" nil)

  (:keymaps 'jupyter-repl-mode-map
   :states '(normal visual motion insert)
   :prefix "C-c"
   "C-c" 'jupyter-repl-interrupt-kernel
   "C-l" 'jupyter-repl-clear-cells)

  (:keymaps 'jupyter-repl-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "q" 'gatsby:jupyter-kill-repl))

(provide 'gatsby:jupyter)
;;; gatsby:jupyter.el ends here
