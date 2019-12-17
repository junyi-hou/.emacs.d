;;; bc-jupyter.el --- jupyter frontend for REPL -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package jupyter
  :defer t

  :init

  (unless (executable-find "jupyter")
    (user-error "Cannot find jupyter executable, please check jupyter is installed"))

  (defconst bc-jupyter-default-header-args
    '((:async . "yes")
      (:session . "master")))

  (defconst bc-jupyter-available-kernels
    (split-string
     (shell-command-to-string "jupyter kernelspec list | awk 'NR>1 {print $1 }'")
     "\n" 'omit-nulls))

  (with-eval-after-load 'org
    (mapc (lambda (kernel)
            (set (intern (format "org-babel-default-header-args:jupyter-%s" kernel))
                 (append bc-jupyter-default-header-args `((:kernel . ,kernel)))))
          bc-jupyter-available-kernels))

  (with-eval-after-load 'ob-async
    (setq ob-async-no-async-languages-alist
          `(,@(mapcar (lambda (kernel) (format "jupyter-%s" kernel))
                      bc-jupyter-available-kernels))))

  (setq jupyter-repl-echo-eval-p t
        jupyter-repl-maximum-size 12000
        jupyter-repl-history-maximum-length 300)

  ;; hotfix https://github.com/dzop/emacs-jupyter/issues/172
  (jupyter-tramp-file-name-p "~/.bashrc")

  ;; functions

  ;; there is good enough built-in support for remote-kernel
  ;; (i.e., when the file edited is a remote file, start repl in the remote directory)
  ;; so I can delete the home-made patch of starting remote kernel.
  (defun bc-jupyter-start-or-switch-to-repl (kernel)
    "Switch to REPL associated the current buffer.  If there is no REPL associated with the current buffer, start one according to KERNEL type."
    (interactive)
    (if jupyter-current-client
        (condition-case _
            (jupyter-repl-pop-to-buffer)
          (error
           (progn
             (setq-local jupyter-current-client nil)
             (bc-jupyter-start-or-switch-to-repl kernel))))
      (let ((code-buffer (current-buffer)))
        (jupyter-run-repl kernel kernel (current-buffer))
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
   "/" 'evil-search-forward
   "?" 'evil-search-backward
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
