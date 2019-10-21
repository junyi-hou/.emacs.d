;;; bc-ide-guile.el --- IDE feature for GNU guile -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package geiser
  ;; provide code-completion and documentation
  :hook
  (scheme-mode . bc-guile--init)
  :init

  (require 'bc-jupyter)

  (defalias 'bc-guile-local-repl
    (lambda () (interactive)
      (bc-jupyter-start-or-switch-to-repl "guile"))
    "Open a jupyter repl for python interpreter.")

  (defalias 'bc-guile-reconnect
    #'jupyter-repl-restart-kernel
    "Reconnect to the current REPL.")

  (defun bc-guile--repl-buffer-name (&rest _)
    "Return the repl buffer name for the current buffer."
    (format "*REPL-%s*" (buffer-name)))

  (setq geiser-repl-buffer-name-function #'bc-guile--repl-buffer-name)

  (defun bc-guile--advicing-geiser-repl (geiser-cmd &rest _)
    "1. Open geiser REPL only if there is no one attached to the current buffer;
     2. automatically delete window after REPL is called."
    (unless (get-buffer (bc-guile--repl-buffer-name))
      (funcall geiser-cmd)
      (delete-window)))

  (advice-add 'run-guile :around #'bc-guile--advicing-geiser-repl)

  (defun bc-guile--init ()
    "Init guile help and autocompletion."
    (geiser-mode)
    (run-at-time 0.01 nil #'run-guile))

  (defun bc-guile--quit-repl ()
    "Quit the corresponding geiser REPL associated with BUFFER-OR-NAME"
    (when (and (eq major-mode 'scheme-mode)
               (memq (get-buffer (bc-guile--repl-buffer-name)) (buffer-list)))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) t)))
        (with-current-buffer (bc-guile--repl-buffer-name)
          (kill-buffer-and-window)))))

  (add-hook 'kill-buffer-hook #'bc-guile--quit-repl)
  

  :general
  (:states '(motion normal visual)
   :keymaps 'scheme-mode-map
   :prefix "SPC"
   "rb" 'jupyter-eval-buffer
   "rf" 'jupyter-eval-defun
   "rr" 'jupyter-eval-line-or-region
   "rh" 'geiser-doc-symbol-at-point

   "ro" 'bc-guile-local-repl
   "rz" 'jupyter-repl-associate-buffer
   "rZ" 'bc-guile-reconnect))

(provide 'bc-ide-guile)
;;; bc-ide-guile.el ends here
