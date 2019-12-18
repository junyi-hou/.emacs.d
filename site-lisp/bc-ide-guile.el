;;; bc-ide-guile.el --- IDE feature for GNU guile -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package geiser
  ;; provide code-completion and documentation
  :hook
  (scheme-mode . bc-guile--init)
  :init
  (require 'bc-jupyter)

  ;; use guile
  (setq scheme-program-name "guile"
        geiser-default-implementation 'guile)

  (setq-local tab-width 2)

  (defalias 'bc-guile-local-repl
    (lambda () (interactive)
      (bc-jupyter-start-or-switch-to-repl "guile"))
    "Open a jupyter repl for python interpreter.")

  (defalias 'bc-guile-reconnect
    #'jupyter-repl-restart-kernel
    "Reconnect to the current REPL.")

  ;; when eval, send it to both the geiser REPL (for autocompletion)
  ;; and jupyter REPL

  (defun bc-guile--eval-region (beg end)
    "Evaluate region between BEG and END."
    (geiser-eval-region beg end nil nil 'nomsg)
    (jupyter-eval-region beg end))

  (defun bc-guile--eval-last-sexp ()
    "Evaluate the last sexp before point."
    ;; HACK: depend on evil
    (save-excursion
      (let* ((end (1+ (re-search-backward "[\])]")))
             (beg (1- (evil-jump-item))))
        (bc-guile--eval-region beg end))))

  (defun bc-guile-eval-sexp-or-region ()
    (interactive)
    (if (region-active-p)
        (bc-guile--eval-region (region-beginning) (region-end))
      (bc-guile--eval-last-sexp)))

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
    (unless (string-match-p "jupyter-repl" (buffer-name))
      (geiser-mode)
      ;; HACK: the REPL fire up too quick I am still in the last buffer
      (run-at-time 0.5 nil #'run-guile)))

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
   "rr" 'bc-guile-eval-sexp-or-region
   "rh" 'geiser-doc-symbol-at-point

   "ro" 'bc-guile-local-repl
   "rz" 'jupyter-repl-associate-buffer
   "rZ" 'bc-guile-reconnect))

(provide 'bc-ide-guile)
;;; bc-ide-guile.el ends here
