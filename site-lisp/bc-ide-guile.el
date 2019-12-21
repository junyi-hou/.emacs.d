;;; bc-ide-guile.el --- IDE feature for GNU guile -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package geiser
  ;; provide code-completion and documentation
  :hook
  (scheme-mode . bc-guile--init)
  :init
  ;; use guile
  (setq scheme-program-name "guile"
        geiser-default-implementation 'guile
        ;; unmap everything in geiser-mode-map
        geiser-repl-mode-map (make-sparse-keymap))

  (setq-local tab-width 2)

  ;;; ===============================
  ;;  separable repl
  ;;; ===============================

  (defvar-local bc-guile-repl-buffer nil
    "The geiser repl associated to the current buffer.")

  (defun bc-guile--start-repl ()
    "Start a geiser guile buffer."
    (let ((code-buffer (current-buffer)))
      (run-geiser geiser-default-implementation)
      (let ((repl-buffer (current-buffer)))
        (with-current-buffer code-buffer
          (setq bc-guile-repl-buffer repl-buffer)))
      (switch-to-buffer code-buffer)))

  (defun bc-guile--pop-to-repl ()
    "Switch to `bc-guile-repl-buffer' associated with the current buffer."
    (pop-to-buffer bc-guile-repl-buffer))

  (defun bc-guile-start-or-pop-to-repl ()
    "Pop to `bc-guile-repl-buffer'.  If `bc-guile-repl-buffer' is nil, start a new repl."
    (if bc-guile-repl-buffer
        (bc-guile--switch-to-repl)
      (bc-guile--pop-to-repl)))

  (defun bc-guile-associate-repl (repl-buffer)
    "Select a geiser repl and associate the current buffer to."
    (interactive
     (list (ivy-read "Choose REPL to associate to"
                     (seq-filter (lambda (bf)
                                   (eq major-mode geiser-repl-mode)
                                   (buffer-list)))
                     :action 'identity)))
    (setq bc-guile-repl-buffer repl-buffer))

  (defun bc-guile-eval-sexp-or-region ()
    (interactive)
    (if (region-active-p)
        (geiser-eval-region (region-beginning) (region-end) nil nil 'nomsg)
      (geiser-eval-last-sexp)))
  

  :general
  (:states '(motion normal visual)
   :keymaps 'scheme-mode-map
   :prefix "SPC"
   "rr" 'bc-guile-eval-sexp-or-region
   "rh" 'geiser-doc-symbol-at-point
   "rz" 'bc-guile-associate-repl
   "ro" 'bc-guile-start-or-pop-to-repl)

  (:states '(motion normal visual insert)
   :keymaps 'geiser-repl-mode-map
   "<up>" 'comint-previous-matching-input-from-input
   "<down>" 'comint-next-matching-input-from-input
   "<return>" 'geiser-repl--maybe-send
   "C-y" 'comint-previous-prompt
   "C-e" 'comint-next-prompt)

  (:keymaps 'geiser-repl-mode-map
   :states '(normal visual motion insert)
   :prefix "C-c"
   "C-c" 'geiser-repl-interrupt)

  (:keymaps 'geiser-repl-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "q" 'geiser-repl-exit)

  (:keymaps 'geiser-repl-mode-map
   :states '(normal visual motion)
   "A" (lambda () (interactive)
         (goto-char (point-max))
         (evil-insert 1))
   "SPC" nil))

(provide 'bc-ide-guile)
;;; bc-ide-guile.el ends here
