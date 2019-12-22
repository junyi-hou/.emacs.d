;;; bc-ide-guile.el --- IDE feature for GNU guile -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package geiser
  ;; provide code-completion and documentation
  :hook (scheme-mode . geiser-mode)
  :init
  ;; use guile
  (setq scheme-program-name "guile"
        geiser-default-implementation 'guile
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
          (setq-local bc-guile-repl-buffer repl-buffer)))
      (switch-to-buffer-other-window code-buffer)))

  (defun bc-guile--pop-to-repl ()
    "Switch to `bc-guile-repl-buffer' associated with the current buffer."
    (pop-to-buffer bc-guile-repl-buffer))

  (defun bc-guile-start-or-pop-to-repl ()
    "Pop to `bc-guile-repl-buffer'.  If `bc-guile-repl-buffer' is nil, start a new repl."
    (interactive)
    (if bc-guile-repl-buffer
        (bc-guile--pop-to-repl)
      (bc-guile--start-repl)))

  (defun bc-guile-associate-repl (repl-buffer)
    "Select a geiser repl and associate the current buffer to."
    (interactive
     (list (ivy-read "Choose REPL to associate to"
                     (seq-filter (lambda (bf)
                                   (eq major-mode geiser-repl-mode)
                                   (buffer-list)))
                     :action 'identity)))
    (setq bc-guile-repl-buffer repl-buffer))

  (defun bc-guile--send-code-to-repl (string)
    "Send STRING to the associated buffer for evaluation."
    (if bc-guile-repl-buffer
        (with-current-buffer bc-guile-repl-buffer
          (goto-char (point-max))
          (let ((bol (save-excursion
                       (comint-bol))))
            (delete-region bol (point-max)))
          (insert string)
          (geiser-repl--maybe-send))
      (user-error "Associated geiser REPL not found")))

  (defun bc-guile--eval-region (beg end)
    (bc-guile--send-code-to-repl (buffer-substring-no-properties beg end))
    (deactivate-mark))

  (defun bc-guile--eval-last-sexp ()
    "Evaluate the last sexp before point."
    ;; HACK: depend on evil
    (save-excursion
      (let ((end (if (looking-at "[\])]")
                     (1+ (point))
                   (1+ (re-search-backward "[\])]"))))
            (beg (1- (evil-jump-item))))
        (bc-guile--eval-region beg end))))

  (defun bc-guile-eval-sexp-or-region ()
    (interactive)
    (if (region-active-p)
        (bc-guile--eval-region (region-beginning) (region-end))
      (bc-guile--eval-last-sexp)))

  (defun bc-guile--reset-repl-buffer (&rest _)
    "When quitting repl buffer, reset `bc-guile-repl-buffer' for all associated code buffers."
    (let ((repl-buffer (current-buffer)))
      (mapc (lambda (bf)
              (with-current-buffer bf
                (setq-local bc-guile-repl-buffer nil)))
            (seq-filter (lambda (bf)
                          (with-current-buffer bf
                            (eq bc-guile-repl-buffer repl-buffer)))
                        (buffer-list)))))

  (advice-add 'geiser-repl-exit :before 'bc-guile--reset-repl-buffer)
  (advice-add 'geiser-repl-exit :after 'delete-window)

  :general

  (:keymaps 'geiser-repl-mode-map
   :states 'insert
   "<return>" 'geiser-repl--maybe-send
   "<up>" 'comint-previous-matching-input-from-input
   "<down>" 'comint-next-matching-input-from-input)

  (:keymaps 'geiser-repl-mode-map
   :state '(normal motion visual)
   "<up>" 'comint-previous-prompt
   "<down>" 'comint-next-prompt
   "A" (lambda () (interactive)
         (goto-char (point-max))
         (evil-insert-state))
   "H" 'geiser-repl--bol
   "SPC" 'nil)

  (:keymaps 'geiser-repl-mode-map
   :states '(normal motion visual insert emacs)
   :prefix "C-c"
   "C-c" 'geiser-repl-interrupt)

  ;; FIXME - this is a hack - why is geiser-repl-mode-map covering
  ;; my global mappings?
  (:keymaps 'geiser-repl-mode-map
   :states '(normal motion visual insert emacs)
   "C-j" 'evil-window-down
   "C-d" 'evil-scroll-down)

  (:keymaps 'geiser-repl-mode-map
   :state '(normal motion visual)
   :prefix "SPC"
   "q" 'geiser-repl-exit)

  (:keymaps 'scheme-mode-map
   :states '(motion normal visual)
   :prefix "SPC"
   "rr" 'bc-guile-eval-sexp-or-region
   "rh" 'geiser-doc-symbol-at-point
   "rz" 'bc-guile-associate-repl
   "ro" 'bc-guile-start-or-pop-to-repl))

(provide 'bc-ide-guile)
;;; bc-ide-guile.el ends here
