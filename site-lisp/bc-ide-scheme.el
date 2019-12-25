;;; bc-ide-scheme.el --- IDE feature for GNU guile -*- lexical-binding: t; -*-

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

  (defvar-local bc-scheme-repl-buffer nil
    "The geiser repl associated to the current buffer.")

  (defun bc-scheme--start-repl ()
    "Start a geiser guile buffer."
    (let ((code-buffer (current-buffer)))
      (run-geiser geiser-default-implementation)
      (let ((repl-buffer (current-buffer)))
        (with-current-buffer code-buffer
          (setq-local bc-scheme-repl-buffer repl-buffer)))
      (switch-to-buffer-other-window code-buffer)))

  (defun bc-scheme--pop-to-repl ()
    "Switch to `bc-scheme-repl-buffer' associated with the current buffer."
    (pop-to-buffer bc-scheme-repl-buffer))

  (defun bc-scheme-start-or-pop-to-repl ()
    "Pop to `bc-scheme-repl-buffer'.  If `bc-scheme-repl-buffer' is nil, start a new repl."
    (interactive)
    (if bc-scheme-repl-buffer
        (bc-scheme--pop-to-repl)
      (bc-scheme--start-repl)))

  (defun bc-scheme-associate-repl (repl-buffer)
    "Select a geiser repl and associate the current buffer to."
    (interactive
     (list (ivy-read "Choose REPL to associate to"
                     (seq-filter (lambda (bf)
                                   (eq major-mode geiser-repl-mode)
                                   (buffer-list)))
                     :action 'identity)))
    (setq bc-scheme-repl-buffer repl-buffer))

  (defun bc-scheme--send-code-to-repl (string)
    "Send STRING to the associated buffer for evaluation."
    (if bc-scheme-repl-buffer
        (with-current-buffer bc-scheme-repl-buffer
          (goto-char (point-max))
          (let ((bol (save-excursion
                       (comint-bol))))
            (delete-region bol (point-max)))
          (insert string)
          (geiser-repl--maybe-send))
      (user-error "Associated geiser REPL not found")))

  (defun bc-scheme--eval-region (beg end)
    (bc-scheme--send-code-to-repl (buffer-substring-no-properties beg end))
    (deactivate-mark))

  (defun bc-scheme--eval-last-sexp ()
    "Evaluate the last sexp before point."
    ;; HACK: depend on evil
    (save-excursion
      (let* ((end (if (looking-at "[\])]")
                      (1+ (point))
                    (1+ (re-search-backward "[\])]"))))
             (beg (evil-jump-item)))
        (bc-scheme--eval-region beg end))))

  (defun bc-scheme-eval-sexp-or-region ()
    (interactive)
    (if (region-active-p)
        (bc-scheme--eval-region (region-beginning) (region-end))
      (bc-scheme--eval-last-sexp)))

  (defun bc-scheme--exit-repl (geiser-quit &rest args)
    "When quitting repl buffer, reset `bc-scheme-repl-buffer' for all associated code buffers."
    (when-let ((_ (y-or-n-p "Really quit this REPL? "))
               (repl-buffer (current-buffer)))
      (mapc (lambda (bf)
              (with-current-buffer bf
                (setq-local bc-scheme-repl-buffer nil)))
            (seq-filter (lambda (bf)
                          (with-current-buffer bf
                            (eq bc-scheme-repl-buffer repl-buffer)))
                        (buffer-list)))
      (funcall geiser-quit args)
      (delete-window)))

  (advice-add 'geiser-repl-exit :around 'bc-scheme--exit-repl)

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
   "A" 'bc-comint-goto-last-prompt
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
   "rr" 'bc-scheme-eval-sexp-or-region
   "rh" 'geiser-doc-symbol-at-point
   "rz" 'bc-scheme-associate-repl
   "ro" 'bc-scheme-start-or-pop-to-repl))

(provide 'bc-ide-scheme)
;;; bc-ide-scheme.el ends here
