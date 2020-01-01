;;; bc-comint.el --- init comint for REPLs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package comint
  :defer t
  :straight (:type built-in)
  :init
  (defun bc-comint-goto-last-prompt ()
    "Goto current prompt and continue editting."
    (interactive)
    (goto-char (point-max))
    (evil-insert 1))

  (defun bc-comint-cls ()
    "clear current REPL buffer."
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))

  (defun bc-comit--move-to-eol (&rest _)
    (end-of-line))

  (advice-add 'comint-previous-matching-input-from-input :after 'bc-comit--move-to-eol)
  (advice-add 'comint-next-matching-input-from-input :after 'bc-comit--move-to-eol)

  ;;; ===============================
  ;;  REPL settings
  ;;; ===============================

  (defvar-local bc-comint-repl-buffer nil
    "The geiser repl associated to the current buffer.")

  (defun bc-comint--start-repl (repl-fn &rest args)
    "Start a REPL buffer using REPL-FN with ARGS."
    (let ((code-buffer (current-buffer)))
      (apply repl-fn args)
      (let ((repl-buffer (current-buffer)))
        (with-current-buffer code-buffer
          (setq-local bc-comint-repl-buffer repl-buffer)))
      (switch-to-buffer-other-window code-buffer)))

  (defun bc-comint--pop-to-repl ()
    "Switch to `bc-comint-repl-buffer' associated with the current buffer."
    (pop-to-buffer bc-comint-repl-buffer))

  (defun bc-comint-associate-repl (repl-mode)
    "Select a repl of mode REPL-MODE and associate the current buffer to that repl."
    (let ((repl-buffer (ivy-read "Choose REPL to associate to: "
                                 (mapcar 'buffer-name
                                         (seq-filter (lambda (bf)
                                                       (with-current-buffer bf
                                                         (eq major-mode repl-mode)))
                                                     (buffer-list)))
                                 :action 'identity)))
      (setq bc-comint-repl-buffer repl-buffer)))

  (defun bc-comint--send-code-to-repl (send-fn string)
    "Send STRING using SEND-FN to the associated buffer for evaluation."
    (if bc-comint-repl-buffer
        (with-current-buffer bc-comint-repl-buffer
          (goto-char (point-max))
          (let ((bol (save-excursion
                       (comint-bol))))
            (delete-region bol (point-max)))
          (insert string)
          (funcall send-fn))
      (user-error "Associated REPL not found")))

  (defun bc-comint--eval-region (send-fn beg end)
    (bc-comint--send-code-to-repl send-fn (buffer-substring-no-properties beg end))
    (deactivate-mark))

  (defun bc-comint--eval-last-sexp (send-fn)
    "Evaluate the last sexp before point."
    ;; HACK: depend on evil
    (save-excursion
      (let* ((end (if (looking-at "[\])]")
                      (1+ (point))
                    (1+ (re-search-backward "[\])]"))))
             (beg (evil-jump-item)))
        (bc-comint--eval-region send-fn beg end))))

  (defun bc-comint-exit-repl ()
    "When quitting repl buffer, reset `bc-comint-repl-buffer' for all associated code buffers."
    (interactive)
    (when-let ((_ (y-or-n-p "Really quit this REPL? "))
               (repl-buffer (current-buffer)))
      (mapc (lambda (bf)
              (with-current-buffer bf
                (setq-local bc-comint-repl-buffer nil)))
            (seq-filter (lambda (bf)
                          (with-current-buffer bf
                            (eq bc-comint-repl-buffer repl-buffer)))
                        (buffer-list)))
      (kill-buffer)
      (delete-window)))

  :general
  (:keymaps 'comint-mode-map
   :states '(normal visual motion emacs insert)
   :prefix "C-c"
   "C-l" 'bc-comint-cls
   "C-c" 'comint-interrupt-subjob)

  (:keymaps 'comint-mode-map
   :states 'insert
   "<up>" 'comint-previous-matching-input-from-input
   "<down>" 'comint-next-matching-input-from-input)

  (:keymaps 'comint-mode-map
   :states '(normal motion visual insert emacs)
   "C-j" 'evil-window-down
   "C-d" 'evil-scroll-down)

  (:keymaps 'comint-mode-map
   :states '(normal motion visual)
   :prefix "SPC"
   "q" 'bc-comint-exit-repl)

  (:keymaps 'comint-mode-map
   :states '(normal motion visual)
   "<up>" 'comint-previous-prompt
   "<down>" 'comint-next-prompt
   "A" 'bc-comint-goto-last-prompt
   "H" 'comint-bol
   "SPC" 'nil))

(provide 'bc-comint)
;;; bc-comint.el ends here
