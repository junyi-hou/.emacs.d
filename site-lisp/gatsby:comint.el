;;; gatsby:comint.el --- init comint for REPLs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package comint
  :defer t
  :straight (:type built-in)
  :init
  (defun gatsby:comint-goto-last-prompt ()
    "Goto current prompt and continue editting."
    (interactive)
    (goto-char (point-max))
    (evil-insert 1))

  (defun gatsby:comint-cls ()
    "clear current REPL buffer."
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))

  (defun gatsby:comit--move-to-eol (&rest _)
    (goto-char (point-max)))

  (advice-add 'comint-previous-matching-input-from-input :after 'gatsby:comit--move-to-eol)
  (advice-add 'comint-next-matching-input-from-input :after 'gatsby:comit--move-to-eol)

  ;;; ===============================
  ;;  REPL settings
  ;;; ===============================

  (defvar-local gatsby:comint-repl-buffer nil
    "The repl associated to the current buffer.")

  (defun gatsby:comint--start-repl (repl-fn &rest args)
    "Start a REPL buffer using REPL-FN with ARGS."
    (let ((code-buffer (current-buffer)))
      (apply repl-fn args)
      (let ((repl-buffer (current-buffer)))
        (with-current-buffer code-buffer
          (setq-local gatsby:comint-repl-buffer repl-buffer)))
      (switch-to-buffer-other-window code-buffer)))

  (defun gatsby:comint--pop-to-repl ()
    "Switch to `gatsby:comint-repl-buffer' associated with the current buffer."
    (pop-to-buffer gatsby:comint-repl-buffer))

  (defun gatsby:comint-associate-repl (repl-mode)
    "Select a repl of mode REPL-MODE and associate the current buffer to that repl."
    (let ((repl-buffer-name (ivy-read
                             "Choose REPL to associate to: "
                             #'internal-complete-buffer
                             :predicate (lambda (bf)
                                          (with-current-buffer (cdr bf)
                                            (eq major-mode repl-mode)))
                             :action 'identity)))
      (setq gatsby:comint-repl-buffer (get-buffer repl-buffer-name))))

  (defun gatsby:comint--send-code-to-repl (send-fn string)
    "Send STRING using SEND-FN to the associated buffer for evaluation."
    (if gatsby:comint-repl-buffer
        (with-current-buffer gatsby:comint-repl-buffer
          (goto-char (point-max))
          (let ((bol (save-excursion
                       (comint-bol))))
            (delete-region bol (point-max)))
          (insert string)
          (funcall send-fn))
      (user-error "Associated REPL not found")))

  (defun gatsby:comint--eval-region (send-fn beg end)
    (gatsby:comint--send-code-to-repl send-fn (buffer-substring-no-properties beg end))
    (deactivate-mark))

  (defun gatsby:comint--eval-last-sexp (send-fn)
    "Evaluate the last sexp before point."
    ;; HACK: depend on evil
    (save-excursion
      (let* ((end (if (looking-at "[\])]")
                      (1+ (point))
                    (1+ (re-search-backward "[\])]"))))
             (beg (evil-jump-item)))
        (gatsby:comint--eval-region send-fn beg end))))

  (defun gatsby:comint-exit-repl ()
    "When quitting repl buffer, reset `gatsby:comint-repl-buffer' for all associated code buffers."
    (interactive)
    (when-let ((_ (y-or-n-p "Really quit this REPL? "))
               (repl-buffer (current-buffer)))
      (mapc (lambda (bf)
              (with-current-buffer bf
                (setq-local gatsby:comint-repl-buffer nil)))
            (seq-filter (lambda (bf)
                          (with-current-buffer bf
                            (eq gatsby:comint-repl-buffer repl-buffer)))
                        (buffer-list)))
      (kill-buffer)
      (delete-window)))

  :general
  (:keymaps 'comint-mode-map
   :states '(normal visual motion emacs insert)
   :prefix "C-c"
   "C-l" 'gatsby:comint-cls
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
   "q" 'gatsby:comint-exit-repl)

  (:keymaps 'comint-mode-map
   :states '(normal motion visual)
   "<up>" 'comint-previous-prompt
   "<down>" 'comint-next-prompt
   "A" 'gatsby:comint-goto-last-prompt
   "H" 'comint-bol
   "SPC" 'nil))

(provide 'gatsby:comint)
;;; gatsby:comint.el ends here
