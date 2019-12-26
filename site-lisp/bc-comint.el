;;; bc-comint.el --- init comint for REPLs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package comint
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
    (let ((inhibit-read-only t))
      (erase-buffer)
      (comint-send-input)))

  (defun bc-comit--move-to-eol (&rest _)
    (end-of-line))

  (advice-add 'comint-previous-matching-input-from-input :after 'bc-comit--move-to-eol)
  (advice-add 'comint-next-matching-input-from-input :after 'bc-comit--move-to-eol)

  :general
  (:keymaps 'comint-mode-map
   :states '(normal visual motion emacs insert)
   :prefix "C-c"
   "C-k" 'bc-comit-cls))

(provide 'bc-comint)
;;; bc-comint.el ends here
