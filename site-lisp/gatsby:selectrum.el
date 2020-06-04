;;; gatsby:selectrum.el --- config for selecturm -*- lexical-binding: t; -*-

;;; Commentary:



;;; Code:

(require 'gatsby:core)

(use-package selectrum
  :init
  (selectrum-mode 1)

  (defvar-local gatsby:selectrum-jump-history nil
    "History of `selectrum-jump-to-hs-header'.")

  (defun gatsby:selectrum-jump-to-hs-header ()
    "Provide a list of `hideshow' headers from which one can jump to."
    (interactive)
    (unless hs-block-start-regexp
      (user-error "cannot find `hs-block-start-regexp', enable `hs-minor-mode' first."))
    (let* ((selectrum-should-sort-p nil)
           (headings
            (cl-loop with buffer-text-lines = (split-string (buffer-string) "\n")
                     for txt in buffer-text-lines
                     for num from 1 to (1- (length buffer-text-lines))
                     ;; HACK:
                     ;; only include `hideshow' headers, identified using
                     ;; `hs-block-start-regexp'
                     ;; also exclude any line in comments or in docs or in string
                     when (and (string-match hs-block-start-regexp txt)
                               (not (seq-some (lambda (p)
                                                (or (eq p font-lock-doc-face)
                                                    (eq p font-lock-comment-face)
                                                    (eq p font-lock-string-face)))
                                              (text-properties-at (- (length txt) 2) txt))))
                     collect (propertize (concat (number-to-string num)
                                                 " "
                                                 txt
                                                 " ...")
                                         'line-num num)))
           (chosen-heading (completing-read "Jump to: "
                                            headings nil t nil
                                            'gatsby:selectrum-jump-history))
           (jump-position (get-text-property 0 'line-num chosen-heading)))
      (goto-char (point-min))
      (forward-line jump-position)))

  (defun gatsby:selectrum-better-backspace ()
    "If `point' is at \"/\", delete till the last \"/\"."
    (interactive)
    (if (thing-at-point-looking-at "/")
        (let* ((delete-untill (save-excursion
                                (1+ (re-search-backward "/" nil 'noerror 2)))))
          (delete-region delete-untill (point)))
      (call-interactively #'backward-delete-char)))

  :config
  (setq selectrum-minibuffer-bindings
        (append selectrum-minibuffer-bindings
                '(("M-j" . selectrum-next-candidate)
                  ("M-k" . selectrum-previous-candidate)
                  ("<backspace>" . gatsby:selectrum-better-backspace))))

  :general
  (:keymaps '(motion normal visual emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "oo" 'find-file
   "or" (lambda () (interactive)
          (let ((selectrum-should-sort-p nil))
            (find-file (completing-read "Recent file: "
                                        (mapcar #'abbreviate-file-name recentf-list)
                                        nil t))))
   "ob" 'switch-to-buffer
   "om" (lambda () (interactive)
          (switch-to-buffer-other-window (get-buffer-create "*Messages*")))
   "oj" 'gatsby:selectrum-jump-to-hs-header))

(provide 'gatsby:selectrum)
;;; gatsby:selectrum.el ends here
