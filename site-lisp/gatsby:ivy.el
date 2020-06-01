;;; gatsby:ivy.el --- config for ivy -*- lexical-binding: t; -*-

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
    (let* ((selectrum-should-sort-p nil)
           (headings
            (cl-loop with buffer-text-lines = (split-string (buffer-string) "\n")
                     for txt in buffer-text-lines
                     for num from 1 to (1- (length buffer-text-lines))
                     ;; Only get the heading lines.
                     when (and (string-match hs-block-start-regexp txt)
                               (not (seq-some (lambda (p) (eq p font-lock-doc-face))
                                              (text-properties-at (- (length txt) 2) txt))))
                     ;; Heading text, Outline level, Line number
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

  :config
  (setq selectrum-minibuffer-bindings
        (append selectrum-minibuffer-bindings
                '(("M-j" . selectrum-next-candidate)
                  ("M-k" . selectrum-previous-candidate))))
  :general
  (:keymaps '(motion normal visual emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "oo" 'find-file
   "or" (lambda () (interactive)
          (find-file (completing-read "Recent file: "
                                      (mapcar #'abbreviate-file-name recentf-list)
                                      nil t)))
   "ob" 'switch-to-buffer
   "om" (lambda () (interactive)
          (switch-to-buffer-other-window (get-buffer-create "*Messages*")))
   "oj" 'gatsby:selectrum-jump-to-hs-header))

;; (use-package selectrum-prescient)

(provide 'gatsby:ivy)
;;; gatsby:ivy.el ends here
