;;; gatsby:selectrum.el --- config for selecturm -*- lexical-binding: t; -*-

;;; Commentary:



;;; Code:

(require 'gatsby:core)

(use-package selectrum
  :defines (selectrum-minibuffer-bindings selectrum-should-sort-p)
  :init
  (selectrum-mode 1)

  (defun gatsby:selectrum-jump-to-hs-header ()
    "Provide a list of `hideshow' headers from which one can jump to."
    (interactive)
    (unless hs-block-start-regexp
      (user-error "Cannot find `hs-block-start-regexp', enable `hs-minor-mode' first"))
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
      (forward-line jump-position)
      (when (memq 'hs-minor-mode minor-mode-list)
        (save-excursion
          (hs-show-block)))
      (call-interactively #'recenter)))

  (defun gatsby:selectrum-better-backspace ()
    "If `point' is at \"/\", delete till the last \"/\"."
    (interactive)
    (cond ((thing-at-point-looking-at "~/")
           (progn
             (delete-region (minibuffer-prompt-end) (point))
             (insert "/home/")))
          ((thing-at-point-looking-at "/")
           (let* ((last-slash-pos (max
                                   (or (save-excursion
                                         (1+ (re-search-backward "/" nil 'noerror 2)))
                                       0)
                                   selectrum--start-of-input-marker)))
             (delete-region last-slash-pos (point))))
          (t (call-interactively #'backward-delete-char))))

  (defun gatsby:selectrum-next-candidate-cycle ()
    "Move selection to next candidate, if at the end, go to the top."
    (interactive)
    (when selectrum--current-candidate-index
      (setq selectrum--current-candidate-index
            (if (= selectrum--current-candidate-index
                   (1- (length selectrum--refined-candidates)))
                (if selectrum--match-required-p 0 -1)
              (1+ selectrum--current-candidate-index)))))

  (defun gatsby:selectrum-previous-candidate-cycle ()
    "Move selection to previous candidate, if at the beginning, go to the end."
    (interactive)
    (when selectrum--current-candidate-index
      (setq selectrum--current-candidate-index
            (if (= selectrum--current-candidate-index
                   (if selectrum--match-required-p 0 -1))
                (1- (length selectrum--refined-candidates))
              (1- selectrum--current-candidate-index)))))

  (defun gatsby:selectrum-unified-tab (&optional arg)
    "Tab now does the following things:
1. if the minibuffer input perfectly matches the selected candidate, select the candidate and exit (i.e., <tab> works like <enter> in this case.)
2. if not, tab tries to complete the common prefix of all candidates. Do nothing if there is no common prefix.
3. if there is no common prefix, double tab puts the selected candidate to the minibuffer input."
    (interactive "P")
    (when selectrum--current-candidate-index
      (let* ((candidate (selectrum--get-full (nth selectrum--current-candidate-index
                                                  selectrum--refined-candidates)))
             (user-input (buffer-substring-no-properties
                          selectrum--start-of-input-marker
                          selectrum--end-of-input-marker)))
        (if (string= user-input candidate)
            (selectrum-select-current-candidate arg)
          (let ((common (try-completion "" selectrum--refined-candidates)))
            (if (and (string= common "")
                     (eq last-command this-command))
                (selectrum-insert-current-candidate)
              (delete-region selectrum--start-of-input-marker
                             selectrum--end-of-input-marker)
              (insert common)))))))

  :config
  (setq selectrum-minibuffer-bindings
        (append selectrum-minibuffer-bindings
                '(("M-j" . gatsby:selectrum-next-candidate-cycle)
                  ("M-k" . gatsby:selectrum-previous-candidate-cycle)
                  ("<backspace>" . gatsby:selectrum-better-backspace)
                  ("TAB" . gatsby:selectrum-unified-tab))))

  :custom
  (selectrum-fix-minibuffer-height t)

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

(use-package selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode))

(provide 'gatsby:selectrum)
;;; gatsby:selectrum.el ends here
