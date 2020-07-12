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

  (defun gatsby:selectrum--remove-until-slash (bound n)
    "Return the position of the backwards Nth slash until BOUND.
If no slash was found, return BOUND."
    (save-excursion
      (if-let ((found (search-backward "/" bound 'noerror n)))
          (1+ found)
        bound)))

  (defun gatsby:selectrum-better-backspace ()
    "If `point' is at \"/\", delete till the last \"/\"."
    (interactive)
    (cond ((thing-at-point-looking-at "~/")
           (progn
             (delete-region (minibuffer-prompt-end) (point))
             (insert "/home/")))
          ((string= (buffer-substring (minibuffer-prompt-end) (point)) "/")
           (call-interactively #'backward-delete-char))
          ((thing-at-point-looking-at "/")
           (delete-region (gatsby:selectrum--remove-until-slash
                           selectrum--start-of-input-marker 2)
                          (point)))
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

  (defun gatsby:selectrum--combine (input common)
    "Combine INPUT with COMMON that are prescient regexp conscious.
Example: input = \"/home/gat sel\"
        common = \"gatsby:selectrum.el\"
        output = \"/home/gatsby:selectrum.el\"
This function takes advantage of `selectrum-prescient--highlight' which highlights the matching part of COMMON in INPUT. To combine those two, I can \"fill the blanks\" in INPUT by getting un-highlighted parts of COMMON"
    (let* ((input "/home/gat sel")
           (common (buffer-name))
           ;; COMMON does not have highlight information, so get it from the first
           ;; candidate
           (common (s-shared-start
                    (buffer-substring (line-beginning-position 2)
                                      (line-end-position 2))
                    common)))
      (let (out
            (in (split-string input " " 'omit-nulls)))
        in))
    ;; now fill in the blanks
    )

  (defun gatsby:selectrum-unified-tab ()
    "TODO: incorporate `gatsby:selectrum--combine'."
    (interactive)
    (when selectrum--current-candidate-index
      (let* ((candidate (selectrum--get-full (nth selectrum--current-candidate-index
                                                  selectrum--refined-candidates)))
             (input (buffer-substring-no-properties
                     selectrum--start-of-input-marker
                     selectrum--end-of-input-marker)))
        (if (string= input candidate)
            (selectrum-select-current-candidate)
          (when-let* ((common (try-completion "" selectrum--refined-candidates))
                      (common (or (and (not (string= "" common)) common)
                                  (and (eq this-command last-command) candidate))))
            (delete-region (gatsby:selectrum--remove-until-slash
                            selectrum--start-of-input-marker 1)
                           (point))
            (insert common))))))
  
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
  (selectrum-prescient-mode 1)

  )

(provide 'gatsby:selectrum)
;;; gatsby:selectrum.el ends here
