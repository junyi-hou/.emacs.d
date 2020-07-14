;;; gatsby:completion.el --- init company-mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby:core)

(use-package prescient
  :init
  (setq prescient-save-file (concat no-littering-var-directory "prescient-save.el"))
  :config
  (prescient-persist-mode 1))

(use-package yasnippet
  :commands (yas-minor-mode yas-reload-all)
  :hook
  (company-mode . yas-reload-all)
  (company-mode . yas-minor-mode)
  :init
  (defun gatsby:completion-better-yas-backspace ()
    "If `point' is at the beginning of an unmodified yas-field, delete the field, otherwise backwards delete char."
    (interactive)
    (cond ((yas--maybe-clear-field-filter t)
           (yas--skip-and-clear (yas-current-field)))
          (t (call-interactively #'backward-delete-char-untabify))))

  :general
  (:keymaps 'yas-keymap
   "<tab>" nil
   "TAB" nil
   "C-n" 'yas-next-field
   "C-p" 'yas-prev-field
   "<backspace>" 'gatsby:completion-better-yas-backspace)

  ;; kill M-j/M-k so they won't show up when snippets are active
  (:keymaps 'global-map
   "M-j" nil
   "M-k" nil))

(use-package company
  :hook
  (prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  (jupyter-repl-mode . company-mode)
  (eshell-mode . company-mode)
  (org-mode . company-mode)

  :custom
  (company-idle-delay nil)
  (company-require-match 'never)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-code-other-buffers t)
  (company-show-numbers t)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  (company-backends '((company-capf)
                      company-yasnippet
                      company-files)
                    (company-dabbrev-code
                     company-dabbrev
                     company-abbrev))

  :init
  (defun gatsby:completion-unified-tab ()
    "Use tab for both company and indentation.

In insert mode, first try `company-manual-begin'.  If there is no completion available at point, indent the current line by `tab-width' length."
    (interactive)
    (if (looking-back "^[ \t]*" (line-beginning-position))
        (dotimes (n tab-width) (insert " "))
      (yas-expand)
      (company-manual-begin)
      ;; manually call `company-manual-begin' will set
      ;; `company-minimum-prefix-length' to 0, which means that the snippets
      ;; will always get included. To fix this add a condition that if
      ;; all candidates are snippets, cancel auto completion and indent region.
      (if (null (seq-every-p
                 (lambda (candidate)
                   (member candidate (yas-active-keys)))
                 company-candidates))
          (if (or company-selection-changed
                  (memq last-command `(company-complete-common
                                       ,this-command)))
              (call-interactively 'company-complete-selection)
            (call-interactively 'company-complete-common)
            (when company-candidates
              (setq this-command 'company-complete-common)))
        (company-cancel)
        (save-excursion
          (indent-region (line-beginning-position) (line-end-position))))))

  :general
  (:keymaps 'insert
   "<tab>" 'gatsby:completion-unified-tab)

  (:keymaps 'company-active-map
   "<tab>" 'gatsby:completion-unified-tab
   "M-j" 'company-select-next
   "M-k" 'company-select-previous-or-abort
   "C-g" 'company-abort))

(use-package company-prescient
  :hook
  (company-mode . company-prescient-mode))

(use-package company-posframe
  :after company
  :hook (company-mode . company-posframe-mode)
  :custom
  (company-posframe-show-metadata nil)
  (company-posframe-show-indicator nil)
  (company-posframe-quickhelp-delay nil)

  :general
  (:keymaps 'company-active-map
   "M-n" 'company-posframe-quickhelp-toggle
   "M-." 'company-posframe-quickhelp-scroll-up
   "M-," 'company-posframe-quickhelp-scroll-down))

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
  (selectrum-prescient-mode 1))


(provide 'gatsby:completion)
;;; gatsby:completion.el ends here
