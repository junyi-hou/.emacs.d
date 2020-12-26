;;; gatsby:completion.el --- init company-mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby:minibuffer)

(use-package yasnippet
  :commands (yas-minor-mode yas-reload-all)
  :hook
  (company-mode . yas-reload-all)
  (company-mode . yas-minor-mode)
  :init
  (defun gatsby:yas-better-backspace ()
    "If `point' is at the beginning of an unmodified yas-field, delete the field, otherwise backwards delete char."
    (interactive)
    (cond ((yas--maybe-clear-field-filter t)
           (yas--skip-and-clear (yas-current-field)))
          (t (call-interactively #'backward-delete-char-untabify))))

  :general
  (:keymaps 'yas-keymap
   "<backspace>" 'gatsby:yas-better-backspace))

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
  (company-dabbrev-code-other-buffers nil)
  (company-show-numbers t)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  (company-auto-complete t)
  (company-auto-complete-chars '(44 46)) ;; trigger complete if press "," or "."
  (company-search-regexp-function #'company-search-words-in-any-order-regexp)
  (company-backends '(company-semantic
                      company-yasnippet
                      company-capf
                      company-files
                      (company-dabbrev-code
                       company-keywords
                       company-dabbrev
                       company-abbrev)))

  :config
  (defun gatsby:indent-or-complete ()
    "Tab for company, yas, and indentation.
1, if nothing before the `point', insert `tab-width' number of spaces.
2, if there is a yas-snippet, expand it.
3, if 1/2 not satisfies, call `company-manual-begin'."
    (interactive)
    (if (looking-back "^[ \t]*" (line-beginning-position)) ;; case 1
        (dotimes (n tab-width) (insert " "))
      (unless (yas-expand) ;; case 2
        ;; case 3
        (let ((company-backends
               (-filter #'identity
                        (--map
                         (cond ((sequencep it) (--remove (eq it 'company-yasnippet) it))
                               ((eq it 'company-yasnippet) nil)
                               (t it))
                         company-backends))))
          ;; manually call `company-manual-begin' will set
          ;; `company-minimum-prefix-length' to 0, which means that the snippets
          ;; will always get included. To fix this add a condition that if
          ;; all candidates are snippets, cancel auto completion and indent region.
          (unless (symbol-value company-search-mode)
            (company-filter-candidates))))))

  (with-eval-after-load 'yasnippet
    (defun gatsby:yas-disable-company ()
      (general-define-key
       :keymaps 'company-active-map
       "<tab>" 'yas-next-field-or-maybe-expand))

    (defun gatsby:yas-restore-company ()
      (general-define-key
       :keymaps 'company-active-map
       "<tab>" 'company-complete-selection))

    (add-hook 'yas-before-expand-snippet-hook 'gatsby:yas-disable-company)
    (add-hook 'yas-after-exit-snippet-hook 'gatsby:yas-restore-company))

  (defun gatsby:complete-and-insert-char (char)
    "Complete with the current selection and insert CHAR."
    (company-complete-selection)
    (insert (char-to-string char)))

  (defun gatsby:company-filter-delete ()
    "improve `company-search-delete-char'."
    (if (string= company-search-string "")
        (progn
          (company-search-abort)
          (call-interactively #'backward-delete-char-untabify))
      (let ((ss (substring company-search-string 0 -1)))
        (when company-search-filtering
          (company--search-update-predicate ss))
        (company--search-update-string ss))))
  (advice-add #'company-search-delete-char :override #'gatsby:company-filter-delete)

  (defun gatsby:company-filter-abort ()
    "Abort both the `company-search-mode' and the `company-mode'"
    (interactive)
    (ignore-errors (company-search-abort))
    (company-abort))

  (defun gatsby:company--insert-char (char)
    "Complete the company completion session with the current selection and insert CHAR."
    (company-complete-selection)
    (insert (char-to-string char)))

  ;; bind auto complete chars in `company-search-map'
  (dolist (i company-auto-complete-chars)
    (define-key company-search-map (char-to-string i)
      (lambda () (interactive) (gatsby:company--insert-char i))))

  :general
  (:keymaps 'company-active-map
   "M-j" 'company-select-next
   "M-k" 'company-select-previous
   "<tab>" 'company-complete-selection)

  (:keymaps 'company-search-map
   "M-j" 'company-select-next
   "M-k" 'company-select-previous
   "C-g" 'gatsby:company-filter-abort
   "<tab>" 'company-complete-selection)

  (:keymaps 'insert
   "<tab>" 'gatsby:indent-or-complete))

(use-package company-prescient
  :hook
  (company-mode . company-prescient-mode))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-enable-icon nil)
  (company-box-doc-delay 2)
  :general)

(use-package company-tabnine
  :config
  (when-let ((capf-idx (--find-index (eq it 'company-capf) company-backends)))
    (setq company-backends (-insert-at (1+ capf-idx) 'company-tabnine company-backends))))

(provide 'gatsby:completion)
;;; gatsby:completion.el ends here
