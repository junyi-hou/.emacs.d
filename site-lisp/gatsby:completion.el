;;; gatsby:completion.el --- init company-mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby:core)

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

(provide 'gatsby:completion)
;;; gatsby:completion.el ends here
