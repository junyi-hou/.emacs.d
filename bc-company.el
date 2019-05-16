;;; bc-company.el -- init company-mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package yasnippet
  :commands (yas-minor-mode yas-reload-all)
  :general
  (:keymaps 'yas-minor-mode-map
            "<tab>" nil
            "TAB" nil
            "M-f" 'yas-expand
            "M-j" 'yas-next-field
            "M-k" 'yas-prev-field))

(use-package company
  :commands company-mode
  :general
  (:keymaps 'company-active-map
   "<tab>" 'bc-company-unified-tab
   "M-j" 'company-select-next
   "M-k" 'company-select-previous-or-abort
   "M-J" 'company-next-page
   "M-K" 'company-previous-page)

  :config
  (add-hook 'company-mode-hook (defun bc-lsp-load-yas ()
                                 (yas-minor-mode)
                                 (yas-reload-all)))
  (setq company-idle-delay nil
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-show-numbers t
        company-tooltip-limit 10
        company-selection-wrap-around t
        company-backends '((company-capf
                            company-yasnippet
                            company-files)
                           (company-dabbrev
                            company-abbrev))))

(use-package company-posframe
  :after company
  :commands company-posframe-mode
  :hook (company-mode . company-posframe-mode))

(defun bc-company-unified-tab ()
  "Use tab for both company and indentation.

In insert mode, first try `company-manual-begin'.  If there is no completion available at point, indent the current line by `tab-width' length."
  (interactive)
  (if (looking-back "^[ \t]*" (line-beginning-position))
      (dotimes (n tab-width) (insert " "))
    (yas-expand)
    (company-manual-begin)
    ;; HACK: manually call `company-manual-begin' will set
    ;; `company-minimum-prefix-length' to 0, which means that the snippets
    ;; will always get included. To fix this add a condition that if
    ;; all candidates are snippets, cancel auto completion and indent region.
    (if (null (seq-every-p
               (lambda (candidate)
                 (member candidate (yas-active-keys)))
               company-candidates))
        (if (or company-selection-changed
                (memq last-command '(company-complete-common
                                     bc-lsp-unified-tab)))
            (call-interactively 'company-complete-selection)
          (call-interactively 'company-complete-common)
          (when company-candidates
            (setq this-command 'company-complete-common)))
      (company-cancel)
      (indent-region (line-beginning-position) (line-end-position)))))

(provide 'bc-company)
;;; bc-company.el ends here
