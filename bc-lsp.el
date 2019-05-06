;;; bc-lsp.el -- LSP settings -*- lexical-binding: t; -*-

;;; Commentary:

;; this file contains settings for language server protocal related settings
;; including autocompletion, snippet, linter and refactoring supports.

;;; Code:

(use-package eglot
  :commands eglot)

(use-package yasnippet
  :commands yas-minor-mode
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
   "<tab>" 'bc-autocomplete--company-complete
   "TAB" 'bc-autocomplete--company-complete
   "M-j" 'company-select-next
   "M-k" 'company-select-previous-or-abort
   "RET" 'company-abort
   "<return>" 'company-abort)

  :config
  (add-hook 'company-mode-hook #'yas-minor-mode)
  (setq company-idle-delay nil
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-limit 10
        company-selection-wrap-around t
        company-backends '((company-files
                            company-capf
                            company-yasnippet)
                           (company-dabbrev
                            company-abbrev))))

(use-package company-posframe
  :after company
  :commands company-posframe-mode
  :hook (company-mode . company-posframe-mode))

;; function:

;; taken from RSW at https://gist.github.com/rswgnu/85ca5c69bb26551f3f27500855893dbe
(defun bc-autocomplete--company-complete ()
  "Insert the common part of all candidates or the current selection.
The first time this is called, the common part is inserted, the second
time, or when the selection has been changed, the selected candidate is
inserted."
  (interactive)
  (when (company-manual-begin)
    (if (or company-selection-changed company-common)
        (call-interactively 'company-complete-selection)
      (call-interactively 'company-complete-common))))


(provide 'bc-lsp)
;;; bc-lsp.el ends here
