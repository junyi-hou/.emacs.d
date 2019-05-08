;;; bc-lsp.el -- LSP settings -*- lexical-binding: t; -*-

;;; Commentary:

;; this file contains settings for language server protocal related settings
;; including autocompletion, snippet, linter and refactoring supports.

;;; Code:

(use-package eglot
  :commands eglot-ensure)

(use-package flymake
  :config
  (use-package flymake-posframe
    :after posframe
    :load-path "~/Documents/projects/flymake-posframe"
    :hook (flymake-mode . flymake-posframe-mode)))


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
   "<tab>" 'bc-lsp--complete
   "TAB" 'bc-lsp--complete
   "M-j" 'company-select-next
   "M-k" 'company-select-previous-or-abort
   "M-J" 'company-next-page
   "M-K" 'company-previous-page
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

(defun bc-lsp--complete ()
  (interactive)
  (when (company-manual-begin)
    (if (or company-selection-changed
            (member last-command '(company-complete-common
                                   bc-lsp--complete)))
        (call-interactively 'company-complete-selection)
      (call-interactively 'company-complete-common)))
  (completion-at-point))

(provide 'bc-lsp)
;;; bc-lsp.el ends here
