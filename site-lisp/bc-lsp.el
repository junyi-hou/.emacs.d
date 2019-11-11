;; bc-lsp.el --- LSP settings -*- lexical-binding: t; -*-

;;; Commentary:

;; this file contains settings for language server protocal related settings
;; including autocompletion, snippet, linter and refactoring supports.

;;; Code:

(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  (python-mode . bc-lsp--reformat-buffer)

  :init
  (defun bc-lsp--reformat-buffer ()
    "Run `eglot-format' before save in the eglot powered buffer."
    (add-hook 'before-save-hook 'eglot-format nil t))

  :config
  (setq eglot-autoreconnect t
        eglot-put-doc-in-help-buffer t))

(provide 'bc-lsp)
;;; bc-lsp.el ends here
