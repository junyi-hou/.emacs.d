;; gatsby:lsp.el --- LSP settings -*- lexical-binding: t; -*-

;;; Commentary:

;; this file contains settings for language server protocal related settings
;; including autocompletion, snippet, linter and refactoring supports.

;;; Code:
(require 'gatsby:core)

(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  (python-mode . gatsby:lsp--reformat-buffer)

  :init

  ;; FIXME
  ;; dirty work-around for bug at
  ;; https://github.com/hlissner/doom-emacs/issues/3269
  (defun project-root (project)
    (car (project-roots project)))

  (defun gatsby:lsp--reformat-buffer ()
    "Run `eglot-format' before save in the eglot powered buffer."
    (add-hook 'before-save-hook 'eglot-format nil t))

  :config
  (setq eglot-autoreconnect t
        eglot-put-doc-in-help-buffer t
        eglot-stay-out-of '(company)))

(provide 'gatsby:lsp)
;;; gatsby:lsp.el ends here
