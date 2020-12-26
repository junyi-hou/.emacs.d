;; gatsby:lsp.el --- LSP settings -*- lexical-binding: t; -*-

;;; Commentary:

;; this file contains settings for language server protocal related settings
;; including autocompletion, snippet, linter and refactoring supports.

;;; Code:
(require 'gatsby:core)

(use-package nox
  :straight (nox :host github :repo "manateelazycat/nox")
  :custom
  (nox-python-server "pyright")
  :config
  ;; company-box compatibility
  (with-eval-after-load 'company-box
    (defconst company-box-icons--nox-alist
      company-box-icons--lsp-alist)

    (defun company-box-icons--nox (candidate)
      (-when-let* ((nox-item (get-text-property 0 'nox--lsp-item candidate))
                   (kind-num (plist-get nox-item :kind)))
        (alist-get kind-num company-box-icons--nox-alist)))

    (let ((idx (--find-index
                (eq it 'company-box-icons--lsp) company-box-icons-functions)))
      (setq company-box-icons-functions
            (-insert-at idx 'company-box-icons--nox company-box-icons-functions)))))

(provide 'gatsby:lsp)
;;; gatsby:lsp.el ends here
