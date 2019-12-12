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
        eglot-put-doc-in-help-buffer t
        eglot-stay-out-of '(company)))

(use-package eglot-childframe
  :hook
  (eglot--managed-mode . eglot-childframe-mode)
  (emacs-lisp-mode . eglot-childframe-mode)
  (lisp-interaction-mode . eglot-childframe-mode)

  :straight (eglot-childframe
             :host github
             :repo "junyi-hou/eglot-childframe"
             :branch "elisp-backend-support")
  :general
  (:keymaps 'eglot-childframe-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "rh" 'eglot-childframe-help
   "rd" 'eglot-childframe-definition
   "rj" 'eglot-childframe-reference))

(provide 'bc-lsp)
;;; bc-lsp.el ends here
