;;; gatsby:rust.el --- rust mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package rust-mode
  :hook
  (rust-mode . eglot-ensure)
  (rust-mode . tree-sitter-hl-mode)
  :general
  (:states '(motion normal visual)
   :keymaps 'rust-mode-map
   :prefix "SPC"
   "rh" 'gatsby:lsp-help-at-point))

(provide 'gatsby:rust)
;;; gatsby:rust.el ends here
