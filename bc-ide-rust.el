;;; bc-ide-rust.el -- provide IDE feature for working on rust code -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-company)
(require 'bc-flymake)
(require 'bc-lsp)

(use-package rust-mode
  :config
  (add-hook 'rust-before-save-hook #'rust-format-buffer))

(use-package toml-mode)

;; setting

(defun rust-hook ()
  (setq tab-width 4)
  (setq lsp-rust-clippy-preference "on")
  (lsp)

  (setq-local company-backends
              (let* ((first (car company-backends))
                     (rest (cdr company-backends))
                     (removed-capf (remove 'company-capf first))
                     (add-lsp (push 'company-lsp removed-capf)))
                (cons add-lsp rest)))
  (company-mode))


(add-hook 'rust-mode-hook #'rust-hook)



(provide 'bc-ide-rust)
;;; bc-ide-rust.el ends here
