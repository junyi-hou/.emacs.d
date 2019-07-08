;;; bc-ide-rust.el --- provide IDE feature for working on rust code -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-company)
(require 'bc-flymake)
(require 'bc-lsp)

(use-package rust-mode
  :init
  ;; functions

  (defun bc-ide-rust-edit-cargo-toml ()
    "Edit the cargo.toml file of the current rust project in a new window."
    (interactive)
    (let ((cargo
           (concat (cdr (project-current)) "Cargo.toml")))
      (when (file-exists-p cargo)
        (bc-core--split-window)
        (other-window 1)
        (find-file cargo))))

  (defun bc-ide-rust-build ()
    "Build the current project in eshell."
    (interactive)
    (bc-eshell-open-here)
    (insert "cargo build")
    (eshell-send-input))

  (defun bc-ide-rust-release ()
    "Build the current project in eshell."
    (interactive)
    (bc-eshell-open-here)
    (insert "cargo build --release")
    (eshell-send-input))

  (defun bc-ide-rust-run-in-eshell ()
    "Run the current project in eshell."
    (interactive)
    (bc-eshell-open-here)
    (insert "cargo run")
    (eshell-send-input))
  
  :config
  (setq rust-format-on-save t)
  :general
  (:keymaps 'rust-mode-map
   :states '(normal visual)
   :prefix "SPC"
   "rr" 'bc-ide-rust-run-in-eshell
   "rB" 'bc-ide-rust-release
   "rb" 'bc-ide-rust-build
   "ro" 'bc-ide-rust-edit-cargo-toml))

(use-package toml-mode
  :general
  (:keymaps 'toml-mode-map
   :states '(normal visual)
   :prefix "SPC"
   "q" 'kill-buffer-and-window))

(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'company-mode t)

(provide 'bc-ide-rust)
;;; bc-ide-rust.el ends here
