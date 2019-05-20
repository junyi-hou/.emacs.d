;;; bc-ide-rust.el -- provide IDE feature for working on rust code -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-company)
(require 'bc-flymake)
(require 'bc-lsp)

(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  :general
  (:keymaps 'rust-mode-map
   :states '(normal visual)
   :prefix "SPC"
   "rr" 'bc-ide-rust-run-in-eshell
   "rb" 'rust-compile
   "ro" 'bc-ide-rust-edit-cargo-toml))

(use-package toml-mode
  :general
  (:keymaps 'toml-mode-map
   :states '(normal visual)
   :prefix "SPC"
   "q" 'kill-buffer-and-window))

;; functions

(defun bc-ide-rust-edit-cargo-toml ()
  "Edit the cargo.toml file of the current rust project in a new window."
  (interactive)
  (let ((cargo
         (concat (projectile-root-bottom-up default-directory) "Cargo.toml")))
    (when (file-exists-p cargo)
      (bc-core--split-window)
      (other-window 1)
      (find-file cargo))))

(defun bc-ide-rust-run-in-eshell ()
  "Run the current project in eshell."
  (interactive)
  (bc-eshell-open-here)
  (insert "cargo run")
  (eshell-send-input))


(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'company-mode t)

(provide 'bc-ide-rust)
;;; bc-ide-rust.el ends here
