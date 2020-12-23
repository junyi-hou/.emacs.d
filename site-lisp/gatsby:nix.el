;;; gatsby:nix.el --- nix package/environment manager setting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package json-mode)

(use-package nix-mode

  :mode ("\\.nix\\'" "\\.nix.in\\'"))

(use-package nix-drv-mode
  :straight nix-mode
  :mode "\\.drv\\'")

(use-package nix-shell
  :straight nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package envrc
  :straight (envrc :host github :repo "purcell/envrc")
  ;; :init
  ;; (envrc-global-mode)
  )



(provide 'gatsby:nix)
;;; gatsby:nix.el ends here
