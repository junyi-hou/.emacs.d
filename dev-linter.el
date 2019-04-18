;;; dev-linter.el -- provide on-the-fly linting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package flycheck
  :after evil
  :commands
  (flycheck-buffer flycheck-list-errors flycheck-mode flycheck-add-next-checker)
  :config
  (setq-default flycheck-check-syntax-automatically '(save))
  (flycheck-add-next-checker 'python-flake8 '(warning . python-mypy)))

(use-package flycheck-posframe
  :after flycheck
  :commands flycheck-posframe-mode)

;; setting

(add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)

(provide 'dev-linter)
;;; dev-linter ends here
