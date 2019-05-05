;;; bc-linter.el -- provide on-the-fly linting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package flycheck
  :after evil
  :defer t
  :commands
  (flycheck-buffer flycheck-list-errors flycheck-mode flycheck-add-next-checker)
  :config
  (setq-default flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-add-next-checker 'python-flake8 '(warning . python-mypy)))

(use-package flycheck-posframe
  :after flycheck
  :defer t
  :commands flycheck-posframe-mode)

(use-package hl-todo
  :config
  :defer t
  (setq hl-todo-keyword-faces
        '(("TODO" . "#cc9393")
          ("OKAY" . "#7cb8bb")
          ("FAIL" . "#8c5353")
          ("NOTE"   . "#d0bf8f")
          ("HACK"   . "#d0bf8f")
          ("TEMP"   . "#d0bf8f")
          ("FIXME"  . "#cc9393")
          ("\\?\\?\\?+" . "#cc9393")))
  (add-hook 'prog-mode-hook #'hl-todo-mode))

;; setting

(add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)

(provide 'bc-linter)
;;; bc-linter ends here
