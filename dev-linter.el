;;; dev-linter.el -- provide on-the-fly linting

(use-package flycheck
  :after evil
  :commands (flycheck-buffer flycheck-list-errors flycheck-mode)
  :config
  (setq-default flycheck-check-syntax-automatically '(save))
  (defun dev-linter--check ()
    (when flycheck-mode
      (ignore-errors (flycheck-buffer))
      nil))
  (add-hook 'evil-normal-state-entry-hook #'dev-linter--check)

  (flycheck-add-next-checker 'python-flake8 '(warning . python-mypy)))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5))

(eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(provide 'dev-linter)
;;; dev-linter ends here
