;;; bc-project.el --- config to jump to definitions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package projectile
  :init
  (setq projectile-project-search-path
        '("~/Documents/Research/" "~/Documents/projects/")
        projectile-globally-ignored-directories
        '(".mypy_cache" "__pycache__" ".git" "log"))
  :config
  (projectile-mode 1))

(provide 'bc-project)
;;; bc-project.el ends here
