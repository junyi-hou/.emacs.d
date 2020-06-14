;;; gatsby:project.el --- find-file-in-project -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby:core)

(use-package project
  :init
  (with-eval-after-load 'vc-hooks
    (setq vc-follow-symlinks t))

  ;; FIXME
  ;; dirty work-around for bug at
  ;; https://github.com/hlissner/doom-emacs/issues/3269
  (defun project-root (project)
    (car (project-roots project)))
  )

(use-package project-plus
  :straight (project-plus :repo "junyi-hou/project-plus.el" :host github)
  :after project
  :config
  (project-plus-mode 1)
  :general
  (:keymaps '(normal motion visual emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "op" 'project-plus-switch-project
   "of" 'project-plus-find-file))

(provide 'gatsby:project)
;;; gatsby:project.el ends here
