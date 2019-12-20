;;; bc-flymake.el --- init flymake for syntax check -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package flymake
  :hook
  (LaTeX-mode . flymake-mode)
  (python-mode . flymake-mode)
  (emacs-lisp-mode . flymake-mode)
  (lisp-interaction-mode . flymake-mode)

  :config
  ;; get rid of the annoying underlines
  (dolist (face '(flymake-note flymake-warning flymake-error))
    (set-face-attribute face nil
                        :underline nil))

  (setq flymake-start-on-newline nil))

;; flymake frontend
(use-package flymake-childframe
  :after flymake
  :straight (flymake-childframe
             :host github
             :repo "junyi-hou/flymake-childframe")
  :hook (flymake-mode . flymake-childframe-mode))

(provide 'bc-flymake)
;;; bc-flymake.el ends here
