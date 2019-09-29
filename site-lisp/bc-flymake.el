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
(use-package flymake-posframe
  :after flymake
  :straight (flymake-posframe
             :host github
             :repo "junyi-hou/flymake-posframe")
  :hook (flymake-mode . flymake-posframe-mode)
  :init
  (setq flymake-posframe-prefix
        (let ((hash (make-hash-table :test 'equal)))
          (puthash ':note "? " hash)
          (puthash ':warning "! " hash)
          (puthash ':error "!! " hash)
          (puthash 'eglot-note "? " hash)
          (puthash 'eglot-warning "! " hash)
          (puthash 'eglot-error "!! " hash)
          hash)))

(provide 'bc-flymake)
;;; bc-flymake.el ends here
