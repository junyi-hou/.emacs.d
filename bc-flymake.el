;;; bc-flymake.el --- init flymake for syntax check -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package flymake
  :config
  ;; get rid of the annoying underlines
  (dolist (face '(flymake-note flymake-warning flymake-error))
    (set-face-attribute face nil
     :underline nil))

  (setq flymake-start-on-newline nil)

  ;; flymake frontend
  (use-package flymake-posframe
    :after flymake
    :quelpa (flymake-posframe
             :repo "junyi-hou/flymake-posframe"
             :fetcher github)
    :hook (flymake-mode . flymake-posframe-mode)))


(provide 'bc-flymake)
;;; bc-flymake.el ends here
