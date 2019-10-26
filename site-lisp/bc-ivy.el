;;; bc-ivy.el --- config for ivy -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-core)

(use-package ivy
  :config
  (setq ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-count-format ""
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ivy-format-function #'ivy-format-function-line
        ivy-magic-slash-non-match-action nil)

  :init
  (ivy-mode 1)
  :general
  (:keymaps 'ivy-minibuffer-map
   "M-j" 'ivy-next-line
   "M-k" 'ivy-previous-line))

(use-package counsel
  :after ivy
  :init
  (defun bc-ivy-grep-at-point ()
    "Call `counsel-ag' on the `symbol-at-point'."
    (interactive)
    (counsel-ag (symbol-name (symbol-at-point))))

  :general
  (:keymaps '(normal visual motion)
   :prefix "SPC"
   "rs" 'bc-ivy-grep-at-point
   "rS" 'counsel-ag))

;; need to figure out a way to make this work with exwm-mode
;; (use-package ivy-posframe
;;   :after ivy
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
;;   (ivy-posframe-mode 1))

(use-package ivy-prescient
    :after ivy
    :config
    (ivy-prescient-mode))


(provide 'bc-ivy)
;;; bc-ivy.el ends here
