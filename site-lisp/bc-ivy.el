;;; bc-ivy.el --- config for ivy -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-core)

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-count-format ""
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t   . ivy--regex-ignore-order))
        ivy-format-function #'ivy-format-function-line
        ivy-magic-slash-non-match-action nil
        projectile-completion-system 'ivy)

  (use-package counsel
    :after ivy
    :general
    (:keymaps '(normal visual motion)
     :prefix "SPC"
     "or" 'counsel-recentf
     "js" 'counsel-ag
     "jr" (lambda () (interactive)
            (counsel-ag (symbol-name (symbol-at-point))))))

  (use-package ivy-posframe
    :after ivy
    :config
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
    (ivy-posframe-mode 1))

  :general
  (:keymaps 'ivy-minibuffer-map
   "M-j" 'ivy-next-line
   "M-k" 'ivy-previous-line))


;; functions

;; (defun bc-ivy-open-remote-shell (&optional remote)
;;   "Open a ivy menu with a list of REMOTE location.  Open a eshell at the chosen location."
;;   (interactive)
;;   (let* ((remote (or remote bc-default-remote)))
;;     (ivy-read "Where to?"
;;               '("home/junyi/Documents/"
;;                 "home/junyi/Documents/Research/"
;;                 "home/junyi/Downloads/data/")
;;               :action (lambda (x)
;;                         (let* ((height (/ (window-total-height) 3)))
;;                           (split-window-vertically (- height))
;;                           (evil-window-down 1)
;;                           (bc-eshell--open (concat remote x)))))))

;; TODO:
;; uniform ivy backend:
;; 1. projects
;; 2. files
;; 3. buffers
;; 4. recently opened files

;; settings


(provide 'bc-ivy)
;;; bc-ivy.el ends here
