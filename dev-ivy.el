;;; dev-ivy.el -- config for ivy -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-initial-inputs-alist nil ; do not use ^
        ivy-format-function #'ivy-format-function-line ; highlight til EOL
        ivy-magic-slash-non-match-action nil ; disable magic slash on nonmatch
        projectile-completion-system 'ivy  ; use projectile
        )
  :general
  (:keymaps 'ivy-minibuffer-map
   "M-j" 'ivy-next-line
   "M-k" 'ivy-previous-line
   "M-J" (lambda () (interactive) (ivy-next-line 3))
   "M-K" (lambda () (interactive) (ivy-previous-line 3))))

(use-package ivy-posframe
  :after ivy
  :config
  (setq ivy-display-function #'ivy-posframe-display-at-point)
  (ivy-posframe-enable))

;; functions

(defun dev-ivy-open-remote-shell (&optional remote)
  "Open a ivy menu with a list of REMOTE location.  Open a eshell at the chosen location."
  (interactive)
  (let* ((remote (or remote dev-default-remote-machine)))
    (ivy-read "Where to?"
              '("home/junyi/Documents/"
                "home/junyi/Documents/Research/"
                "home/junyi/Downloads/data/")
              :action (lambda (x)
                        (let* ((height (/ (window-total-height) 3)))
                          (split-window-vertically (- height))
                          (evil-window-down 1)
                          (dev-eshell--open (concat remote x)))))))

;; settings
(general-define-key
 :keymaps '(normal visual motion)
 :prefix "SPC"
 "or" 'dev-ivy-open-remote-shell)


(provide 'dev-ivy)
;;; dev-ivy.el ends here
