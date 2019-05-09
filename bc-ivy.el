;;; bc-ivy.el -- config for ivy -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-core)

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

(use-package ivy-xref
  :after (ivy lsp-mode)
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


;; functions

(defun bc-ivy-open-remote-shell (&optional remote)
  "Open a ivy menu with a list of REMOTE location.  Open a eshell at the chosen location."
  (interactive)
  (let* ((remote (or remote bc-default-remote)))
    (ivy-read "Where to?"
              '("home/junyi/Documents/"
                "home/junyi/Documents/Research/"
                "home/junyi/Downloads/data/")
              :action (lambda (x)
                        (let* ((height (/ (window-total-height) 3)))
                          (split-window-vertically (- height))
                          (evil-window-down 1)
                          (bc-eshell--open (concat remote x)))))))

;; settings
(general-define-key
 :keymaps '(normal visual motion)
 :prefix "SPC"
 "or" 'bc-ivy-open-remote-shell)


(provide 'bc-ivy)
;;; bc-ivy.el ends here
