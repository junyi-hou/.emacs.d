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
        ivy-use-virtual-buffers t  ; add recent files and bookmarks to blist
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

(provide 'dev-ivy)
;;; dev-ivy.el ends here
