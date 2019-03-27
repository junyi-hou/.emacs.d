;;; dev-ivy.el -- config for ivy

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq ivy-height 10
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
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

(provide 'dev-ivy)
;;; dev-ivy.el ends here
