;;; dev-jump.el -- jump to definition

(use-package dumb-jump
  :commands (dumb-jump-mode dumb-jump-go dumb-jump-quick-look dumb-jump-go-other-window dumb-jump-back)
  :init
  (setq dumb-jump-selector 'ivy
        dumb-jump-force-searcher 'ag))

(provide 'dev-jump)
;;; dev-jump.el ends here
