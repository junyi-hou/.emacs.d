;;; dev-jump.el -- jump to definition -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package dumb-jump
  :commands (dumb-jump-mode dumb-jump-go dumb-jump-quick-look dumb-jump-go-other-window dumb-jump-back)
  :init
  (setq dumb-jump-selector 'ivy
        dumb-jump-force-searcher 'ag)
  :general
  (:keymaps '(motion normal visual)
   :prefix "SPC"
   
   "jg" 'dumb-jump-go-other-window
   "jG" 'dumb-jump-go
   "jb" 'dumb-jump-back
   "jj" 'dumb-jump-quick-look))

(provide 'dev-jump)
;;; dev-jump.el ends here
