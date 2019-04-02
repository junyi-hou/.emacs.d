;;; dev-style.el -- sytle guide: spell and grammar check -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; spell checker

(use-package flyspell-correct
  :commands flyspell-correct-wrapper
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  :config
  (require 'flyspell-correct-ivy)
  :general
  (:keymaps '(motion normal visual)
   :prefix "SPC"
   "sp" 'flyspell-correct-wrapper))


(add-hook 'text-mode-hook (lambda () (flyspell-mode)))
(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))

(provide 'dev-style)
;;; dev-style.el ends here
