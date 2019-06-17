;;; bc-style.el --- sytle guide: spell and grammar check -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; spell checker
(use-package flyspell-correct-ivy)

(use-package flyspell-correct
  :commands flyspell-correct-wrapper
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  :general
  (:keymaps '(motion normal visual)
   :prefix "SPC"
   "sp" 'bc-style-correct)
  (:keymaps 'insert
   :prefix "C-c"
   "s" 'bc-style-correct))

(defun bc-style-correct (arg)
"Advising `flyspell-correct-wrapper' to go to previous cursor position."
  (interactive "p")
  (save-excursion
    (flyspell-correct-wrapper arg)))


(add-hook 'text-mode-hook (lambda () (flyspell-mode)))
(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))

(provide 'bc-style)
;;; bc-style.el ends here
