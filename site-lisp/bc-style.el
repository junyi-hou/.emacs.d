;;; bc-style.el --- sytle guide: spell and grammar check -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; spell checker
(use-package flyspell-correct-ivy
  :after '(ivy flyspell-correct))

(use-package flyspell-correct
  :commands flyspell-correct-wrapper
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)

  ;; functions

  (defun bc-style-correct (arg)
    "Advising `flyspell-correct-wrapper' to go to previous cursor position."
    (interactive "p")
    (save-excursion
      (forward-char)
      (flyspell-correct-wrapper arg)))

  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)

  :general
  (:keymaps '(motion normal visual)
   :prefix "SPC"
   "sp" 'bc-style-correct)
  (:keymaps 'insert
   :prefix "C-c"
   "s" 'bc-style-correct))

(provide 'bc-style)
;;; bc-style.el ends here
