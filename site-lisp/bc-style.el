;;; bc-style.el --- sytle guide: spell and grammar check -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; spell checker
(use-package flyspell-correct-ivy
  :after flyspell-correct)

(use-package flyspell-correct
  :config
  (setq flyspell-correct-interface #'flyspell-correct-ivy)

  ;; functions
  (defun bc-style-correct ()
    "Advising `flyspell-correct-wrapper' to go to previous cursor position."
    (interactive)
    (save-excursion
      (forward-char)
      (flyspell-correct-wrapper)))

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
