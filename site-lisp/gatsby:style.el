;;; gatsby:style.el --- sytle guide: spell and grammar check -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby:core)

;; TODO: better SPC-sp
(use-package flyspell-correct
  :config
  (setq flyspell-correct-interface #'flyspell-correct-ivy)

  ;; functions
  (defun gatsby:style-correct ()
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
   "sp" 'gatsby:style-correct)
  (:keymaps 'insert
   :prefix "C-c"
   "s" 'gatsby:style-correct))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(provide 'gatsby:style)
;;; gatsby:style.el ends here
