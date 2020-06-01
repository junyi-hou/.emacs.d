;;; gatsby:style.el --- sytle guide: spell and grammar check -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby:core)

;; TODO: better SPC-sp
;; TODO: integrate with selectrum
;; (use-package flyspell-correct
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

(provide 'gatsby:style)
;;; gatsby:style.el ends here
