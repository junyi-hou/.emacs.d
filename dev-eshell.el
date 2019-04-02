;;; dev-eshell.el -- my eshell setting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dev-evil)

(defun dev-eshell--goto-prompt ()
  "Goto current prompt and continue editting."
  (interactive)
  (goto-char (point-max))
  (evil-insert 1))

(defun dev-eshell--keymap ()
  "Assign keymaps to eshell mode."
  (general-define-key
   :states 'normal
   :keymaps 'eshell-mode-map
    [remap evil-append-line] 'dev-eshell--goto-prompt))

(add-hook 'eshell-mode-hook #'dev-eshell--keymap)


(provide 'dev-eshell)
;;; dev-eshell.el ends here
