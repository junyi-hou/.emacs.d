;;; bc-eshell.el -- my eshell setting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package company-shell
  :after company
  :commands (company-shell company-shell-env))

(require 'bc-autocomplete)

(defun bc-eshell--open (dir)
  "Open an eshell in directory DIR."
  (let* ((default-directory dir)
         (name
          (file-name-base (directory-file-name (file-name-as-directory dir)))))
    (eshell)
    (rename-buffer (concat "*" name "*")))

(defun bc-eshell-open-here ()
  "Open a new shell in the directory associated with the current buffer."
  (interactive)
  (let* ((dir (file-name-directory (or (buffer-file-name) default-directory)))
         (height (/ (window-total-height) 3)))
    (split-window-vertically (- height))
    (evil-window-down 1)
    (bc-eshell--open dir)))

(defun bc-eshell-open-file-in-parent-buffer (file)
  "Open FILE from eshell in the window above the current eshell buffer."
    (evil-window-up 1)
    (find-file file))

(defun bc-eshell-goto-prompt ()
  "Goto current prompt and continue editting."
  (interactive)
  (goto-char (point-max))
  (evil-insert 1))

(defun bc-eshell-clear-buffer ()
  "Eshell version of `cls'."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; settings
;; a bug in eshell mode makes direct defining keymap impossible, need to run
;; hook

;; alias
(eshell/alias "ff" "bc-eshell-open-file-in-parent-buffer $1")
(eshell/alias "cls" "bc-eshell-clear-buffer")

(defun bc-eshell--keymaps ()
  "Define keymap for eshell."
  (general-define-key
   :states 'normal
   :keymaps 'eshell-mode-map
   "A" 'bc-eshell-goto-prompt)

  (general-define-key
   :states 'normal
   :keymaps 'eshell-mode-map
   :prefix "SPC"
   "q" (lambda () (interactive)
         (kill-buffer)
         (evil-quit))))

(defun bc-eshell--autocomplete ()
  "Enable `company-mode' for `eshell-mode'."
  (interactive)
   (make-local-variable 'company-backends)
   (setq company-backends '((company-files company-capf company-yasnippet company-shell company-shell-env)))
   (company-mode 1))


(add-hook 'eshell-mode-hook #'bc-eshell--keymaps)
(add-hook 'eshell-mode-hook #'bc-eshell--autocomplete)


(provide 'bc-eshell)
;;; bc-eshell.el ends here
