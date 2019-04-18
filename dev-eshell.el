;;; dev-eshell.el -- my eshell setting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun dev-eshell--open (dir)
  "Open an eshell in directory DIR.  If there is already an eshell opened in DIR, switch to that eshell."
  (let* ((default-directory dir)
         (name
          (file-name-base (directory-file-name (file-name-as-directory dir))))
         (eshell-name (if (string-match-p "^/ssh:" dir)
                          (concat "*eshell[remote-" name "]*")
                        (concat "*eshell[" name "]*"))))
    (if (member eshell-name (mapcar 'buffer-name (buffer-list)))
        (switch-to-buffer eshell-name)
      (progn
        (eshell)
        (rename-buffer eshell-name)))))

(defun dev-eshell-open-here ()
  "Open a new shell in the directory associated with the current buffer."
  (interactive)
  (let* ((dir (file-name-directory (or (buffer-file-name) default-directory)))
         (height (/ (window-total-height) 3)))
    (split-window-vertically (- height))
    (evil-window-down 1)
    (dev-eshell--open dir)))

(defun dev-eshell-open-file-in-parent-buffer (file)
  "Open FILE from eshell in the `dev-eshell--parent-buffer` of the eshell buffer."
    (evil-window-up 1)
    (find-file file))

(defun dev-eshell-goto-prompt ()
  "Goto current prompt and continue editting."
  (interactive)
  (goto-char (point-max))
  (evil-insert 1))

(defun dev-eshell-clear-buffer ()
  "Eshell version of `cls'."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; settings
;; a bug in eshell mode makes direct defining keymap impossible, need to run
;; hook

(defun dev-eshell--keymaps ()
  "Define keymap for eshell."
  (general-define-key
   :states 'normal
   :keymaps 'eshell-mode-map
   "A" 'dev-eshell-goto-prompt)

  (general-define-key
   :states 'normal
   :keymaps 'eshell-mode-map
   :prefix "SPC"
   "q" (lambda () (interactive)
         (kill-buffer)
         (evil-quit))))

(add-hook 'eshell-mode-hook #'dev-eshell--keymaps)


(provide 'dev-eshell)
;;; dev-eshell.el ends here
