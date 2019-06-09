;;; bc-eshell.el -- my eshell setting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-core)
(require 'bc-company)

(use-package eshell
  :init
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t))

(use-package company-shell
  :after eshell
  :commands (company-shell company-env))

(defun bc-eshell--open (dir)
  "Open an eshell in directory DIR.  If there is already a eshell buffer open, switch to that buffer and cd to DIR."
  (switch-to-buffer (get-buffer-create "*eshell*"))
  (unless (string-equal major-mode "eshell-mode")
    (eshell 'Z))
  (eshell-interrupt-process)
  (eshell/cd dir)
  (goto-char (point-max))
  (bc-eshell-clear-buffer)
  (eshell-send-input)
  (evil-insert-state))

(defun bc-eshell-open-here ()
  "Open a new shell in the pwd of the current buffer.  If there is already a eshell buffer open for that directory, switch to that buffer."
  (interactive)
  (let* ((dir (file-name-directory (or (buffer-file-name) default-directory))))
    (split-window-below (- (/ (window-total-height) 3)))
    (other-window 1)
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
;; (eshell/alias "ff" "bc-eshell-open-file-in-parent-buffer $1")
;; (eshell/alias "cls" "bc-eshell-clear-buffer")
;; (eshell/alias "l" "ls -AlohG --color=always")
;; (eshell/alias "cd" "bc-eshell-cd $1")

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
   "q" 'kill-buffer-and-window))

(add-hook 'eshell-mode-hook #'bc-eshell--keymaps)
(add-hook 'eshell-mode-hook (defun bc-eshell--company ()
                              (setq-local company-backend
                                          '((company-shell
                                             company-env
                                             company-flies
                                             company-yasnippet)
                                            (company-dabbrev
                                             company-abbrev)))
                              (company-mode)))


(provide 'bc-eshell)
;;; bc-eshell.el ends here
