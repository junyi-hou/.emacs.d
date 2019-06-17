;;; bc-eshell.el --- my eshell setting -*- lexical-binding: t; -*-

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
        eshell-destroy-buffer-when-process-dies t)
  :config
  (use-package eshell-up
    :commands eshell-up))

(use-package company-shell
  :after eshell
  :commands (company-shell company-env))

(defun bc-eshell-open-here ()
  "Open a new shell in the pwd of the current buffer.  If there is already a eshell buffer open for that directory, switch to that buffer."
  (interactive)
  (let* ((dir (file-name-directory (or (buffer-file-name) default-directory)))
         ;; check whether there exists a eshell buffer for the current directory
         (exists (seq-filter (lambda (buf)
                               (with-current-buffer buf
                                 (and (string-equal major-mode "eshell-mode")
                                      (equal dir default-directory))))
                             (buffer-list)))
         ;; check if the matched eshell buffer is visible
         (visible (when exists
                    (get-buffer-window (car exists)))))
    (if visible
        (select-window visible)
      (split-window-below (- (/ (window-total-height) 3)))
      (other-window 1)
      (if exists
          (switch-to-buffer (car exists))
        (eshell 'Z)))
      (goto-char (point-max))
      (evil-insert-state)))

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
