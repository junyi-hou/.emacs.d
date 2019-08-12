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

  ;; functions

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

  (add-hook 'eshell-mode-hook #'bc-eshell--keymaps))

(use-package xterm-color
  :after eshell

  :init
  (defun bc-eshell--set-term-envvar ()
    "Set TERM to term-256color."
    (setenv "TERM" "xterm-256color"))

  :config
  (add-hook 'eshell-mode-hook #'bc-eshell--set-term-envvar)
  (add-hook 'eshell-before-prompt-hook (lambda () (setq xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

(use-package company-shell
    :commands (company-shell company-env)
    :init
    (add-hook 'eshell-mode-hook
            (defun bc-eshell--company ()
              (setq-local company-backend
                          '((company-shell
                             company-env
                             company-flies
                             company-yasnippet)
                            (company-dabbrev
                             company-abbrev)))
              (company-mode))))


(use-package esh-module
  ;; foo package, just to make things tidy
  :ensure nil
  :after eshell
  :init
  (setq password-cache t
        password-cache-expiry 360)
  :config
  (require 'esh-module)
  (require 'em-tramp)
  (add-to-list 'eshell-modules-list #'eshell-tramp))


(provide 'bc-eshell)
;;; bc-eshell.el ends here
