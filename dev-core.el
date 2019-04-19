;;; dev-core.el -- settings that should be loaded for every buffer each time emacs starts -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; put custom files in a separate location
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; tramp uses ssh
(require 'tramp)
(setq tramp-default-method "ssh")

;; smooth scroll
(setq-default scroll-step 1
              scroll-conservatively 10000
              auto-window-vscroll nil)

;; first try to split window side-by-side, if window width is < 90, split it top-and-down

(defun dev-core--split-window (&optional window)
  "Split WINDOW side-by-side, if WINDOW width < 90, split it top-and-down."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             (with-selected-window window
               (split-window-right)))
        (split-window-below))))

(setq-default split-window-preferred-function 'dev-core--split-window)

;; always use y-or-n-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; in-house packages that needs to be enable at all time
(setq-default inhibit-splash-screen t
              inhibit-startup-message t
              inhibit-startup-echo-area-message t)

;; disable menu tool and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(tooltip-mode -1)

;; other minor modes I always want
(show-paren-mode 1)          ; highlight matching paren
(global-visual-line-mode 1)  ; word wrapping
(add-hook 'prog-mode-hook #'hs-minor-mode) ; enable folding for all prog modes

;; some major modes
(use-package csv-mode)

;; line numbers
(setq-default display-line-numbers-type 'visual
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; indentation settings
(setq-default indent-tabs-mode nil
              tab-width 4
              python-indent-offset 4
              electric-indent-inhibit t  ; don't change indentation for me
              indicate-empty-lines nil)
(setq backward-delete-char-untabify-method 'hungry)  ; bs kill whole tab

;; Allow 20MB of memory (instead of 0.76MB) before calling
;; garbage collection. This means GC runs less often, which speeds
;; up some operations.
(setq-default large-file-warning-threshold nil
              gc-cons-threshold 20000000)

;; direct backup files to /tmp
(setq-default backup-directory-alist
    `((".*" . ,temporary-file-directory)))
(setq-default auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))

;; When something changes a file, automatically refresh the
;; buffer containing that file so they can't get out of sync.
(global-auto-revert-mode t)


;; define my groups
(defgroup development nil
  "Group for customizing development group."
  :prefix "dev-"
  :group 'emacs)

(defcustom dev-default-remote-machine
  "/ssh:remote:/"
  "The remote machine."
  :type 'string
  :group 'development)

(defcustom dev-default-pairs
  (let ((hash (make-hash-table :test 'equal)))
        (puthash "\"" "\"" hash)
        (puthash "\'" "\'" hash)
        (puthash "(" ")" hash)
        (puthash "\{" "\}" hash)
        (puthash "[" "]" hash)
        (puthash "\`" "\'" hash)
        hash)
  "My auto pair system."
  :group 'development
  :type 'hash-table)


(provide 'dev-core)
;;; dev-core.el ends here
