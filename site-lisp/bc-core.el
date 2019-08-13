;;; bc-core.el --- settings that should be loaded for every buffer each time emacs starts -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; put custom files in a separate location
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; turn off bell
(setq ring-bell-function 'ignore)

;; tramp uses ssh
(setq-default tramp-default-method "ssh")

;; smooth scroll
(setq-default scroll-step 1
              scroll-conservatively 10000
              auto-window-vscroll nil)

;; first try to split window side-by-side, if window width is < 90, split it top-and-down

(defun bc-core--split-window (&optional window)
  "Split WINDOW side-by-side, if WINDOW width < 90, split it top-and-down."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             (with-selected-window window
               (split-window-right)))
        (split-window-below))))

(setq-default split-window-preferred-function 'bc-core--split-window)

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
(blink-cursor-mode -1)

;; other minor modes I always want
(show-paren-mode 1)          ; highlight matching paren
(global-visual-line-mode 1)  ; word wrapping
(global-subword-mode 1)      ; better camelCase support
(use-package hideshow
  :init
  (advice-add #'hs-show-block :before #'beginning-of-visual-line)
  :hook
  (prog-mode . hs-hide-all)
  (prog-mode . hs-minor-mode))

;; indentation settings
(setq-default indent-tabs-mode nil
              tab-width 4
              python-indent-offset 4
              electric-indent-inhibit t  ; don't change indentation for me
              indicate-empty-lines nil)

(setq-default backward-delete-char-untabify-method 'hungry)  ; bs kill whole tab

;; use doom-emacs gc
(setq gc-cons-threshold 402653184
    gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

;; direct backup files to /tmp
(setq-default backup-directory-alist
    `((".*" . ,temporary-file-directory)))
(setq-default auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))

;; When something changes a file, automatically refresh the
;; buffer containing that file so they can't get out of sync.
(global-auto-revert-mode 1)

(provide 'bc-core)
;;; bc-core.el ends here
