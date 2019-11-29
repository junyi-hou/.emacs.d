;;; bc-core.el --- settings that should be loaded for every buffer each time emacs starts -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; GC
(use-package gcmh
  :straight
  (gcmh :host gitlab :repo "koral/gcmh")
  :init
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)

  (add-hook 'after-init-hook
            (defun bc-core--reset-gc ()
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.2)))

  (gcmh-mode 1))

;; turn off bell
(setq-default visible-bell t
              ring-bell-function 'ignore)

;; mini window should be mini
(setq-default max-mini-window-height 1)

;; smooth scroll
(setq-default scroll-step 1
              scroll-conservatively 10000
              auto-window-vscroll nil)

(defun bc-core--split-vertical (window)
  "Return t if should split WINDOW vertically, otherwise return nil."
  (let* ((h (window-height window))
         (w (window-width window))
         (ratio (/ (float h) w)))
    (cond
     ((< ratio 0.15) t)
     ((< (/ (float w) 2) 80) nil)
     (t t))))

(defun bc-core-split-window (&optional window)
  "Split WINDOW side-by-side, if WINDOW width < 90, split it top-and-down."
  (let ((window (or window (selected-window))))
    (if (bc-core--split-vertical window)
        (split-window-right)
      (split-window-below))))

(setq-default split-window-preferred-function 'bc-core-split-window)

;; always use y-or-n-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; in-house packages that needs to be enable at all time
(setq-default inhibit-splash-screen t
              inhibit-startup-message t
              inhibit-startup-echo-area-message t)

;; cannot delete *scratch* buffer
(defun bc-core--unkillable-scratch ()
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        (insert initial-scratch-message)
        nil)
    t))

(add-hook 'kill-buffer-query-functions
          #'bc-core--unkillable-scratch)

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
(global-auto-revert-mode 1)  ; automatically refresh file when it changes
(use-package recentf
  :hook (after-init . recentf-mode)
  :init (setq
         recentf-save-file       "~/.emacs.d/var/recentf"
         recentf-max-saved-items 100
         recentf-exclude         '("/tmp/" "/ssh:"))
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory))
(use-package hideshow
  :init
  ;; don't make me move to the beginning of line before expanding the block
  (advice-add #'hs-show-block :before #'beginning-of-visual-line)
  ;; don't fold comments
  (setq hs-hide-comments-when-hiding-all nil)

  :hook
  (prog-mode . hs-hide-all)
  (prog-mode . hs-minor-mode))
(use-package prescient
  :init
  (setq prescient-save-file (concat no-littering-var-directory "prescient-save.el"))
  :config
  (prescient-persist-mode))
(use-package page-break-lines
  :config (global-page-break-lines-mode))
(use-package eldoc-box
  :hook ((text-mode prog-mode) . eldoc-box-hover-mode)
  :config
  (defun bc-eldoc--box-position (_ height)
    "Display `eldoc-box' in the bottom left corner of the `selected-window'."
    (let* ((window (selected-window))
           (y (- (nth 3 (window-inside-pixel-edges window)) 5  height))
           (x (window-pixel-left window)))
      (cons x y)))

  (setq eldoc-box-position-function #'bc-eldoc--box-position))
(use-package general)

;; indentation settings
(setq-default indent-tabs-mode nil
              tab-width 4
              python-indent-offset 4
              electric-indent-inhibit t  ; don't change indentation for me
              indicate-empty-lines nil)

(setq-default backward-delete-char-untabify-method 'hungry)  ; bs kill whole tab

(provide 'bc-core)
;;; bc-core.el ends here
