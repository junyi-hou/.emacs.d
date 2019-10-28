;;; bc-theme.el --- setting themes -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; theme
(use-package gruvbox-theme
  :init
  (setq custom-safe-themes t)

  :config
  (load-theme 'gruvbox-dark-hard t)

  (set-face-attribute
   'default
   nil
   :family "Monospace"
   :height 150
   :weight 'Light
   :width 'normal)

  (set-face-attribute
   'minibuffer-prompt
   nil
   :weight 'normal)

  (set-face-attribute
   'line-number
   nil
   :background (face-background 'default)))

;; mode line
(use-package telephone-line
  :init

  (set-face-attribute
   'mode-line-buffer-id
   nil
   :weight 'normal)

  (telephone-line-defsegment* bc-theme-pctg-buffer-position ()
    (concat "LN " (format "%d/%d"
                          (1+ (count-lines 1 (point)))
                          (1+ (count-lines (point-min) (point-max))))))
  (setq telephone-line-lhs
        '((evil     . (telephone-line-evil-tag-segment))
          (accent   . (telephone-line-vc-segment
                       telephone-line-erc-modified-channels-segment))
          (nil      . (telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil      . (telephone-line-misc-info-segment))
          (accent   . (telephone-line-major-mode-segment))
          (evil     . (bc-theme-pctg-buffer-position))))
  :config
  (telephone-line-mode 1))

;; indentation guide
(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (LaTeX-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\┆
        highlight-indent-guides-responsive 'stack))

;; line numbers
(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  (org-mode . display-line-numbers-mode)
  (LaTeX-mode . display-line-numbers-mode)
  :config
  (setq-default
   display-line-numbers-type 'visual
   display-line-numbers-current-absolute t
   display-line-numbers-width 3
   display-line-numbers-widen nil))

;; dim windows out of focus
(use-package dimmer
  :demand t
  :config
  (dimmer-mode))

;; display time
(use-package time
  :config
  (setq display-time-load-average-threshold 2.0
        display-time-24hr-format t)
  (display-time-mode))

;; display battery level
(use-package battery
  :config
  (setq battery-mode-line-limit 30)
  (display-battery-mode))

;; highlight keywords
(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . "#FB4934")
          ("FIXME"  . "#FB4934")
          ("WONT FIX" . "#FB4934")
          ("REVIEW"   . "#FABD2F")
          ("NOTE"   . "#FABD2F")
          ("HACK"   . "#FABD2F")
          ("\\?\\?\\?+" . "#cc9393")))
  (global-hl-todo-mode))

;; load everything

(setq-default left-fringe-width 8)

(defun bc-fontsize-up (&optional size)
  "Increase the font size in the current frame by SIZE.  If SIZE is nil, default to 5."
  (interactive)
  (let* ((current-size (plist-get (custom-face-attributes-get 'default nil) :height))
         (new-size (+ (or size 5) current-size)))
    (set-face-attribute
     'default (selected-frame)
     :height new-size)))

(defun bc-fontsize-down (&optional size)
  "Decrease the font size in the current frame by SIZE.  If SIZE is nil, default to 5."
  (interactive)
  (let* ((current-size (plist-get (custom-face-attributes-get 'default nil) :height))
         (new-size (- current-size (or size 5))))
    (set-face-attribute
     'default (selected-frame)
     :height new-size)))

(defun bc-fontsize-reset ()
  "Reset the current frame font size."
  (interactive)
  (set-face-attribute
   'default (selected-frame) :height 150))


(provide 'bc-theme)
;;; bc-theme.el ends here
