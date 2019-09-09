;;; bc-theme.el --- setting themes -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; theme
(use-package gruvbox-theme
  :init
  (setq custom-safe-themes t)
  :config
  (load-theme 'gruvbox-dark-hard t)
  (set-face-attribute 'line-number nil
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
(use-package highlight-indentation
  :hook (prog-mode . highlight-indentation-mode))

(defun bc-theme--set-highlight-stipple ()
  "Define custom stipple for highlight-indentation.  See https://github.com/antonj/Highlight-Indentation-for-Emacs/issues/16."
  (let* ((char-width (frame-char-width (selected-frame)))
        (hl-stipple (if (> char-width 8)
                        (list char-width 4 (string 16 0 0 0 0 0 0 0))
                      (list char-width 4 (string 16 0 0 0)))))
    (set-face-attribute
     'highlight-indentation-face
     nil
     :background (face-background 'default)
     :stipple hl-stipple)))

;; Patch highlight-indentation-mode to set/update a stipple attribute
(defadvice highlight-indentation-mode
    (before set-highlight-indentation-stipple activate)
  "Set the stipple used by indentation highlighting."
  (bc-theme--set-highlight-stipple))

;; line numbers
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
        (org-mode . display-line-numbers-mode)
        (LaTeX-mode . display-line-numbers-mode)
  :config
  (setq-default
   display-line-numbers-type 'visual
   display-line-numbers-current-absolute t
   display-line-numbers-width 3
   display-line-numbers-widen nil))

;; display time
(use-package time
  :init
  (setq display-time-load-average-threshold 2.0
        display-time-24hr-format t)
  :config
  (display-time-mode))

;; display battery level
(use-package battery
  :init
  (setq battery-mode-line-limit 30)
  :config
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

;; font

(set-face-attribute
 'default nil
 :family "monospace"
 :height 130
 :weight 'thin
 :width 'normal)

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

(provide 'bc-theme)
;;; bc-theme.el ends here
