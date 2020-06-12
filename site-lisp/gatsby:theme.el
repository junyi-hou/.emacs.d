;;; gatsby:theme.el --- setting themes -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby:core)

;; theme
(use-package vscode-dark-plus-theme
  :straight
  (vscode-dark-plus-theme :repo "ianpan870102/vscode-dark-plus-emacs-theme" :host github)
  :init
  (setq custom-safe-themes t)
  (setq-default left-fringe-width 8)

  (load-theme 'vscode-dark-plus t)

  (with-eval-after-load 'magit-delta
    (setq magit-delta-delta-args `("--max-line-distance" "0.6"
                                   "--24-bit-color" "always"
                                   "--minus-color=#420000"
                                   "--plus-color=#4a5200"
                                   "--plus-emph-color=#00b300"
                                   "--color-only")))

  ;; increase active/inactive mode-line contrast
  (set-face-foreground 'mode-line "#fafafa")
  (set-face-background 'mode-line "#00538a" )
  (set-face-foreground 'mode-line-inactive "#444444")
  (set-face-background 'mode-line-inactive "#002c4a")

  (defun gatsby:theme-fontsize-up (&optional size)
    "Increase the font size in the current frame by SIZE.  If SIZE is nil, default to 5."
    (interactive)
    (let* ((current-size (plist-get (custom-face-attributes-get 'default nil) :height))
           (new-size (+ (or size 5) current-size)))
      (set-face-attribute
       'default (selected-frame)
       :height new-size)))

  (defun gatsby:theme-fontsize-down (&optional size)
    "Decrease the font size in the current frame by SIZE.  If SIZE is nil, default to 5."
    (interactive)
    (let* ((current-size (plist-get (custom-face-attributes-get 'default nil) :height))
           (new-size (- current-size (or size 5))))
      (set-face-attribute
       'default (selected-frame)
       :height new-size)))

  (defun gatsby:theme-fontsize-reset ()
    "Reset the current frame font size."
    (interactive)
    (set-face-attribute
     'default (selected-frame) :height 150))

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

  :general
  (:keymaps '(motion normal visual emacs insert)
   "C-+" 'gatsby:theme-fontsize-up
   "C--" 'gatsby:theme-fontsize-down
   "C-=" 'gatsby:theme-fontsize-reset))

;; display battery level
(use-package battery
  :init
  (setq battery-mode-line-limit 100)
  :config
  (display-battery-mode))

;; display time
(use-package time
  :init
  (setq display-time-default-load-average nil
        display-time-24hr-format t)
  :config
  (display-time-mode))

(use-package doom-modeline
  :init
  (setq doom-modeline-project-detection 'project
        doom-modeline-buffer-file-name-style 'relative-to-project
        ;; if only I can disable all-the-icon dependency...
        doom-modeline-icon nil
        doom-modeline-vcs-max-length 20)
  :config
  (doom-modeline-def-segment current-line
    (if-let ((line (format "%s(%d/%d)"
                           (doom-modeline-spc)
                           (line-number-at-pos (point))
                           (line-number-at-pos (point-max))))
             ((doom-modeline--active)))
        line
      (propertize line 'face 'mode-line-inactive)))

  (doom-modeline-def-segment time-battery
    "Display time and battery"
    (if-let ((time (if (bound-and-true-p display-time-mode)
                       display-time-string ""))
             (battery (if (bound-and-true-p display-battery-mode)
                          (format "(%s)%s"
                                  (car doom-modeline--battery-status)
                                  (cdr doom-modeline--battery-status)) ""))
             ((doom-modeline--active)))
        (concat time (doom-modeline-spc) battery (doom-modeline-spc))
      (propertize (concat time (doom-modeline-spc) battery (doom-modeline-spc))
                  'face 'mode-line-inactive)))

  (doom-modeline-def-modeline 'main
    '(modals vcs remote-host buffer-info current-line)
    '(input-method major-mode time-battery))

  (doom-modeline-def-modeline 'project
    '(modals vcs buffer-default-directory)
    '(input-method major-mode time-battery))

  (doom-modeline-def-modeline 'message
    '(modals buffer-info-simple)
    '(input-method major-mode time-battery))

  (doom-modeline-def-modeline 'vcs
    '(modals buffer-info current-line)
    '(input-method process time-battery))

  (doom-modeline-def-modeline 'info
    '(modals buffer-info)
    '(misc-info time-battery))

  (doom-modeline-def-modeline 'media
    '(modals buffer-info-simple media-info)
    '(major-mode time-battery))

  (doom-modeline-def-modeline 'pdf
    '(modals vcs buffer-info)
    '(pdf-pages time-battery))

  (with-eval-after-load 'exwm
    (defvar gatsby:theme-exwm-title-max-length 60)

    (doom-modeline-def-segment exwm-title
      "exwm buffer title, truncated if too long."
      (if-let ((title (buffer-name))
               ((> (length title) gatsby:theme-exwm-title-max-length)))
          (concat (substring title 0 gatsby:theme-exwm-title-max-length) "...")
        title))

    (doom-modeline-def-modeline 'exwm
      '(modals exwm-title) '(major-mode time-battery))

    (defun gatsby:theme-set-exwm-modeline ()
      (doom-modeline-set-modeline 'exwm))

    (add-hook 'exwm-manage-finish-hook #'gatsby:theme-set-exwm-modeline))

  (doom-modeline-mode 1))

;; indentation guide
(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (LaTeX-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-responsive 'stack))

;; visual cue of the cursor position
(use-package beacon
  ;; use chep's fork so it can grow backwards
  :straight (beacon :host github :repo "junyi-hou/beacon")
  :init
  (setq beacon-blink-when-window-scrolls nil
        beacon-can-go-backwards t
        beacon-size 15)
  :config
  (beacon-mode 1))

;; line numbers
(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  (org-mode . display-line-numbers-mode)
  (LaTeX-mode . display-line-numbers-mode)
  :init
  (setq display-line-numbers-type 'visual
        display-line-numbers-current-absolute t
        display-line-numbers-width 3
        display-line-numbers-widen nil)
  :config
  (set-face-attribute
   'line-number
   nil
   :background (face-background 'default)))

;; highlight keywords
(use-package hl-todo
  :init
  (setq hl-todo-keyword-faces
        '(("TODO" . "#FB4934")
          ("FIXME"  . "#FB4934")
          ("WONT FIX" . "#FB4934")
          ("REVIEW"   . "#FABD2F")
          ("NOTE"   . "#FABD2F")
          ("HACK"   . "#FABD2F")
          ("\\?\\?\\?+" . "#cc9393")))
  :config
  (global-hl-todo-mode))

(provide 'gatsby:theme)
;;; gatsby:theme.el ends here
