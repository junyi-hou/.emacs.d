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
                                   "--plus-color=#006910"
                                   "--plus-emph-color=#00b300"
                                   "--color-only")))

  ;; increase active/inactive mode-line contrast
  (defvar gatsby:modeline-fg "#fafafa")
  (defvar gatsby:modeline-bg "#00538a")
  (defvar gatsby:modeline-fg-inactive "#666666")
  (defvar gatsby:modeline-bg-inactive "#002c4a")

  (set-face-foreground 'mode-line gatsby:modeline-fg)
  (set-face-background 'mode-line gatsby:modeline-bg)
  (set-face-foreground 'mode-line-inactive gatsby:modeline-fg-inactive)
  (set-face-background 'mode-line-inactive gatsby:modeline-bg-inactive)

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

(use-package simple-modeline
  :hook (after-init . simple-modeline-mode)
  :config
  (require 'battery)
  (require 'time)

  (simple-modeline-create-segment
   "modified-p"
   "Displays a color-coded buffer modification/read-only indicator in the mode-line."
   (if (not (string-match-p "\\*.*\\*" (buffer-name)))
       (let ((read-only (and buffer-read-only (buffer-file-name)))
             (modified (buffer-modified-p)))
         (propertize (cond (read-only " %")
                           (modified  " ●")
                           (t         " ○"))
                     'face `(:inherit
                             ,(cond (read-only 'simple-modeline-status-modified)
                                    (modified 'simple-modeline-unimportant)))))))

  (simple-modeline-create-segment
   "evil"
   "Indicator for evil state."
   (cond ((evil-insert-state-p)
          (propertize " <I>"
                      'face `(:inherit 'simple-modeline-unimportant
                              :foreground "#E06C75")))
         ((evil-normal-state-p)
          (propertize " <N>"
                      'face `(:inherit 'simple-modeline-unimportant
                              :foreground "#79d177")))
         ((evil-visual-state-p)
          (propertize " <V>"
                      'face `(:inherit 'simple-modeline-unimportant
                              :foreground "#DCDCAA")))
         ((evil-motion-state-p)
          (propertize " <M>"
                      'face `(:inherit 'simple-modeline-unimportant
                              :foreground "#61AFEF")))
         ((evil-operator-state-p)
          (propertize " <O>"
                      'face `(:inherit 'simple-modeline-unimportant
                              :foreground "#61AFEF")))
         ((evil-emacs-state-p)
          (propertize " <E>"
                      'face `(:inherit 'simple-modeline-unimportant
                              :foreground "#C678DD")))))

  (setq simple-modeline--mode-line
        '((:eval
           (simple-modeline--format
            '(simple-modeline-segment-evil
              simple-modeline-segment-modified-p
              simple-modeline-segment-buffer-name)
            '(simple-modeline-segment-vc
              simple-modeline-segment-major-mode)))))
  )

(battery-format "%L" (funcall battery-status-function))

;; indentation guide
(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (LaTeX-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-responsive 'stack))

;; visual cue of the cursor position
(use-package beacon
  ;; use chep's fork so it can grow backwards
  :straight (beacon :host github :repo "junyi-hou/beacon")
  :custom
  (beacon-blink-when-window-scrolls nil)
  (beacon-can-go-backwards t)
  (beacon-size 15)
  :config
  (beacon-mode 1))

;; line numbers
(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  (org-mode . display-line-numbers-mode)
  (LaTeX-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'visual)
  (display-line-numbers-current-absolute t)
  (display-line-numbers-width-start t)
  :config
  (set-face-attribute
   'line-number
   nil
   :background (face-background 'default)))

;; highlight keywords
(use-package hl-todo
  :custom
  (hl-todo-keyword-faces
   '(("TODO" . "#FB4934")
     ("FIXME"  . "#FB4934")
     ("WONT FIX" . "#FB4934")
     ("REVIEW"   . "#FABD2F")
     ("NOTE"   . "#FABD2F")
     ("HACK"   . "#FABD2F")
     ("\\?\\?\\?+" . "#cc9393")))
  :config
  (global-hl-todo-mode 1))

(provide 'gatsby:theme)
;;; gatsby:theme.el ends here
