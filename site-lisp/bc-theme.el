;;; bc-theme.el --- setting themes -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; theme
(use-package gruvbox-theme
  :init
  (setq custom-safe-themes t)
  (setq-default left-fringe-width 8)

  (defun bc-theme-fontsize-up (&optional size)
    "Increase the font size in the current frame by SIZE.  If SIZE is nil, default to 5."
    (interactive)
    (let* ((current-size (plist-get (custom-face-attributes-get 'default nil) :height))
           (new-size (+ (or size 5) current-size)))
      (set-face-attribute
       'default (selected-frame)
       :height new-size)))

  (defun bc-theme-fontsize-down (&optional size)
    "Decrease the font size in the current frame by SIZE.  If SIZE is nil, default to 5."
    (interactive)
    (let* ((current-size (plist-get (custom-face-attributes-get 'default nil) :height))
           (new-size (- current-size (or size 5))))
      (set-face-attribute
       'default (selected-frame)
       :height new-size)))

  (defun bc-theme-fontsize-reset ()
    "Reset the current frame font size."
    (interactive)
    (set-face-attribute
     'default (selected-frame) :height 150))

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

  :general
  (:keymaps '(motion normal visual emacs insert)
   "C-+" 'bc-theme-fontsize-up
   "C--" 'bc-theme-fontsize-down
   "C-=" 'bc-theme-fontsize-reset))

;; display battery level
(use-package battery
  :config
  (setq battery-mode-line-limit 100)
  (display-battery-mode))

;; display time
(use-package time
  :config
  (setq display-time-default-load-average nil
        display-time-24hr-format t)
  (display-time-mode))

(use-package doom-modeline
  :init
  (setq doom-modeline-project-detection 'project
        doom-modeline-buffer-file-name-style 'relative-to-project
        ;; if only I can disable all-the-icon dependency...
        doom-modeline-icon nil
        doom-modeline-vcs-max-length 20)

  :config
  (doom-modeline-mode 1)

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

  (with-eval-after-load 'exwm
    (defvar bc-theme-exwm-title-max-length 60)

    (doom-modeline-def-segment exwm-title
      "exwm buffer title, truncated if too long."
      (if-let ((title (buffer-name))
               ((> (length title) bc-theme-exwm-title-max-length)))
          (concat (substring title 0 bc-theme-exwm-title-max-length) "...")
        title))

    (doom-modeline-def-modeline 'exwm
      '(modals exwm-title) '(major-mode time-battery))

    (defun bc-theme-set-exwm-modeline ()
      (doom-modeline-set-modeline 'exwm))

    (add-hook 'exwm-manage-finish-hook #'bc-theme-set-exwm-modeline))

  ;;; ===============================
  ;;  display time-battery only at the
  ;;  bottom right window
  ;;; ===============================

  (defun bc-theme--doom-modeline (key-or-buffer)
    "Return the doom-modeline structure for KEY-OR-BUFFER."
    (if (bufferp key-or-buffer)
        (thread-last key-or-buffer
          (buffer-local-value 'mode-line-format)
          cadr
          cadr
          eval)
      (thread-last key-or-buffer
        (format "doom-modeline-format--%s")
        intern
        list
        eval)))

  (defun bc-theme--append-segment (structure segment)
    "Return doom-modeline structure with SEGMENT appended to STRUCTURE."
    (let ((lhs (seq-take structure 2))
          (rhs (nth 2 structure))
          (segment (thread-last segment
                     symbol-name
                     (format "doom-modeline-segment--%s")
                     intern
                     list)))
      (append lhs (list (append rhs `((:eval ,segment)))))))

  (defun bc-theme--modify-modeline (buffer)
    "Append `time-battery' to the modeline if BUFFER's window is at the bottom right corner, otherwise reset its modeline."
    (interactive)
    )


  (defun bc-theme--rearrange-modeline (&rest _)
    "When window config changes, loop over list of windows, append `time-battery'
    segment to the modeline of the bottom right window while reset all other window
    to their default config."
    )


  )

;; indentation guide
(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (LaTeX-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\┆
        highlight-indent-guides-responsive 'stack))

;; visual cue of the cursor position
(use-package beacon
  ;; use chep's fork so it can grow backwards
  :straight (beacon :host github :repo "chep/beacon")
  :config
  (setq beacon-blink-when-window-scrolls nil
        beacon-can-go-backwards t
        beacon-size 15)
  (beacon-mode 1))

;; line numbers
(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  (org-mode . display-line-numbers-mode)
  (LaTeX-mode . display-line-numbers-mode)
  :config

  (set-face-attribute
   'line-number
   nil
   :background (face-background 'default))

  (setq-default
   display-line-numbers-type 'visual
   display-line-numbers-current-absolute t
   display-line-numbers-width 3
   display-line-numbers-widen nil))
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

(provide 'bc-theme)
;;; bc-theme.el ends here
