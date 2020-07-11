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

  :config
  ;; custom mode-line

  (defvar mode-line-current-window (frame-selected-window))

  (defun mode-line-set-current-window (&rest _)
    (when (and (not (minibuffer-window-active-p (frame-selected-window))) ;1
               (not (frame-parameter (selected-frame) 'parent-frame)) ; 2
               ;; 1: not minibufer window
               ;; 2: not child frame
               )
      (setq mode-line-current-window (frame-selected-window))
      (force-mode-line-update)))

  (defun mode-line-unset-current-window ()
    (setq mode-line-current-window nil)
    (force-mode-line-update))

  (defun mode-line-current-window-active-p ()
    (eq mode-line-current-window (selected-window)))

  (add-hook 'window-configuration-change-hook #'mode-line-set-current-window)
  (add-hook 'focus-in-hook #'mode-line-set-current-window)
  (add-hook 'focus-out-hook #'mode-line-unset-current-window)
  (advice-add 'handle-switch-frame :after #'mode-line-set-current-window)

  ;; allow mode line to be inactive when lose focus
  (copy-face 'mode-line 'mode-line-active-face)
  (defun mode-line-focus-out ()
    (copy-face 'mode-line-inactive 'mode-line))
  (defun mode-line-focus-in ()
    (copy-face 'mode-line-active-face 'mode-line))

  (add-hook 'focus-in-hook #'mode-line-focus-in)
  (add-hook 'focus-out-hook #'mode-line-focus-out)
  ;; segments
  (defun mode-line-evil-face ()
    "Return the appropriate color foreground for `evil-mode-line-tag'"
    `(:weight bold
      :foreground
      ,(cond ((not (mode-line-current-window-active-p))
              (face-attribute 'mode-line-inactive :foreground))
             ((or (evil-normal-state-p) (evil-motion-state-p)) "#98C379")
             ((or (evil-insert-state-p)
                  (evil-replace-state-p)
                  (evil-operator-state-p)) "#d16969")
             ((evil-visual-state-p) "#E5C07B")
             ((evil-emacs-state-p) "#C586C0"))))

  (defun mode-line-git-info ()
    "Return the repo name and current branch."
    (if-let ((vc-p (cdr (project-current))))
        (let* ((repo (file-name-nondirectory (directory-file-name vc-p)))
               (branch (substring-no-properties vc-mode 5)))
          (propertize (concat "[" repo "::" branch "]") 'face
                      `(:foreground
                        ,(face-attribute (if (mode-line-current-window-active-p)
                                             'mode-line
                                           'mode-line-inactive)
                                         :foreground))))
      ""))

  (defun mode-line-buf-name ()
    "Patch mode-line-buf-name to have different text property."
    (propertize (buffer-name)
                'face `(:foreground
                        ,(cond
                          ((not (mode-line-current-window-active-p))  ;; not focused
                           (face-attribute 'mode-line-inactive :foreground))
                          ((or (buffer-modified-p)
                               (not (buffer-file-name)))  ;; modified or non-file
                           (face-attribute 'font-lock-warning-face :foreground))
                          (buffer-read-only
                           (face-attribute 'minibuffer-prompt :foreground))
                          (t (face-attribute 'mode-line :foreground))))))

  (defun mode-line-remote-p ()
    (propertize
     (if (file-remote-p default-directory) "[R]" "")
     'face (if (mode-line-current-window-active-p) 'mode-line 'mode-line-inactive)))

  (setq-default mode-line-format
                (list
                 '(:eval (propertize evil-mode-line-tag
                                     'face (mode-line-evil-face)))
                 '(:eval (mode-line-git-info))
                 " "
                 '(:eval (mode-line-remote-p))
                 '(:eval (mode-line-buf-name))
                 ;; align to the right
                 '(:eval (propertize " " 'display
                                     `((space :align-to (- (+ right
                                                              right-fringe
                                                              right-margin-width)
                                                           ,(+ 3 (string-width
                                                                  mode-name)))))))
                 ;; major mode
                 " %m"))


  :general
  (:keymaps '(motion normal visual emacs insert)
   "C-+" 'gatsby:theme-fontsize-up
   "C--" 'gatsby:theme-fontsize-down
   "C-=" 'gatsby:theme-fontsize-reset))

;; indentation guide
(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (LaTeX-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-responsive 'stack))

;; highlight color codes
(use-package rainbow-mode
  :defer t
  :hook (prog-mode . rainbow-mode))

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
