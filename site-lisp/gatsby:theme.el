;;; gatsby:theme.el --- setting themes -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby:core)

;; theme
(use-package vscode-dark-plus-theme
  :straight
  (vscode-dark-plus-theme
   :repo "ianpan870102/vscode-dark-plus-emacs-theme"
   :host github)
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
   :width 'normal
   :background "#1e1e1e")

  (set-face-attribute
   'minibuffer-prompt
   nil
   :weight 'normal)

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
;; manually call when needed
(use-package rainbow-mode :defer t)

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
     ("DONE" . "#98C379")
     ("\\?\\?\\?+" . "#cc9393")))
  :config
  (global-hl-todo-mode 1))

(use-package battery)

(use-package mini-modeline
	:straight (mini-modeline :repo "kiennq/emacs-mini-modeline" :host github)
	:custom
	(mini-modeline-face-attr `(:background ,(face-attribute 'default :background)))
	(mini-modeline-echo-duration 1)
	(mini-modeline-l-format
	 '((:eval (propertize evil-mode-line-tag 'face (mode-line-evil-face)))
		 (:eval (if (file-remote-p default-directory) "[R] " " "))
		 (:eval (if-let ((proj (project-current))
                     vc-mode)
                (concat "["
                        (file-name-nondirectory (directory-file-name (cdr proj)))
                        "::"
                        (substring-no-properties vc-mode 5)
                        "] ")))
		 (:eval (mode-line-buf-name))))
	(mini-modeline-r-format
	 '((:eval (mode-line-battery))
		 (:eval (format-time-string "%b %d%l:%M %p"))))
	:config

	(defface mode-line-evil-normal-face
		'((t :inherit default :foreground "#98C379" :weight bold))
		"Face for normal and motion state")

	(defface mode-line-evil-modifying-face
		'((t :inherit default :foreground "#d16969" :weight bold))
		"Face for insert, replace and operator state")

	(defface mode-line-evil-visual-face
		'((t :inherit default :foreground "#E5C07B" :weight bold))
		"Face for visual and visual line state")

	(defface mode-line-evil-emacs-face
		'((t :inherit default :foreground "#C586C0" :weight bold))
		"Face for emacs state")

	(defconst mode-line-evil-faces
		'((normal . mode-line-evil-normal-face)
			(motion . mode-line-evil-normal-face)
			(insert . mode-line-evil-modifying-face)
			(replace . mode-line-evil-modifying-face)
			(operator . mode-line-evil-modifying-face)
			(visual . mode-line-evil-visual-face)
			(emacs . mode-line-evil-emacs-face))
		"Mapping between `evil-state' and the mode-line faces.")

	(defun mode-line-evil-face ()
		"Return the appropriate color foreground for `evil-mode-line-tag'"
		(alist-get evil-state mode-line-evil-faces))

	(defun mode-line-buf-name ()
		"Patch mode-line-buf-name to have different text property."
		(propertize (buffer-name)
								'face `(:foreground
												,(cond ((or (buffer-modified-p)
																		(not (buffer-file-name)))  ;; modified or non-file
																(face-attribute 'font-lock-warning-face :foreground))
															 (buffer-read-only
																(face-attribute 'minibuffer-prompt :foreground))
															 (t (face-attribute 'mode-line :foreground))))))

	(defun mode-line-battery ()
		(let* ((bat (funcall battery-status-function))
					 (percent (battery-format "%p" bat))
					 (status (battery-format "%L" bat)))
			(propertize (format "%s%% (%s) " percent status)
									'face
									(if (or (string= status "AC")
													(> (string-to-number percent) 15))
											'default 'font-lock-warning-face))))

	(set-face-attribute 'mini-modeline-mode-line nil
											:background "#00538a")
	(set-face-attribute 'mini-modeline-mode-line-inactive nil
											:background "#002c4a")
	
	(mini-modeline-mode 1))

(provide 'gatsby:theme)
;;; gatsby:theme.el ends here
