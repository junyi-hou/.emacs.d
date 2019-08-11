;;; bc-theme.el --- setting themes -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; theme
(use-package color-theme-sanityinc-tomorrow
  :init
  (setq-default custom-safe-themes t)
  :config
  (color-theme-sanityinc-tomorrow-night))

;; mode line
(use-package telephone-line
  :init
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
     :stipple hl-stipple
     :inherit nil)))

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
  :defer 10
  :init
  (setq display-time-load-average-threshold 0.9
        display-time-24hr-format t)
  :config
  (display-time-mode))

;; highlight keywords
(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . "#cc9393")
          ("NOTE"   . "#d0bf8f")
          ("HACK"   . "#d0bf8f")
          ("FIXME"  . "#cc9393")
          ("\\?\\?\\?+" . "#cc9393")))
  (global-hl-todo-mode))

;; load everything

(setq-default left-fringe-width 8)

;; font

(set-face-attribute
 'default nil
 :family "monospace"
 :height 130
 :weight 'normal
 :width 'normal)

(add-to-list 'default-frame-alist '(alpha . (100 . 85)))  ; transparency

(provide 'bc-theme)
;;; bc-theme.el ends here
