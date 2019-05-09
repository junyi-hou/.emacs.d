;;; bc-theme.el -- setting themes -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; theme
(use-package zenburn-theme)

;; mode line
(use-package telephone-line
  :init
  (setq telephone-line-lhs
        '((evil     . (telephone-line-evil-tag-segment))
          (accent   . (telephone-line-vc-segment
                       telephone-line-erc-modified-channels-segment))
          (nil      . (telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil      . (telephone-line-misc-info-segment))
          (evil     . (telephone-line-major-mode-segment)))))

;; indentation guide
(use-package highlight-indentation)

(defun bc-theme--set-highlight-stipple ()
  "Define custom stipple for highlight-indentation.  See https://github.com/antonj/Highlight-Indentation-for-Emacs/issues/16."
  (let* ((char-width (frame-char-width (selected-frame)))
        (hl-stipple (if (> char-width 8)
                        (list char-width 4 (string 16 0 0 0 0 0 0 0))
                      (list char-width 4 (string 16 0 0 0)))))
    (set-face-attribute 'highlight-indentation-face nil
                        :stipple hl-stipple
                        :inherit nil)
    (set-face-attribute 'highlight-indentation-current-column-face nil
                        :stipple hl-stipple
                        :inherit nil
                        :foreground "yellow")))

;; Patch highlight-indentation-mode to set/update a stipple attribute
(defadvice highlight-indentation-mode
    (before set-highlight-indentation-stipple activate)
  "Set the stipple used by indentation highlighting."
  (bc-theme--set-highlight-stipple))

;; line numbers
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'visual
                display-line-numbers-current-absolute t
                display-line-numbers-width 2
                display-line-numbers-widen t))

;; display time
(display-time-mode)

;; highlight keywords
(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . "#cc9393")
          ("NOTE"   . "#d0bf8f")
          ("HACK"   . "#d0bf8f")
          ("TEMP"   . "#d0bf8f")
          ("DONE"   . "#afd8af")
          ("FIXME"  . "#cc9393")
          ("\\?\\?\\?+" . "#cc9393")))
  (global-hl-todo-mode))

;; load everything

(setq-default custom-safe-themes t)
(setq-default left-fringe-width 8)

(set-face-attribute 'default nil
                    :family "monospace"
                    :height 130
                    :weight 'normal
                    :width 'normal)
(add-to-list 'default-frame-alist '(alpha . (100 . 85)))  ; transparency
(load-theme 'zenburn t)  ; theme
(telephone-line-mode 1)  ; status line
(add-hook 'prog-mode-hook 'highlight-indentation-mode)  ; enable indentation hint for all programming modes

(provide 'bc-theme)
;;; bc-theme.el ends here
