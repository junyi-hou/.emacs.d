;;; dev-theme.el -- setting themes -*- lexical-binding: t; -*-


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
        '((nil      . (telephone-line-misc-info-segment
                       telephone-line-major-mode-segment))
          (evil     . (telephone-line-airline-position-segment)))))

;; indentation guide
(use-package highlight-indentation)

(defun dev-set-highlight-stipple ()
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
(defadvice highlight-indentation-mode (before set-highlight-indentation-stipple activate)
"Set the stipple used by indentation highlighting."
  (dev-set-highlight-stipple))

;; load everything

(setq-default custom-safe-themes t)
(set-face-attribute 'default nil
                    :family "monospace"
                    :height 130
                    :weight 'normal
                    :width 'normal)
(add-to-list 'default-frame-alist '(alpha . (100 . 85)))  ; transparency
(load-theme 'zenburn t)  ; theme
(telephone-line-mode 1)  ; status line
(add-hook 'prog-mode-hook 'highlight-indentation-mode)  ; enable indentation hint for all programing works

(provide 'dev-theme)
;;; dev-theme.el ends here
