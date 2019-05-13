;;; bc-lsp.el -- LSP settings -*- lexical-binding: t; -*-

;;; Commentary:

;; this file contains settings for language server protocal related settings
;; including autocompletion, snippet, linter and refactoring supports.

;;; Code:

(require 'bc-company)
(require 'bc-flymake)

(use-package lsp-mode
  :config
  (setq lsp-prefer-flymake t
        lsp-enable-indentation t
        lsp-auto-guess-root t
        lsp-enable-symbol-highlighting nil)

  ;; do not mess with my company-backends
  (advice-add
   'lsp--auto-configure
   :after
   (defun remove-autoadded-backends ()
     (setq company-backends (cdr company-backends))))

  ;; load my doc-viewer
  (use-package lsp-doc-posframe
    :after lsp-mode
    :load-path "~/Documents/projects/posframe-collection"
    :init
    ;; fix unpleasant underline in the doc
    (set-face-attribute
     'nobreak-space nil
     :underline nil))

  :general
  (:keymaps '(normal visual motion)
   :prefix "SPC"
   "rh" 'lsp-doc-posframe-show
   "rn" 'lsp-rename
   "jd" 'lsp-ui-peek-find-definitions
   "jr" 'lsp-ui-peek-find-references
   "jb" 'bc-lsp-switch-to-previous-buffer))

(use-package company-lsp
  :after lsp-mode
  :commands company-lsp)


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  ;; I only need peek
  (setq lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-doc-enable nil

        lsp-ui-peek-list-width 20)

  :config
  ;; set faces for lsp-ui-peek
  ;; 
  (face-spec-set 'lsp-ui-peek-header
   '((((background light)) :background "#6F6F6F" :foreground "#FFFFEF")
    (t :background "#6F6F6F" :foreground "#FFFFEF")))

  (face-spec-set 'lsp-ui-peek-filename
   '((((background light)) :foreground "#CC9393")
    (t :foreground "#CC9393")))
   
  (face-spec-set 'lsp-ui-peek-list
  '((((background light)) :background "#383838")
    (t :background "#383838")))

  (face-spec-set 'lsp-ui-peek-peek
  '((((background light)) :background "#383838")
    (t :background "#383838")))

  (face-spec-set 'lsp-ui-peek-highlight
   '((((background light)) :background "dim gray"
      :foreground "dim gray"
      :distant-foreground "black")
     (t :background "#383838"
        :foreground "#6F6F6F"
        :distant-foreground "white"
        :box (:line-width -1 :color "#DCDCCC"))))

  (face-spec-set 'lsp-ui-peek-selection
    '((((background light)) :background "#6F6F6F" :foreground "#DCDCCC")
      (t :background "#DCDCDC" :foreground "#6F6F6F")))

  (set-face-attribute
   'lsp-ui-peek-line-number nil
   :foreground "#D0BF8F")

  :general
  (:keymaps 'lsp-ui-peek-mode-map
  "j" 'lsp-ui-peek--select-next
  "J" 'lsp-ui-peek--select-next-file
  "k" 'lsp-ui-peek--select-prev
  "K" 'lsp-ui-peek--select-prev-file
  "<tab>" 'lsp-ui-peek--goto-xref
  "<enter>" 'lsp-ui-peek--goto-xref-other-window))


;; functions

;; taking from
;; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun bc-lsp-switch-to-previous-buffer ()
  "Switch to previously open buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(provide 'bc-lsp)
;;; bc-lsp.el ends here
