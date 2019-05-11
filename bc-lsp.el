;;; bc-lsp.el -- LSP settings -*- lexical-binding: t; -*-

;;; Commentary:

;; this file contains settings for language server protocal related settings
;; including autocompletion, snippet, linter and refactoring supports.

;;; Code:

(use-package flymake
  :config
  ;; get rid of the annoying underlines
  (dolist (face '(flymake-note flymake-warning flymake-error))
    (set-face-attribute face nil
     :underline nil))

  (setq flymake-start-on-newline nil)

  ;; flymake frontend
  (use-package flymake-posframe
    :after flymake
    :load-path "~/Documents/projects/posframe-collection"
    :hook (flymake-mode . flymake-posframe-mode)))

(use-package yasnippet
  :commands yas-minor-mode
  :general
  (:keymaps 'yas-minor-mode-map
            "<tab>" nil
            "TAB" nil
            "M-f" 'yas-expand
            "M-j" 'yas-next-field
            "M-k" 'yas-prev-field))

(use-package company
  :commands company-mode
  :general
  (:keymaps 'company-active-map
   "<tab>" 'bc-lsp-unified-tab
   "M-j" 'company-select-next
   "M-k" 'company-select-previous-or-abort
   "M-J" 'company-next-page
   "M-K" 'company-previous-page)

  :config
  (add-hook 'company-mode-hook #'yas-minor-mode)
  (setq company-idle-delay nil
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-limit 10
        company-selection-wrap-around t
        company-backends '((company-capf
                            company-yasnippet
                            company-files)
                           (company-dabbrev
                            company-abbrev))))

(use-package company-posframe
  :after company
  :commands company-posframe-mode
  :hook (company-mode . company-posframe-mode))

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

  :general
  (:keymaps '(normal visual motion)
   :prefix "SPC"
   "eh" 'lsp-ui-doc-show
   "en" 'lsp-rename
   "jd" 'lsp-ui-peek-find-definitions
   "jr" 'lsp-ui-peek-find-references
   "jb" 'bc-lsp-switch-to-previous-buffer))

(use-package company-lsp
  :defer t
  :commands company-lsp)

;; (use-package lsp-doc-posframe
;;   :after lsp-mode
;;   :load-path "~/Documents/projects/posframe-collection"
;;   :hook (lsp-mode . lsp-doc-posframe-mode))

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

(defun bc-lsp-unified-tab ()
  "Use tab for both company and indentation.

In insert mode, first try `company-manual-begin'.  If there is no completion available at point, indent the current line by `tab-width' length."
  (interactive)
  (if (looking-back "^[ \t]*" (line-beginning-position))
      (dotimes (n tab-width) (insert " "))
    (company-manual-begin)
    (if company-candidates
        (if (or company-selection-changed
                (memq last-command '(company-complete-common
                                     bc-lsp-unified-tab)))
            (call-interactively 'company-complete-selection)
          (call-interactively 'company-complete-common)
          (when company-candidates
            (setq this-command 'company-complete-common)))
      (indent-region (line-beginning-position) (line-end-position)))))


;; taking from
;; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun bc-lsp-switch-to-previous-buffer ()
  "Switch to previously open buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(provide 'bc-lsp)
;;; bc-lsp.el ends here
