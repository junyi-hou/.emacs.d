;;; bc-lsp.el -- LSP settings -*- lexical-binding: t; -*-

;;; Commentary:

;; this file contains settings for language server protocal related settings
;; including autocompletion, snippet, linter and refactoring supports.

;;; Code:

(use-package flymake
  :config
  ;; get rid of the annoying underlines
  (dolist (face '(flymake-note flymake-warning flymake-error))
    (face-spec-set
     face
     '((t :underline nil))))

  (setq flymake-start-on-newline nil)

  ;; hover info
  (use-package flymake-posframe
    :after posframe
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
   (lambda ()
     (setq company-backends (cdr company-backends))))

  :general
  (:keymaps '(normal visual motion)
   :prefix "SPC"
   "eh" 'bc-lsp-help
   "en" 'lsp-rename
   "jd" 'lsp-find-definition
   "jr" 'lsp-find-references
   "jj" 'ivy-resume
   "jb" 'bc-lsp-switch-to-previous-buffer))

(use-package company-lsp
  :defer t
  :commands company-lsp)


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

;; (defun bc-lsp-help ()
;;   "Wrapper around `lsp-describe-thing-at-point'.

;; Call `lsp-describe-thing-at-point' to update `*lsp-help*' buffer, then
;; display the help using `posframe'."
;;   (interactive)
;;   (lsp-describe-thing-at-point)
;;   (delete-window)
;;   (posframe-show
;;    (get-buffer "*lsp-help*")
;;    :))

(provide 'bc-lsp)
;;; bc-lsp.el ends here
