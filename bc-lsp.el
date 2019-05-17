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
   "jd" 'lsp-find-definition
   "rn" 'lsp-rename
   "jb" 'bc-lsp-switch-to-previous-buffer))

(use-package company-lsp
  :after lsp-mode
  :commands company-lsp)


;; taking from
;; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun bc-lsp-switch-to-previous-buffer ()
  "Switch to previously open buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(provide 'bc-lsp)
;;; bc-lsp.el ends here
