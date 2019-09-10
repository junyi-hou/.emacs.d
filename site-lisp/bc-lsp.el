;; bc-lsp.el --- LSP settings -*- lexical-binding: t; -*-

;;; Commentary:

;; this file contains settings for language server protocal related settings
;; including autocompletion, snippet, linter and refactoring supports.

;;; Code:

(use-package eglot
  :hook
  (python-mode . eglot-ensure)

  :config
  (setq eglot-autoreconnect t
        eglot-put-doc-in-help-buffer t)

  (add-hook 'before-save-hook 'eglot-format nil t)

  ;; TODO: figure out a way to silent eglot
  ;; use delight?
  ;; (defun bc-lsp--silent-eglot (&optional string)
  ;;   "Do not show eglot info with eldoc."
  ;;   (let ((string
  ;;          (if (string-match-p "^\\[eglot\\]" string)
  ;;              ""
  ;;            string)))
  ;;     (apply 'eldoc-message string)))

  ;; :config
  ;; (advice-add 'eldoc-message :before #'bc-lsp--silent-eglot)
  ;; (advice-remove 'eldoc-message #'bc-lsp--silent-eglot)

  :general
  (:keymaps '(normal visual motion)
            :prefix "SPC"
            "rh" 'eglot-posframe-show-help
            "jd" 'eglot-posframe-show-definition
            "jr" 'eglot-posframe-show-reference
            "rn" 'eglot-rename
            "jb" 'bc-lsp-switch-to-previous-buffer))

(use-package eglot-posframe
  :after eglot
  :init
  ;; functions

  ;; taking from
  ;; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
  (defun bc-lsp-switch-to-previous-buffer ()
    "Switch to previously open buffer."
    (interactive)
    (if (ring-empty-p xref--marker-ring)
        (switch-to-buffer (other-buffer (current-buffer) 1))
      (xref-pop-marker-stack)))
  
  :quelpa (eglot-posframe :repo "junyi-hou/eglot-posframe" :fetcher github)
  :config
  ;; fix unpleasant underline in the doc
  (set-face-attribute
   'nobreak-space nil
   :underline nil))

(provide 'bc-lsp)
;;; bc-lsp.el ends here
