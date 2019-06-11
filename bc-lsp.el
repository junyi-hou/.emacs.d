;;; bc-lsp.el -- LSP settings -*- lexical-binding: t; -*-

;;; Commentary:

;; this file contains settings for language server protocal related settings
;; including autocompletion, snippet, linter and refactoring supports.

;;; Code:

(require 'bc-company)
(require 'bc-flymake)
;; (require 'bc-eldoc)

(use-package eglot
  :init
  (setq eglot-autoreconnect t
        eglot-put-doc-in-help-buffer t)

  :config
  ;; load my doc-viewer

  (use-package eglot-posframe
    :after eglot
    :quelpa (eglot-posframe :repo "junyi-hou/posframe-collection" :fetcher github)
    :config
    ;; fix unpleasant underline in the doc
    (set-face-attribute
     'nobreak-space nil
     :underline nil))

  :general
  (:keymaps '(normal visual motion)
   :prefix "SPC"
   "rh" 'eglot-posframe-show-help
   "jd" 'eglot-posframe-show-definition
   "rn" 'eglot-rename
   "jb" 'bc-lsp-switch-to-previous-buffer))



;; functions

;; taking from
;; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun bc-lsp-switch-to-previous-buffer ()
  "Switch to previously open buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


(provide 'bc-lsp)
;;; bc-lsp.el ends here
