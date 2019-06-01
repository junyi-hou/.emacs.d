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
  (use-package posframe-control
    :after eglot
    :load-path "~/Documents/projects/posframe-collection"
    :init
    ;; fix unpleasant underline in the doc
    (set-face-attribute
     'nobreak-space nil
     :underline nil))

  :general
  (:keymaps '(normal visual motion)
   :prefix "SPC"
   "rh" 'eglot-posframe-doc-show
   "jd" 'xref-find-definitions
   "rn" 'eglot-rename
   "jb" 'bc-lsp-switch-to-previous-buffer))


;; taking from
;; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun bc-lsp-switch-to-previous-buffer ()
  "Switch to previously open buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun eglot-posframe-doc-show ()
  "Update `eglot--help-buffer' with helps of `symbol-at-point' and display it in a `posframe' frame."
  (interactive)
  (eglot--dbind ((Hover) contents range)
      (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                       (eglot--TextDocumentPositionParams))
    (when (seq-empty-p contents) (eglot--error "No hover info here"))
    (when (posframe-workable-p)
      (let* ((blurb (eglot--hover-info contents range))
             (sym (thing-at-point 'symbol))
             (total-height (min 40 (/ (length blurb) 80))))

        (posframe-control-show
         (eglot--help-buffer)
         :poshandler (lambda (_info) '(-1 . 0))
         :string blurb
         :internal-border-width 3
         :internal-border-color "gray80"
         :left-fringe 1
         :right-fringe 1
         :width 80
         :min-width 80
         :height total-height)))))

(defun eglot-posframe-def-show ()
  "Show the context of definition in a `posframe' frame."
  (interactive)
  ()
  )


(provide 'bc-lsp)
;;; bc-lsp.el ends here
