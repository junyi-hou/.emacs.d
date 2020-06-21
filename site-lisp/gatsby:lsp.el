;; gatsby:lsp.el --- LSP settings -*- lexical-binding: t; -*-

;;; Commentary:

;; this file contains settings for language server protocal related settings
;; including autocompletion, snippet, linter and refactoring supports.

;;; Code:
(require 'gatsby:core)

(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  (python-mode . gatsby:lsp--reformat-buffer)
  (rust-mode . eglot-ensure)
  (rust-mode . gatsby:lsp--reformat-buffer)
  (eglot-managed-mode . gatsby:lsp--turn-off-doc-at-point)

  :init
  (defun gatsby:lsp--reformat-buffer ()
    "Run `eglot-format' before save in the eglot powered buffer."
    (add-hook 'before-save-hook 'eglot-format nil t))

  (defun gatsby:lsp--turn-off-doc-at-point ()
    "Do not show documentation of symbol-at-point."
    (eldoc-mode -1))

  ;; integration with eldoc-box
  (with-eval-after-load 'eldoc-box
    (progn
      (defconst gatsby:lsp-help-buffer-keymap
        (let ((map (make-sparse-keymap)))
          (suppress-keymap map t)
          (define-key map (kbd "J") #'gatsby:lsp-help-scroll-down)
          (define-key map (kbd "K") #'gatsby:lsp-help-scroll-up)
          (define-key map (kbd "C-g") #'gatsby:lsp-quit-help-frame)
          map)
        "Keymap in `gatsby:lsp-help-at-point'")

      (defvar-local gatsby:lsp-control-deactivate-fn nil
        "non-nil if current buffer can be controlled by `gatsby:lsp-help-buffer-keymap'.")

      (defun gatsby:lsp-quit-help-frame ()
        "Hide childframe, set `gatsby:lsp-control' to nil."
        (interactive)
        (when eldoc-box--frame
          (make-frame-invisible eldoc-box--frame t)
          (with-current-buffer eldoc-box--buffer
            (when-let ((fn gatsby:lsp-control-deactivate-fn))
              (setq-local gatsby:lsp-control-deactivate-fn nil)
              (funcall fn)))))

      (advice-add #'eldoc-box-quit-frame :override #'gatsby:lsp-quit-help-frame)

      (defun gatsby:lsp-help-scroll-up ()
        "Scroll up in `eldoc-box--frame'"
        (interactive)
        (with-current-buffer eldoc-box--buffer
          (when gatsby:lsp-control-deactivate-fn
            (with-selected-frame eldoc-box--frame
              (scroll-down 5)))))

      (defun gatsby:lsp-help-scroll-down ()
        "Scroll down in `eldoc-box--frame'"
        (interactive)
        (with-current-buffer eldoc-box--buffer
          (when gatsby:lsp-control-deactivate-fn
            (with-selected-frame eldoc-box--frame
              (scroll-up 5)))))

      (defun gatsby:lsp-help-at-point ()
        "display help in a childframe at point"
        (interactive)
        (unless eglot--managed-mode
          (user-error "`eglot' not enabled"))
        (unless (featurep 'eldoc-box)
          (user-error "`eldoc-box' not enabled"))
        (let ((eldoc-box-position-function
               #'eldoc-box--default-at-point-position-function)
              (eldoc-box-max-pixel-height
               (lambda ()
                 (- (nth 3 (window-absolute-body-pixel-edges))
                    (cdr (window-absolute-pixel-position))
                    40))))
          (eldoc-box--display
           (eglot--dbind ((Hover) contents range)
               (jsonrpc-request
                (eglot--current-server-or-lose)
                :textDocument/hover
                (eglot--TextDocumentPositionParams))
             (when (seq-empty-p contents) (eglot--error "No hover info here"))
             (eglot--hover-info contents range))))
        (with-current-buffer eldoc-box--buffer
          (setq-local gatsby:lsp-control-deactivate-fn
                      (set-transient-map gatsby:lsp-help-buffer-keymap
                                         t #'gatsby:lsp-quit-help-frame)))))
    )

  :custom
  (eglot-autoreconnect t)
  (eglot-put-doc-in-help-buffer t)
  (eglot-stay-out-of '(company)))

(provide 'gatsby:lsp)
;;; gatsby:lsp.el ends here
