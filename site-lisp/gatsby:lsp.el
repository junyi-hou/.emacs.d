;; gatsby:lsp.el --- LSP settings -*- lexical-binding: t; -*-

;;; Commentary:

;; this file contains settings for language server protocal related settings
;; including autocompletion, snippet, linter and refactoring supports.

;;; Code:
(require 'gatsby:core)

(use-package flymake
  :config
  ;; get rid of the annoying underlines
  (dolist (face '(flymake-note flymake-warning flymake-error))
    (set-face-attribute face nil :underline nil))
  (setq flymake-start-on-newline nil))

(use-package flymake-childframe
  :after flymake
  :straight (flymake-childframe
             :host github
             :repo "junyi-hou/flymake-childframe")
  :hook
	(flymake-mode . flymake-childframe-mode)
	(eglot-managed-mode-hook . flymake-childframe-mode)
  :custom
  (flymake-childframe-position-offset `(0 . ,(default-font-height)))
  (flymake-childframe-delay 2))

(use-package eglot
	:config
	(setf (alist-get 'python-mode eglot-server-programs)
				'("pyright-langserver" "--stdio"))

	;; less invasive symbol highlight
	(defface gatsby:eglot-highlight '((t :underline t)) "Face for highlight symbols")
	(defun gatsby:eglot-eldoc-function ()
		"EGLOT's `eldoc-documentation-function' function."
		(let* ((buffer (current-buffer))
					 (server (eglot--current-server-or-lose))
					 (position-params (eglot--TextDocumentPositionParams))
					 sig-showing
					 (thing-at-point (thing-at-point 'symbol)))
			(cl-macrolet ((when-buffer-window
										 (&body body) ; notice the exception when testing with `ert'
										 `(when (or (get-buffer-window buffer) (ert-running-test))
												(with-current-buffer buffer ,@body))))
				(when (eglot--server-capable :signatureHelpProvider)
					(jsonrpc-async-request
					 server :textDocument/signatureHelp position-params
					 :success-fn
					 (eglot--lambda ((SignatureHelp)
													 signatures activeSignature activeParameter)
						 (when-buffer-window
							(when (cl-plusp (length signatures))
								(setq sig-showing t)
								(eglot--update-doc (eglot--sig-info signatures
                                                    activeSignature
                                                    activeParameter)
                                   thing-at-point))))
					 :deferred :textDocument/signatureHelp))
				(when (eglot--server-capable :hoverProvider)
					(jsonrpc-async-request
					 server :textDocument/hover position-params
					 :success-fn (eglot--lambda ((Hover) contents range)
												 (unless sig-showing
													 (when-buffer-window
														(eglot--update-doc (and (not (seq-empty-p contents))
																										(eglot--hover-info contents
																																			 range))
																							 thing-at-point))))
					 :deferred :textDocument/hover))
				(when (eglot--server-capable :documentHighlightProvider)
					(jsonrpc-async-request
					 server :textDocument/documentHighlight position-params
					 :success-fn
					 (lambda (highlights)
						 (mapc #'delete-overlay eglot--highlights)
						 (setq eglot--highlights
									 (when-buffer-window
										(mapcar
										 (eglot--lambda ((DocumentHighlight) range)
											 (pcase-let ((`(,beg . ,end)
																		(eglot--range-region range)))
												 (let ((ov (make-overlay beg end)))
													 (overlay-put ov 'face 'gatsby:eglot-highlight)
													 (overlay-put ov 'evaporate t)
													 ov)))
										 highlights))))
					 :deferred :textDocument/documentHighlight))))
		eldoc-last-message)
	(advice-add #'eglot-eldoc-function :override #'gatsby:eglot-eldoc-function))

(provide 'gatsby:lsp)
;;; gatsby:lsp.el ends here
