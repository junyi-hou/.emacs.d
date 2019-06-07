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
   "rh" 'bc-lsp-help-show
   "jd" 'bc-lsp-definition-show
   "rn" 'eglot-rename
   "jb" 'bc-lsp-switch-to-previous-buffer))


;; functions

(defconst bc-lsp-posframe-frame "*bc-lsp-posframe*")

(defvar bc-lsp-posframe-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'bc-lsp-posframe-hide)
    (define-key map (kbd "<escape>") 'bc-lsp-posframe-hide)
    (define-key map (kbd "C-d") 'bc-lsp-posframe-scroll-down)
    (define-key map (kbd "C-u") 'bc-lsp-posframe-scroll-up)
    (define-key map (kbd "J") 'bc-lsp-posframe-scroll-down)
    (define-key map (kbd "K") 'bc-lsp-posframe-scroll-up)
    (define-key map (kbd "<RET>") 'bc-lsp-posframe-enter)
    map)
  "Keymap for controlling posframes.")

;; taking from
;; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun bc-lsp-switch-to-previous-buffer ()
  "Switch to previously open buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun bc-lsp-posframe-hide ()
  "Hide posframe."
  (interactive)
  (posframe-control-hide bc-lsp-posframe-frame))

(defun bc-lsp-posframe-scroll-down ()
  "Scroll down posframe."
  (interactive)
  (posframe-control-command
   bc-lsp-posframe-frame
   :command 'scroll-up
   (max 1 (/ (1- (window-height (selected-window))) 2))))

(defun bc-lsp-posframe-scroll-up ()
  "Scroll down posframe."
  (interactive)
  (posframe-control-command
   bc-lsp-posframe-frame
   :command 'scroll-down
   (max 1 (/ (1- (window-height (selected-window))) 2))))

(defun bc-lsp-posframe-enter ()
  "Hide posframe and pop to the buffer displayed in the posframe."
  (interactive)
  (xref--show-xrefs (bc-lsp--get-xref-list 'definitions) nil)
  (bc-lsp-posframe-hide))


(defun bc-lsp-help-show ()
  "Update variable `eglot--help-buffer' with helps of `symbol-at-point' and display it in a `posframe' frame."
  (interactive)
  (eglot--dbind ((Hover) contents range)
      (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                       (eglot--TextDocumentPositionParams))
    (when (seq-empty-p contents) (eglot--error "No hover info here"))
    (when (posframe-workable-p)
      (let* ((blurb (eglot--hover-info contents range))
             (sym (thing-at-point 'symbol))
             (total-height (min 40 (/ (length blurb) 80))))

        (posframe-show
         bc-lsp-posframe-frame
         :poshandler (lambda (_info) '(-1 . 0))
         :string blurb
         :internal-border-width 3
         :internal-border-color "gray80"
         :left-fringe 1
         :right-fringe 1
         :width 80
         :min-width 80
         :height total-height
         :control-keymap bc-lsp-posframe-keymap
         :hide-fn 'bc-lsp-posframe-hide)))))

(defun bc-lsp--get-xref-list (kind)
  "Get a list of xref references item of KIND (e.g., definition, reference, etc.)."
  (let* ((backend (xref-find-backend))
         (id (xref-backend-identifier-at-point backend))
         (xrefs (funcall (intern (format "xref-backend-%s" kind))
                         (xref-find-backend)
                         id)))
    (if xrefs
        xrefs
      (user-error (format "No %s found at point" kind)))))


(defun bc-lsp-definition-show ()
  "Show definition-at-point in a posframe."
  (interactive)
  (let ((def (bc-lsp--get-xref-list 'definitions)))

    (posframe-show
     bc-lsp-posframe-frame
     :poshandler 'posframe-poshandler-point-bottom-left-corner
     :internal-border-width 3
     :internal-border-color "gray80"
     :left-fringe 1
     :right-fringe 1
     :width 70
     :min-width 70
     :height 15
     :control-keymap bc-lsp-posframe-keymap
     :hide-fn 'bc-lsp-posframe-hide)
    (with-current-buffer bc-lsp-posframe-frame
      (when (framep posframe--frame)
        (with-selected-frame posframe--frame
          (xref--show-xrefs def nil))))))

;; TODO:
;; also, take advantage that in a child frame, there could be multiple windows
;; to write a lsp-ui like reference function


(provide 'bc-lsp)
;;; bc-lsp.el ends here
