;;; bc-ide-lisp.el --- setup ide features (auto-completion, code-jump, linter) for lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; for code jump
;; just need posframe-control.el
;; (require 'eglot-posframe)

;; (defconst lisp-posframe-reference-buffer "*lisp-posframe-reference*")
;; (defconst lisp-posframe-control-buffer "*lisp-posframe-reference-xref*")

;; ;; create a posframe, populate the buffer using `describe-symbol'
;; ;; control: back/forward/enter the file
;; (defun lisp-posframe-show-help ()
;;   "docstring"
;;   (interactive)
;;   (posframe-show
;;    lisp-posframe-reference-buffer
;;    :poshandler 'posframe-poshandler-point-bottom-left-corner
;;    :internal-border-width 3
;;    :internal-border-color "gray80"
;;    :left-fringe 1
;;    :right-fringe 1
;;    :width 85
;;    :min-width 85
;;    :height 35
;;    ))

(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local tab-width 2)))

(provide 'bc-ide-lisp)
;;; bc-ide-lisp.el ends here
