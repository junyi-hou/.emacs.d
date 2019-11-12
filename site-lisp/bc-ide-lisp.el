;;; bc-ide-lisp.el --- setup ide features (auto-completion, code-jump, linter) for lisp -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO: drop posframe dependency
;; 1. do not directly operate on the help buffer
;; 2. write a wrapper over help-go-back/forward so can do that in childframe

;;; Code:

(use-package posframe-control
  :straight
  (posframe-control
   :repo "junyi-hou/posframe-control"
   :host github))

(defconst bc-ide-lisp-help-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "J") (lambda () (interactive)
                                (posframe-control-next-three-lines
                                 (help-buffer))))
    (define-key map (kbd "K") (lambda () (interactive)
                                (posframe-control-prev-three-lines
                                 (help-buffer))))
    (define-key map (kbd "C-d") (lambda () (interactive)
                                  (posframe-control-scroll-down
                                   (help-buffer))))
    (define-key map (kbd "C-u") (lambda () (interactive)
                                  (posframe-control-scroll-up
                                   (help-buffer))))
    (define-key map (kbd "RET") #'bc-ide-lisp-goto-help-buffer)
    (define-key map (kbd "C-<return>") #'bc-ide-lisp-goto-def)
    map)
  "Keymap for controlling help posframes.")

(defun bc-ide-lisp-goto-def ()
  "Goto definition found by the help buffer."
  (interactive)
  (with-current-buffer (help-buffer)
    (button-activate (button-at (next-char-property-change 1))))
  (posframe-control-hide (help-buffer)))

(defun bc-ide-lisp-goto-help-buffer ()
  "Open the help buffer in a new window."
  (interactive)
  (switch-to-buffer-other-window (help-buffer))
  (let ((mode-line
         ;; according to my setting, scratch buffer always exists
         (with-current-buffer "*scratch*"
           mode-line-format)))
    (with-current-buffer (help-buffer)
      (setq-local mode-line-format mode-line)))
  (posframe-control-hide (help-buffer)))

(defun bc-ide-lisp--help-show (&rest _)
  "Use `posframe' to display *Help* buffer."
  ;; step 1: hide help window
  (let ((help-window-p (get-buffer-window (help-buffer))))
    (when help-window-p
        (delete-window help-window-p)))
  ;; step 2: show posframe tips
  (posframe-control-show
   (help-buffer)
   :poshandler 'posframe-poshandler-point-bottom-left-corner
   :internal-border-width 3
   :internal-border-color "gray80"
   :left-fringe 4
   :right-fringe 4
   :width 60
   :min-width 60
   :height 15
   :control-keymap bc-ide-lisp-help-keymap
   :hide-fn (lambda () (interactive)
              (posframe-control-hide (help-buffer)))))

(advice-add 'describe-function :after #'bc-ide-lisp--help-show)
(advice-add 'describe-variable :after #'bc-ide-lisp--help-show)

(defun bc-ide-lisp-describe-function-or-variable (&optional symbol)
  "Describe the function or variable at point.  Place the result in a posframe.  If there is no function or variable at point, prompt user to select one."
  (interactive)
  (let ((symbol (or symbol (symbol-at-point))))
    (cond
     ((symbol-function symbol) (describe-function symbol))
     ((symbol-value symbol) (describe-variable symbol))
     ;; below means the symbol is neither a function nor a variable
     ;; prompt user for it
     (t (ivy-read
         "describe: "
         #'help--symbol-completion-table
         :action
         (lambda (sym)
           (bc-ide-lisp-describe-function-or-variable
            (intern sym))))))
    (bc-ide-lisp--help-show)))

(general-define-key
 :keymaps '(lisp-interaction-mode-map emacs-lisp-mode-map)
 :states '(normal visual motion)
 :prefix "SPC"
 "rh" 'bc-ide-lisp-describe-function-or-variable)

(defun bc-ide-lisp--set-tab-width ()
  (setq-local tab-width 2))

(add-hook 'emacs-lisp-mode-hook #'bc-ide-lisp--set-tab-width)
(add-hook 'lisp-interaction-mode-hook #'bc-ide-lisp--set-tab-width)

(provide 'bc-ide-lisp)
;;; bc-ide-lisp.el ends here
