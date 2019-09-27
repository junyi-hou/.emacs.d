;;; bc-ide-lisp.el --- setup ide features (auto-completion, code-jump, linter) for lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package posframe-control
  :quelpa
  (posframe-control
   :repo "junyi-hou/posframe-control"
   :fetcher github))

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
    (define-key map (kbd "RET") #'bc-ide-lisp-goto-def)
    map)
  "Keymap for controlling help posframes.")

(defun bc-ide-lisp-goto-def ()
  "Goto definition found by the help buffer."
  (interactive)
  (with-current-buffer (help-buffer)
    (button-activate (button-at (next-char-property-change 1))))
  (posframe-control-hide (help-buffer)))

(defun bc-ide-lisp--posframe-frontend (&rest _)
  "Use `posframe' to show *Help* buffer."
  ;; step 1: hide help window
  (delete-window (get-buffer-window (help-buffer)))
  ;; step 2: show posframe tips
  (posframe-control-show
   (help-buffer)
   :poshandler 'posframe-poshandler-point-bottom-left-corner
   :internal-border-width 3
   :internal-border-color "gray80"
   :left-fringe 1
   :right-fringe 1
   :width 60
   :min-width 60
   :height 15
   :control-keymap bc-ide-lisp-help-keymap
   :hide-fn (lambda () (interactive)
              (posframe-control-hide (help-buffer)))))

(advice-add 'describe-function :after #'bc-ide-lisp--posframe-frontend)
(advice-add 'describe-variable :after #'bc-ide-lisp--posframe-frontend)

(defun bc-ide-lisp--set-tab-width ()
  (setq-local tab-width 2))

(add-hook 'emacs-lisp-mode-hook #'bc-ide-lisp--set-tab-width)
(add-hook 'lisp-interaction-mode-hook #'bc-ide-lisp--set-tab-width)

(provide 'bc-ide-lisp)
;;; bc-ide-lisp.el ends here
