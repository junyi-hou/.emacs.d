;;; bc-vcs.el --- version control software settings: mainly magit

;;; Commentary:

;;; Code:

(use-package magit
  :defer t

  :config

  ;; functions
  
  (defun bc-vcs-get-file-at-point (&optional file)
    "Get file at point in magit buffers."
    (cond
     ((magit-section-match [* todos])
        (let* ((todo-item (oref (magit-current-section) value))
               (file (or file (magit-todos-item-filename todo-item))))
          (unless file
            (error "No file at point"))
          (expand-file-name file (magit-toplevel))))
     (t (let ((file (or file (magit-file-at-point t))))
          (unless file
            (error "No file at point"))
          file))))

  (dolist (mode '(magit-status-mode magit-diff-mode magit-log-mode))
    (evil-set-initial-state mode 'motion))
  (evil-set-initial-state 'git-commit-mode 'insert)

  (setq magit-log-auto-more t)

  :general
  (:keymaps '(magit-status-mode-map magit-diff-mode-map magit-log-mode-map)
   :states '(motion normal)
   "SPC" nil
   ;; "C-d" 'magit-section-forward-sibling
   ;; "C-u" 'magit-section-backward-sibling
   "zo" 'magit-section-show
   "zc" 'magit-section-hide
   "?" 'magit-dispatch
   "RET" (lambda () (interactive)
           (let ((file (bc-vcs-get-file-at-point)))
             (when file
               (other-window -1)
               (find-file file)))))

  (:keymaps 'magit-status-mode-map
   :states '(motion normal)
   "d" 'magit-discard
   "s" 'magit-show-commit
   "V" 'magit-revert
   "c" 'magit-commit
   "p" 'magit-push
   "f" 'magit-fetch
   "F" 'magit-pull
   "b" 'magit-branch)

  (:keymaps 'magit-log-mode-map
   :states '(normal motion)
   "s" (lambda () (interactive)
           (magit-diff-show-or-scroll-down)
           (other-window 1)))

  (:keymaps '(magit-status-mode-map magit-diff-mode-map magit-log-mode-map)
   :states '(motion normal)
   :prefix "SPC"
   "q" 'kill-buffer-and-window
   "r" (lambda () (interactive) (magit-refresh-buffer))))

(use-package magit-todos
  :after magit
  :init
  (setq magit-todos-insert-at 'top)
  :commands magit-todos-mode
  :general
  (:keymaps 'magit-todos-section-map
            "j" 'evil-next-visual-line))

(provide 'bc-vcs)
;;; bc-vcs.el ends here
