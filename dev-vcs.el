;;; dev-vcs.el -- version control software settings: mainly magit

;;; Commentary:

;;; Code:

(use-package magit
  :config
  (dolist (mode '(magit-status-mode magit-diff-mode magit-log-mode))
    (evil-set-initial-state mode 'motion))
  (evil-set-initial-state 'git-commit-mode 'insert)
  (setq magit-log-auto-more t  ; auto load logs if hit bottom
        )
  :general
  (:keymaps '(magit-status-mode-map magit-diff-mode-map magit-log-mode-map)
   :states '(motion normal)
   "SPC" nil
   "J" 'magit-section-forward-sibling
   "K" 'magit-section-backward-sibling
   "<tab>" 'magit-section-toggle
   "TAB" 'magit-section-toggle
   "?" 'magit-dispatch)

  (:keymaps 'magit-log-mode-map
   :states '(normal motion)
   "d" (lambda () (interactive)
           (magit-diff-show-or-scroll-down)
           (other-window 1)))

  (:keymaps '(magit-status-mode-map magit-diff-mode-map magit-log-mode-map)
   :states '(motion normal)
   :prefix "SPC"
   "q" 'kill-buffer-and-window
   "r" (lambda () (interactive) (magit-refresh-buffer))))


(defun dev-vcs-commit-current-file ()
  "Open a `git-commit-mode' window, prompt for commit message and commit the current file.  If there are other staged but uncommitted files, error out."
  (interactive)
  (when (-remove (lambda (x)
                   (equal x (file-name-nondirectory (buffer-file-name))))
                 (magit-staged-files))
    (error "There are other files in the staging area, abort action"))
  (magit-stage-file (buffer-file-name))
  (magit-commit-create))


;; settings

(provide 'dev-vcs)
;;; dev-vcs.el ends here
