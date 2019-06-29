;;; bc-vcs.el --- version control software settings: mainly magit

;;; Commentary:

;;; Code:

(require 'dash)

(use-package magit
  :init

  ;; functions
  
  (defun bc-vcs-commit-current-file ()
    "Open a `git-commit-mode' window, prompt for commit message and commit the current file.  If there are other staged but uncommitted files, error out."
    (interactive)
    (when (-remove (lambda (x)
                     (equal x (file-name-nondirectory (buffer-file-name))))
                   (magit-staged-files))
      (error "There are other files in the staging area, aborting"))
    (magit-stage-file (buffer-file-name))
    (magit-commit-create))

  (defun bc-vcs-visit-file-at-point (&optional file)
    "Show FILE or file at point in magit buffer using `magit-file-at-point' function in the previous window."
    (interactive)
    (let ((file (or file (expand-file-name (magit-file-at-point)))))
      (unless file
        (error "No file at point"))
      (other-window -1)
      (find-file file)))
  :config
  (dolist (mode '(magit-status-mode magit-diff-mode magit-log-mode))
    (evil-set-initial-state mode 'motion))
  (evil-set-initial-state 'git-commit-mode 'insert)
  (setq magit-log-auto-more t)

  :general
  (:keymaps '(magit-status-mode-map magit-diff-mode-map magit-log-mode-map)
   :states '(motion normal)
   "SPC" nil
   "C-d" 'magit-section-forward-sibling
   "C-u" 'magit-section-backward-sibling
   "zo" 'magit-section-show
   "zc" 'magit-section-hide
   "?" 'magit-dispatch
   "RET" 'bc-vcs-visit-file-at-point)

  (:keymaps 'magit-status-mode-map
   :states '(motion normal)
   "d" 'magit-discard
   "s" 'magit-show-commit
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


;; (use-package git-timemachine
;;   :config
;;   (add-hook 'git-timemachine-mode-hook #'evil-motion-state)
;;   (evil-make-overriding-map git-timemachine-mode-map 'motion)
;;   (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)
;;   :general
;;   (:keymaps 'git-timemachine-mode-map
;;    :state 'motion
;;    "M-k" 'git-timemachine-show-previous-revision
;;    "M-j" 'git-timemachine-show-next-revision
;;    "<up>" 'git-timemachine-show-previous-revision
;;    "<down>" 'git-timemachine-show-next-revision
;;    "q" 'git-timemachine-quit
;;    "c" 'git-timemachine-show-commit
;;    "b" 'git-timemachine-blame
;;    "t" 'git-timemachine-show-revision-fuzzy
;;    "g" 'git-timemachine-show-revision
;;    "y" 'git-timemachine-kill-abbreviated-revision
;;    "Y" 'git-timemachine-kill-revision)

;;   (:keymaps 'normal
;;    :prefix "SPC"
;;    "gh" 'git-timemachine))


(provide 'bc-vcs)
;;; bc-vcs.el ends here
