;;; bc-vcs.el --- version control software settings: mainly magit

;;; Commentary:

;;; Code:

(use-package magit
  :defer t

  :config
  ;; functions

  (defun bc-vcs-visit-thing-at-point ()
    "Get file at point in magit buffers."
    (interactive)
    (cond ((magit-section-match [todos-item])
           ;; for `magit-todos' block. visit corresponding files
           (let ((file (magit-todos-item-filename
                        (oref (magit-current-section) :value))))
             (unless file
               (error "No file at point"))
             (other-window -1)
             (find-file (expand-file-name file (magit-toplevel)))))
          ((magit-section-match [file])
           ;; file, visit the corresponding files
           (let ((file (magit-file-at-point t)))
             (unless file
               (error "No file at point"))
             (other-window -1)
             (find-file file)))
          ((magit-section-match [commit])
           ;; commits: show the commit details
           (call-interactively #'magit-show-commit))
          ((magit-section-match [* error])
           (magit-process-buffer))
          ((and (magit-section-match '(issue pullreq))
                (featurep 'forge))
           ;; for `forge-issue' and `forge-pullreq' block, visit corresponding issue
           (call-interactively #'forge-visit-topic))
          ;; fallback - `magit-visit-thing'
          (t 'magit-visit-thing)))

  (dolist (mode '(magit-status-mode magit-diff-mode magit-log-mode))
    (evil-set-initial-state mode 'motion))
  (evil-set-initial-state 'git-commit-mode 'insert)

  :general
  (:keymaps '(magit-status-mode-map magit-diff-mode-map magit-log-mode-map)
   :states '(motion normal visual)
   "SPC" nil
   "C-e" 'magit-section-forward-sibling
   "C-y" 'magit-section-backward-sibling
   "zo" 'magit-section-show
   "zc" 'magit-section-hide
   "?" 'magit-dispatch
   "RET" 'bc-vcs-visit-thing-at-point)

  (:keymaps 'magit-status-mode-map
   :states '(motion normal)
   ;; TODO: use SPC prefix
   "`" 'magit-dispatch
   "d" 'magit-discard
   "E" 'magit-ediff
   "c" 'magit-commit
   "p" 'magit-push
   "f" 'magit-fetch
   "F" 'magit-pull
   "b" 'magit-branch)

  (:keymaps '(magit-status-mode-map magit-diff-mode-map magit-log-mode-map)
   :states '(motion normal)
   :prefix "SPC"
   "q" 'kill-buffer-and-window
   "r" (lambda () (interactive) (magit-refresh-buffer)))

  (:keymaps 'git-rebase-mode-map
   :states '(normal visual motion)
   "p" 'git-rebase-pick
   "r" 'git-rebase-reword
   "s" 'git-rebase-squash
   "d" 'git-rebase-kill-line
   "M-j" 'git-rebase-move-line-down
   "M-k" 'git-rebase-move-line-up))

(use-package magit-todos
  :after magit
  :init
  (setq magit-todos-insert-at 'bottom
        magit-todos-section-heading "Todo")
  :commands magit-todos-mode
  :general
  (:keymaps 'magit-todos-section-map
   "j" 'evil-next-visual-line
   "b" 'magit-branch))

(use-package forge
  :after magit
  :init
  (setq ghub-use-workaround-for-emacs-bug 'force)
  :general
  (:keymaps 'magit-status-mode-map
   "@" 'forge-dispatch)

  (:keymaps 'forge-topic-mode-map
   :states '(normal visual motion)
   "<return>" 'bc-vcs-visit-thing-at-point)

  (:keymaps 'forge-topic-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "re" 'magit-edit-thing
   "rr" 'forge-create-post))

(use-package ediff
  :ensure nil
  :hook
  (ediff-keymap-setup . bc-vcs-ediff-modify-keys)
  (ediff-prepare-buffer . bc-vcs-ediff--turn-off-hs)
  (ediff-prepare-buffer . org-show-all)
  (ediff-quit . bc-vcs-ediff--turn-on-hs)

  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (evil-set-initial-state 'ediff-mode 'motion)

  ;; ediff mode integration
  ;; see http://web.mit.edu/~yandros/elisp/hideshow.el
  (defun bc-vcs-ediff--turn-off-hs ()
    "Turn off `hs-minor-mode'."
    (hs-minor-mode -1))

  (defun bc-vcs-ediff--turn-on-hs ()
    "Turn on `hs-minor-mode', run after finishing ediffing."
    (hs-minor-mode 1))

  ;; these functions are taken from bc-vcs
  ;; https://github.com/emacs-evil/evil-collection/blob/master/evil-collection-ediff.el
  (defun bc-vcs-ediff-scroll-left (&optional arg)
    "Scroll left."
    (interactive "P")
    (let ((last-command-event ?>))
      (ediff-scroll-horizontally arg)))

  (defun bc-vcs-ediff-scroll-right (&optional arg)
    "Scroll right."
    (interactive "P")
    (let ((last-command-event ?<))
      (ediff-scroll-horizontally arg)))

  (defun bc-vcs-ediff-scroll-up (&optional arg)
    "Scroll up by half of a page."
    (interactive "P")
    (let ((last-command-event ?V))
      (ediff-scroll-vertically arg)))

  (defun bc-vcs-ediff-scroll-down (&optional arg)
    "Scroll down by half of a page."
    (interactive "P")
    (let ((last-command-event ?v))
      (ediff-scroll-vertically arg)))

  (defun bc-vcs-ediff-scroll-down-1 ()
    "Scroll down by a line."
    (interactive)
    (let ((last-command-event ?v))
      (ediff-scroll-vertically 1)))

  (defun bc-vcs-ediff-scroll-up-1 ()
    "Scroll down by a line."
    (interactive)
    (let ((last-command-event ?V))
      (ediff-scroll-vertically 1)))

  (defun bc-vcs-ediff-first-difference ()
    "Jump to first difference."
    (interactive)
    (ediff-jump-to-difference 1))

  (defun bc-vcs-ediff-last-difference ()
    "Jump to last difference."
    (interactive)
    (ediff-jump-to-difference ediff-number-of-differences))

  (defun bc-vcs-ediff-modify-keys ()
    "Due to the wired way `ediff-mode' sets up its keymap, need to wrap this in a function and run it in `ediff-keymap-setup-hook'."
    (general-define-key
     :keymaps 'ediff-mode-map
     :states 'motion
     "SPC" nil
     "?" 'ediff-toggle-help
     "n" 'ediff-next-difference
     "N" 'ediff-previous-difference
     "G" 'ediff-jump-to-difference
     "gg" 'bc-vcs-ediff-first-difference

     "j" 'bc-vcs-ediff-scroll-down-1
     "k" 'bc-vcs-ediff-scroll-up-1
     "h" 'bc-vcs-ediff-scroll-left
     "l" 'bc-vcs-ediff-scroll-right
     "C-d" 'bc-vcs-ediff-scroll-down
     "C-u" 'bc-vcs-ediff-scroll-up

     "a" 'ediff-copy-A-to-B
     "b" 'ediff-copy-B-to-A

     "C-e" 'ediff-next-difference
     "C-y" 'ediff-previous-difference

     "q" 'ediff-quit)

    ;; fool-proving
    (general-define-key
     :keymaps 'ediff-mode-map
     :states 'motion
     :prefix "SPC"
     "q" 'ediff-quit)

    ;; if it is a three-window job
    (unless ediff-3way-comparison-job
      (general-define-key
       :keymaps 'ediff-mode-map
       :states 'motion
       "a" 'ediff-copy-A-to-C
       "b" 'ediff-copy-B-to-C
       "+" 'ediff-combine-diffs))))

(provide 'bc-vcs)
;;; bc-vcs.el ends here
