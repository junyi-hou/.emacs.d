;;; dev-evil.el -- evil mode related settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; load pkgs

(use-package general
  :commands general-define-key)

(use-package evil
  :config
  (setq evil-normal-state-modes (append evil-emacs-state-modes evil-normal-state-modes))
  :init
  (setq evil-want-C-u-scroll t
        evil-want-C-d-scroll t)
  (evil-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-indent-textobject :after evil)

(use-package evil-nerd-commenter
  :after evil
  :commands evilnc-comment-or-uncomment-lines)

(use-package expand-region
  :commands (er/expand-region er/contract-region))

;; functions

(defun dev-evil--is-user-buffer ()
  "Determine whether the current buffer is a user-buffer by looking at the first char.  Return t if current buffer is not a dired tree or is a user-buffer (include *scratch* buffer)."
  (let ((name (buffer-name)))
    (cond ((string-equal "*scratch*" name) t)
          ((string-equal "*" (substring name 0 1)) nil)
          ((string-equal major-mode "dired-mode") nil)
          (t t))))

(defun dev-evil-next-user-buffer ()
  "Jump to the next user buffer."
  (interactive)
  (next-buffer)
  (if (not (dev-evil--is-user-buffer))
      (dev-evil-next-user-buffer)
    ))

(defun dev-evil-previous-user-buffer ()
  "Jump to the previous user buffer."
  (interactive)
  (previous-buffer)
  (if (not (dev-evil--is-user-buffer))
      (dev-evil-previous-user-buffer)
    ))

(evil-define-motion dev-evil-next-three-lines ()
  (interactive)
  (evil-next-visual-line 3)
  )

(evil-define-motion dev-evil-previous-three-lines ()
  (interactive)
  (evil-previous-visual-line 3)
  )

(defun dev-evil-smart-tab ()
  "Assign tab key to:
`indent-region` if in visual line mode;
`evil-jump-items` if in visual or normal mode."
  (interactive)
  (if (and (evil-visual-state-p) (eq evil-visual-selection 'line))
      (indent-region (region-beginning) (region-end))
    (evil-jump-item)))

(defun dev-evil-insert-pair (pair-begin)
  "Insert a pair defined by PAIR-BEGIN.  Pairs are stored in `dev-default-pairs'.  One can overwrite them in different major modes."
  (interactive)
  (let* ((pair-end (gethash pair-begin dev-default-pairs)))
    (when pair-end
      (insert (concat pair-begin pair-end))
      (left-char 1))))

;; settings

;; leader: SPC
(general-define-key
 :keymaps '(motion normal visual)
 "" nil
 "SPC" nil)

;; leader-relate keymaps
(general-define-key
 :keymaps '(motion normal visual)
 :prefix "SPC"
 
 ;; execute
 "ee"  'execute-extended-command
 "eE"  'evil-ex
 "el"  'eval-last-sexp
 
 ;; helps
 "hf" 'describe-function
 "hw" 'where-is
 "hk" 'describe-key
 "hv" 'describe-variable
 "hm" 'describe-mode
 "hh" 'help-for-help

 ;; basic function
 "w"  'evil-write
 "k"  'delete-window
 "q"  (lambda () (interactive) (kill-buffer (current-buffer)))
 "b" 'balance-windows

 ;; buffer related
 "n"  'dev-evil-next-user-buffer
 "N"  'dev-evil-previous-user-buffer

 ;; split
 "\\"  (lambda () (interactive) (evil-window-vsplit) (evil-window-right 1))
 "|"   (lambda () (interactive) (evil-window-vsplit) (evil-window-right 1))
 "-"   (lambda () (interactive) (evil-window-split) (evil-window-down 1))
 "_"   (lambda () (interactive) (evil-window-split) (evil-window-down 1))

 ;; open stuffs
 "of" 'find-file
 "ob" 'ivy-switch-buffer
 "oo" 'projectile-find-file
 "os" 'dev-eshell-open-here
 "op" 'projectile-switch-project

 ;; git-related
 "gg" 'magit-status
 "gc" 'dev-vcs-commit-current-file
 "gd" 'magit-diff-buffer-file
 "gl" 'magit-log-buffer-file

 ;; search and replace

 ;; other uses
 "t" 'evilnc-comment-or-uncomment-lines
 "f" 'evil-toggle-fold)

;; movement keys
(general-define-key
 :keymaps '(motion normal visual)

 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line

 "H" 'evil-first-non-blank-of-visual-line
 "J" 'dev-evil-next-three-lines
 "K" 'dev-evil-previous-three-lines
 "L" 'evil-end-of-visual-line

 "TAB" 'dev-evil-smart-tab
 "<tab>" 'dev-evil-smart-tab)

;; combination key that should be active in all states
(general-define-key
 :keymaps '(motion normal visual emacs insert)
 "C-h" 'evil-window-left
 "C-j" 'evil-window-down
 "C-k" 'evil-window-up
 "C-l" 'evil-window-right

 "M-(" (lambda () (interactive) (dev-evil-insert-pair "("))
 "M-{" (lambda () (interactive) (dev-evil-insert-pair "\{"))
 "M-[" (lambda () (interactive) (dev-evil-insert-pair "["))
 "M-'" (lambda () (interactive) (dev-evil-insert-pair "\'"))
 "M-\"" (lambda () (interactive) (dev-evil-insert-pair "\""))
 "M-`" (lambda () (interactive) (dev-evil-insert-pair "\`"))

 "C-e" (lambda () (interactive) (evil-scroll-line-down 5))
 "C-y" (lambda () (interactive) (evil-scroll-line-up 5)))

;; expand-region
(general-define-key
 :keymaps 'visual
 "v" 'er/expand-region
 "V" 'er/contract-region
 )

;; help mode
(general-define-key
 :keymaps 'help-mode-map
 :states 'motion
 :prefix "SPC"
 "q" 'kill-buffer-and-window)


(provide 'dev-evil)
;;; dev-evil.el ends here

; LocalWords:  SPC
