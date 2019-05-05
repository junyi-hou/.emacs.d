;;; bc-evil.el -- evil mode related settings -*- lexical-binding: t; -*-

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

(require 'bc-pkgmgmt)
(require 'bc-core)

;; functions

(defun bc-evil--is-user-buffer ()
  "Determine whether the current buffer is a user-buffer by looking at the first char.  Return t if current buffer is not a dired tree or is a user-buffer (include *scratch* buffer)."
  (let ((name (buffer-name)))
    (cond ((string-equal "*scratch*" name) t)
          ((string-equal "*" (substring name 0 1)) nil)
          ((string-equal major-mode "dired-mode") nil)
          ((string-equal "magit" (substring name 0 5)) nil)
          (t t))))

(defun bc-evil-next-user-buffer ()
  "Jump to the next user buffer."
  (interactive)
  (next-buffer)
  (if (not (bc-evil--is-user-buffer))
      (bc-evil-next-user-buffer)))

(defun bc-evil-previous-user-buffer ()
  "Jump to the previous user buffer."
  (interactive)
  (previous-buffer)
  (if (not (bc-evil--is-user-buffer))
      (bc-evil-previous-user-buffer)))

;; borrow from http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun bc-evil-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(evil-define-motion bc-evil-next-three-lines ()
  (interactive)
  (evil-next-visual-line 3)
  )

(evil-define-motion bc-evil-previous-three-lines ()
  (interactive)
  (evil-previous-visual-line 3)
  )

(defun bc-evil-smart-tab ()
  "Assign tab key to:
    `indent-region' if in visual line mode;
    `er/expand-region' if in visual mode;
    insert `tab-width' number of spaces in front of the current line if in insert mode;
    `evil-jump-item' otherwise \(i.e., in the normal state\)"
  (interactive)
  (cond ((and (evil-visual-state-p) (eq evil-visual-selection 'line))
         (indent-region (region-beginning) (region-end)))
        ((evil-visual-state-p) (er/expand-region 1))
        ((evil-insert-state-p) (if (= 0 (current-column))
                                   (dotimes (n tab-width)
                                     (insert " "))
                                 (save-excursion
                                   (beginning-of-line)
                                   (dotimes (n tab-width)
                                     (insert " ")))))
        (t (evil-jump-item))))

(defun bc-evil-insert-pair (pair-begin)
  "Insert a pair defined by PAIR-BEGIN.  Pairs are stored in `bc-default-pairs'.  One can overwrite them in different major modes."
  (interactive)
  (let* ((pair-end (gethash pair-begin bc-default-pairs)))
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
 "q"  'kill-current-buffer
 "b" 'balance-windows

 ;; buffer related
 "n"  'bc-evil-next-user-buffer
 "N"  'bc-evil-previous-user-buffer

 ;; split
 "\\"   (lambda () (interactive) (evil-window-vsplit) (evil-window-right 1))
 "-"   (lambda () (interactive) (evil-window-split) (evil-window-down 1))

 ;; open stuffs
 "of" 'find-file
 "ob" 'ivy-switch-buffer
 "oo" 'projectile-find-file
 "os" 'bc-eshell-open-here
 "op" 'projectile-switch-project

 ;; git-related
 "gg" 'magit-status
 "gc" 'bc-vcs-commit-current-file
 "gd" 'magit-diff-buffer-file
 "gl" 'magit-log-buffer-file

 ;; search and replace

 ;; change files
 "mr" 'bc-evil-rename-file-and-buffer

 ;; other uses
 "t" 'evilnc-comment-or-uncomment-lines
 "f" 'evil-toggle-fold)

;; movement keys
(general-define-key
 :keymaps '(motion normal visual)

 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line

 "H" 'evil-first-non-blank-of-visual-line
 "J" 'bc-evil-next-three-lines
 "K" 'bc-evil-previous-three-lines
 "L" 'evil-end-of-visual-line)


;; combination key that should be active in all states
(general-define-key
 :keymaps '(motion normal visual emacs insert)
 "C-h" 'evil-window-left
 "C-j" 'evil-window-down
 "C-k" 'evil-window-up
 "C-l" 'evil-window-right

 "M-(" (lambda () (interactive) (bc-evil-insert-pair "("))
 "M-{" (lambda () (interactive) (bc-evil-insert-pair "\{"))
 "M-[" (lambda () (interactive) (bc-evil-insert-pair "["))
 "M-'" (lambda () (interactive) (bc-evil-insert-pair "\'"))
 "M-\"" (lambda () (interactive) (bc-evil-insert-pair "\""))
 "M-`" (lambda () (interactive) (bc-evil-insert-pair "\`"))
 "M-$" (lambda () (interactive) (bc-evil-insert-pair "$"))
 "M-l" 'right-char
 "M-h" 'left-char
 "M-<DEL>" (lambda () (interactive)
             (backward-delete-char-untabify 1)
             (delete-char 1))

 "TAB" 'bc-evil-smart-tab
 "<tab>" 'bc-evil-smart-tab

 "C-e" (lambda () (interactive) (evil-scroll-line-down 5))
 "C-y" (lambda () (interactive) (evil-scroll-line-up 5)))

;; expand-region
(general-define-key
 :keymaps 'visual
 "v" 'er/expand-region
 "V" 'er/contract-region)

;; help mode
(general-define-key
 :keymaps 'help-mode-map
 :states 'motion
 :prefix "SPC"
 "q" 'kill-buffer-and-window)


(provide 'bc-evil)
;;; bc-evil.el ends here
