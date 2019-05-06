;;; bc-evil.el -- evil mode related settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; load pkgs

(require 'bc-pkgmgmt)
(require 'bc-core)

(use-package general)

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-indent-textobject :after evil)

(use-package evil-nerd-commenter
  :after evil
  :commands evilnc-comment-or-uncomment-lines)

(use-package expand-region
  :after evil
  :commands (er/expand-region er/contract-region)
  :general
  (:keymaps 'visual
   "v" 'er/expand-region
   "V" 'er/contract-region))

(use-package evil
  :config
  (setq evil-normal-state-modes (append evil-emacs-state-modes
                                        evil-normal-state-modes)
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t)
  (evil-mode 1)
  :general
  ;; leader: SPC
  (:keymaps '(motion normal visual)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line

   "H" 'evil-first-non-blank-of-visual-line
   "J" 'bc-evil-next-three-lines
   "K" 'bc-evil-previous-three-lines
   "L" 'evil-end-of-visual-line
   "" nil
   "SPC" nil)

  (:keymaps '(motion normal visual)
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
  "t" 'evilnc-comment-or-uncomment-lines)

  ;; combination key that should be active in all states
  (:keymaps '(motion normal visual emacs insert)
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right

   "M-l" 'right-char
   "M-h" 'left-char
   "M-<DEL>" (lambda () (interactive)
               (backward-delete-char-untabify 1)
               (delete-char 1))

   "TAB" 'bc-evil-smart-tab
   "<tab>" 'bc-evil-smart-tab

   "C-e" (lambda () (interactive) (evil-scroll-line-down 5))
   "C-y" (lambda () (interactive) (evil-scroll-line-up 5)))

  (:keymaps '(help-mode-map message-mode-map)
   :states 'motion
   :prefix "SPC"
   "q" 'delete-window))

(electric-pair-mode 1)
(setq electric-pair-pairs '((?\" . ?\")
                            (?\{ . ?\})
                            (?\` . ?\')
                            (?\$ . ?\$)
                            (?\< . ?\>)))


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
  "Smart tab key.

In normal mode, call `evil-jump-item';
In visual-line mode, call `indent-region';
In visual mode, call `er/expand-region';
In insert more, first try `company-manual-begin'.  If there is no snippet available at point, indent the current line by `tab-width' length."
  (interactive)
  (cond ((and (evil-visual-state-p) (eq evil-visual-selection 'line))
         (indent-region (region-beginning) (region-end)))
        ((evil-visual-state-p) (er/expand-region 1))
        ((evil-insert-state-p) (progn
                                 (company-manual-begin)
                                 (unless company-candidates
                                   (if (= 0 (current-column))
                                       (dotimes (n tab-width)
                                         (insert " "))
                                     (save-excursion
                                       (beginning-of-line)
                                       (dotimes (n tab-width)
                                         (insert " ")))))))
        (t (evil-jump-item))))


(provide 'bc-evil)
;;; bc-evil.el ends here
