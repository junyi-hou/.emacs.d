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
  :init
  (evil-mode 1)
  (setq evil-normal-state-modes (append evil-emacs-state-modes
                                        evil-normal-state-modes))

  :general
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
  "ee" 'execute-extended-command
  "el" 'eval-last-sexp
  "ei" 'ivy-resume
            
  ;; helps
  "hf" 'describe-function
  "hw" 'where-is
  "hk" 'describe-key
  "hv" 'describe-variable
  "hm" 'describe-mode
  "hh" 'help-for-help

  ;; basic function
  "w" 'evil-write
  "k" 'delete-window
  "q" 'kill-current-buffer
  "b" 'balance-windows

  ;; buffer related
  "n" 'bc-evil-next-user-buffer
  "N" 'bc-evil-previous-user-buffer

  ;; split
  "\\"  (lambda () (interactive) (evil-window-vsplit) (evil-window-right 1))
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

   "C-u" 'evil-scroll-up
   "C-d" 'evil-scroll-down

   "M-l" 'right-char
   "M-h" 'left-char
   "M-<DEL>" (lambda () (interactive)
               (backward-delete-char-untabify 1)
               (delete-char 1))

   "C-e" (lambda () (interactive) (evil-scroll-line-down 5))
   "C-y" (lambda () (interactive) (evil-scroll-line-up 5))

   "M-`" nil)

  (:keymaps 'visual
   "*" 'bc-evil-search-visually-forward
   "#" 'bc-evil-search-visually-backward)

  (:keymaps '(normal motion)
   "<tab>" 'evil-jump-item)

  (:keymaps 'visual
   "<tab>" 'bc-evil-visual-tab)

  (:keymaps 'insert
   "<tab>" 'bc-company-unified-tab)

  (:keymaps '(help-mode-map message-mode-map)
   :states 'motion
   :prefix "SPC"
   "q" 'delete-window))


(electric-pair-mode 1)
(eval-after-load 'electric-pair
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\{ . ?\})
                              (?\` . ?\')
                              (?\$ . ?\$)
                              (?\< . ?\>))))


;; functions:

(defun bc-evil-visual-tab ()
  "Tab binding in visual mode."
  (interactive)
  (when (evil-visual-state-p)
    (if (eq evil-visual-selection 'line)
        (indent-region (region-beginning) (region-end))
      (er/expand-region 1))))

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
  (evil-next-visual-line 3))

(evil-define-motion bc-evil-previous-three-lines ()
  (interactive)
  (evil-previous-visual-line 3))

(defun bc-evil-better-newline (&optional arg interactive)
  "When calling `newline', check whether current line is a comment line (i.e., start with 0 or more spaces followed by `comment-start-skip')  If so, automatically indent and insert `comment-start-skip' after calling `newline'.  Otherwise call `newline' as default.

Optional arguments ARG and INTERACTIVE are included to satisfied `newline'."
  (let ((output-str ""))
    (save-excursion
      (line-move -1)
      (let* ((test-str (thing-at-point 'line t)))
        (setq output-str (if (string-match
                              (concat "\\(^[\t ]*\\)\\("
                                      comment-start-skip "\\)")
                              test-str)
                             (match-string 2 test-str)
                           ""))))
    (insert output-str)))

(advice-add 'newline :after #'bc-evil-better-newline)

(defun bc-evil--search-visually-selected-text (forward)
  "Search visually selected test.  If FORWARD is t, search forward, otherwise search backward."
  (let* ((beg (region-beginning))
         (end (region-end))
         (search-str (buffer-substring-no-properties beg end))
         (search-str-length (length search-str)))
    (evil-exit-visual-state)
    ;; if search backwards, need to move point to region beginning
    ;; likewise, if search forward, move point to region end
    (if forward
        (goto-char end)
      (goto-char beg))
    (evil-search search-str forward)
    (unless (equal (car-safe evil-ex-search-history) search-str)
      (push search-str evil-ex-search-history))
    (evil-push-search-history search-str forward)
    (evil-visual-state)
    (evil-forward-char search-str-length)))

(evil-define-motion bc-evil-search-visually-forward ()
  "Search forward for the visual selected text."
  :jump t
  :repeat nil
  (interactive)
  (bc-evil--search-visually-selected-text t))

(evil-define-motion bc-evil-search-visually-backward ()
  "Search backward for the visual selected text."
  :jump t
  :repeat nil
  (interactive)
  (bc-evil--search-visually-selected-text nil))


(provide 'bc-evil)
;;; bc-evil.el ends here
