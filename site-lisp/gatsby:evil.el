;;; gatsby:evil.el --- evil mode related settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby:core)

;; replace undo-tree with undo-fu
(use-package undo-fu
  :after evil
  :init
  (setq undo-limit 400000
        undo-strong-limit 3000000
        undo-outer-limit 3000000)

  (global-set-key [remap undo] #'undo-fu-only-undo)
  (global-set-key [remap redo] #'undo-fu-only-redo)

  (with-eval-after-load 'undo-tree
    (global-set-key [remap undo-tree-undo] #'undo-fu-only-undo)
    (global-set-key [remap undo-tree-redo] #'undo-fu-only-redo)
    (global-undo-tree-mode -1))

  (with-eval-after-load 'aggressive-indent
    (setq aggressive-indent-protected-commands
          (add-to-list 'aggressive-indent-protected-commands 'undo-fu-only-undo)))
  )

(use-package undo-fu-session
  :straight (undo-fu-session :repo "ideasman42/emacs-undo-fu-session" :host gitlab)
  :after undo-fu
  :init
  (setq undo-fu-session-directory (concat no-littering-var-directory "undo-fu-session/")
        undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (global-undo-fu-session-mode 1))

(use-package evil
  ;;  welcome to the dark side
  :init
  (evil-mode)

  ;; set normal state as default start state
  (setq evil-search-module 'evil-search
        evil-normal-state-modes (append evil-emacs-state-modes
                                        evil-normal-state-modes))

  (evil-define-motion gatsby:evil-next-three-lines ()
    (interactive)
    (evil-next-visual-line 3))

  (evil-define-motion gatsby:evil-previous-three-lines ()
    (interactive)
    (evil-previous-visual-line 3))

  (defun gatsby:evil--search-visually-selected-text (forward)
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

  (evil-define-motion gatsby:evil-search-visually-forward ()
    "Search forward for the visual selected text."
    :jump t
    :repeat nil
    (interactive)
    (gatsby:evil--search-visually-selected-text t))

  (evil-define-motion gatsby:evil-search-visually-backward ()
    "Search backward for the visual selected text."
    :jump t
    :repeat nil
    (interactive)
    (gatsby:evil--search-visually-selected-text nil))

  (defun gatsby:evil-visual-tab ()
    "Indent region if in visual-line-mode, otherwise select contains inside a pair of tags via `evil-jump-item'"
    (interactive)
    (if (eq evil-visual-selection 'line)
        (indent-region (region-beginning) (region-end))
      (evil-jump-item)))

  ;; borrow from http://steve.yegge.googlepages.com/my-dot-emacs-file
  (defun gatsby:evil-rename-file-and-buffer (new-name)
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

  (defun gatsby:evil-move-buffer-file (new-location)
    "Move `buffer-file-name' to NEW-LOCATION."
    (interactive
     (list
      (completing-read "Move to: "
                       #'read-file-name-internal
                       (lambda (x)
                         (string= x (file-name-as-directory x))))))
    (let* ((file-name (buffer-file-name))
           (cmd (concat "mv " file-name " " new-location (file-name-nondirectory file-name))))
      (unless file-name
        (user-error "Current buffer is not associated with any file"))
      (start-process-shell-command
       "move" nil cmd)))

  (defun gatsby:evil--center-cursor-line (&rest _)
    "Thin wrapper around `evil-scroll-line-to-center' for advice purpose."
    (recenter nil))

  (advice-add #'evil-search-next :after #'gatsby:evil--center-cursor-line)
  (advice-add #'evil-search-previous :after #'gatsby:evil--center-cursor-line)
  (advice-add #'gatsby:evil-search-visually-forward :after #'gatsby:evil--center-cursor-line)
  (advice-add #'gatsby:evil-search-visually-backward :after #'gatsby:evil--center-cursor-line)

  (defun gatsby:evil--move-up-after-jump (&rest _)
    "Move up 1 line after `evil-jump-item' so the last line is not covered by eldoc-box."
    (when (and (not (= (window-end) (point-max)))
               (<= (- (line-number-at-pos (window-end)) (line-number-at-pos)) 2))
      (evil-scroll-line-down 2)))

  (advice-add #'evil-jump-item :after #'gatsby:evil--move-up-after-jump)

  (defun gatsby:evil--recenter-after-goto-point-max (count)
    "Thin wrapper around `evil-scroll-line-to-center' so center the end-of-buffer after a G motion."
    (unless count
      (recenter nil)))

  (advice-add #'evil-goto-line :after #'gatsby:evil--recenter-after-goto-point-max)

  (defun gatsby:evil-normal-state-if-not-motion ()
    "Switch to evil normal state if the current state is not motion state."
    (interactive)
    (unless (evil-motion-state-p)
      (evil-normal-state)))

  ;; bind esc to normal state in all cases
  (global-set-key (kbd "<escape>") 'gatsby:evil-normal-state-if-not-motion)

  (defun gatsby:evil-better-newline (newline-fun &rest args)
    "When calling `newline', check whether current line is a comment line (i.e., start with 0 or more spaces followed by `comment-start-skip')  If so, automatically indent and insert `comment-start-skip' after calling `newline' for the first call.  Delete the auto-inserted comment for the second call.  Otherwise call `newline' as default."
    (let* (;; line - the current line as string
           (line (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))
           ;; only-comment - t if the current line starts with comment
           (only-comment (and comment-start-skip
                              (string-match (concat "\\(^[\t ]*\\)\\(" comment-start-skip "\\)") line)))
           ;; newline-string - string insert into newline
           (newline-string (if only-comment
                               (match-string 2 line)
                             "")))
      (if (and only-comment
               (eq last-command 'newline))
          (progn
            (delete-region (line-beginning-position) (point))
            (insert (match-string 1 line)))
        (apply newline-fun args)
        (insert newline-string))))

  (advice-add 'newline :around #'gatsby:evil-better-newline)

  :general
  (:keymaps '(motion normal visual)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line

   "H" 'evil-first-non-blank-of-visual-line
   "J" 'gatsby:evil-next-three-lines
   "K" 'gatsby:evil-previous-three-lines
   "L" 'evil-end-of-visual-line

   "SPC" nil
   "S-SPC" nil)

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

   "C-e" (lambda () (interactive) (evil-scroll-line-down 5))
   "C-y" (lambda () (interactive) (evil-scroll-line-up 5)))

  ;; leader key
  (:keymaps '(motion normal visual emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"

   ;; execute
   "ee" 'execute-extended-command
   "el" 'eval-last-sexp
   "eL" (lambda () (interactive)
          (eval-buffer)
          (message "buffer %s evaluated!" (buffer-name)))

   ;; basic function
   "w" 'evil-write
   "k" 'delete-window
   "K" 'delete-frame
   "q" 'kill-current-buffer
   "b" 'balance-windows

   ;; split
   "\\"  (lambda () (interactive) (evil-window-vsplit) (evil-window-right 1))
   "-"   (lambda () (interactive) (evil-window-split) (evil-window-down 1)))

  (:keymaps 'visual
   "*" 'gatsby:evil-search-visually-forward
   "#" 'gatsby:evil-search-visually-backward
   "<tab>" 'gatsby:evil-visual-tab)

  (:keymaps '(normal motion)
   "<tab>" 'evil-jump-item)

  (:keymaps 'visual
   :prefix "SPC"
   "a" 'align-regexp)

  (:keymaps 'message-mode-map
   :states 'motion
   :prefix "SPC"
   "q" 'delete-window)

  (:keymaps 'help-mode-map
   :states 'motion
   :prefix "SPC"
   "q" 'delete-window
   "M-j" 'help-go-forward
   "M-k" 'help-go-back))

(use-package evil-surround
  ;; vim-surround ported to emacs
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-indent-textobject :after evil)

(use-package evil-nerd-commenter
  :after evil
  :commands evilnc-comment-or-uncomment-lines
  :general
  (:keymaps '(motion normal visual emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "t" 'evilnc-comment-or-uncomment-lines))

(use-package expand-region
  :after evil
  :general
  (:keymaps 'visual
   "v" 'er/expand-region
   "V" 'er/contract-region))

(use-package elec-pair
  :config
  (setq electric-pair-open-newline-between-pairs t
        electric-pair-delete-adjacent-pairs t)
  :hook
  (prog-mode . electric-pair-mode))

(provide 'gatsby:evil)
;;; gatsby:evil.el ends here
