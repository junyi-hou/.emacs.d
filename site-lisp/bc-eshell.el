;;; bc-eshell.el --- my eshell setting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eshell
  :init
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-buffer-maximum-lines 12000
        eshell-error-if-no-glob t
        eshell-glob-case-insensitive t
        eshell-hist-ignoredups t
        eshell-history-size 5000
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-list-files-after-cd t
        eshell-destroy-buffer-when-process-dies t
        password-cache t
        password-cache-expiry 3600
        tramp-histfile-override "/dev/null")

  (setenv "PAGER" "cat")

  (defun bc-eshell--setkey ()
    "Customize key in eshell-mode."
    (general-define-key
     :states '(normal visual motion)
     :keymaps 'eshell-mode-map
     "A" 'bc-eshell-goto-prompt
     "H" 'eshell-bol
     "S" 'bc-eshell-toggle-sudo
     "c" 'eshell/evil-change
     "C" 'eshell/evil-change-line
     "d" 'eshell/evil-delete
     "D" 'eshell/evil-delete-line)

    (general-define-key
     :states '(normal visual motion)
     :keymaps 'eshell-mode-map
     :prefix "SPC"
     "q" 'kill-buffer-and-window))

  (add-hook 'eshell-mode-hook #'bc-eshell--setkey)

  (unless (file-exists-p (no-littering-expand-var-file-name "eshell/alias"))
    (require 'em-alias)
    ;; if alias file does not exists, define aliases
    (eshell/alias "su" "eshell/su $*")
    (eshell/alias "sudo" "eshell/sudo $*")
    (eshell/alias "ff" "find-file $1")
    (eshell/alias "FF" "find-file-other-window $1")
    (eshell/alias "cls" "bc-eshell-clear-buffer")
    (eshell/alias "ll" "ls -Aloh --color=always"))

  ;; functions
  (defun eshell/x (file &rest args)
    "Unpack FILE with ARGS using default command."
    (let* ((command
            (-some (lambda (x)
                     (if (string-match-p (car x) file)
                         (cadr x)))
                   '((".*\.tar.bz2" "tar xjf")
                     (".*\.tar.gz" "tar xzf")
                     (".*\.bz2" "bunzip2")
                     (".*\.rar" "unrar x")
                     (".*\.gz" "gunzip")
                     (".*\.tar" "tar xf")
                     (".*\.tbz2" "tar xjf")
                     (".*\.tgz" "tar xzf")
                     (".*\.zip" "unzip")
                     (".*\.Z" "uncompress")
                     (".*" "echo 'Could not unpack the file:'"))))
           (unpack-command
            (concat command " " file " " (mapconcat 'identity args " "))))
      (eshell/printnl "Unpack command: " unpack-command)
      (eshell-command-result unpack-command)))

  (defun bc-eshell-toggle-sudo ()
    "Add/Remove sudo in the begining of command line."
    (interactive)
    (let ((commands (buffer-substring-no-properties
                     (eshell-bol) (point-max))))
      (if (string-match-p "^sudo " commands)
          (progn
            (eshell-bol)
            (while (re-search-forward "sudo " nil t)
              (replace-match "" t nil)))
        (progn
          (eshell-bol)
          (insert "sudo "))))
    (goto-char (point-max))
    (evil-insert-state))

  (defun bc-eshell-open-here ()
    "Open a new shell in the pwd.  If there is already a eshell buffer open for that directory, switch to that buffer."
    (interactive)
    (let* ((dir (file-name-directory (or (buffer-file-name) default-directory)))
           ;; check whether there exists a eshell buffer for DIR
           (exists (seq-filter (lambda (buf)
                                 (with-current-buffer buf
                                   (and (string-equal major-mode "eshell-mode")
                                        (equal dir default-directory))))
                               (buffer-list)))
           ;; check if the matched eshell buffer is visible
           (visible (when exists
                      (get-buffer-window (car exists)))))
      (if visible
          (select-window visible)
        (split-window-below (- (/ (window-total-height) 3)))
        (other-window 1)
        (if exists
            (switch-to-buffer (car exists))
          (eshell 'Z)))
      (goto-char (point-max))
      (evil-insert-state)))

  (defun bc-eshell-open-home ()
    "Open a new shell in ~ using a new window.  If there is already a eshell buffer open for that directory, switch to that buffer."
    (interactive)
    (bc-core-split-window)
    (other-window 1)
    (let ((default-directory "~"))
      (eshell 'Z)
      (goto-char (point-max))
      (evil-insert-state)))

  (defun eshell/sudo (&rest args)
    "Use Tramp to re-implement sudo."
    (interactive)
    (throw
     'eshell-external
     (let* ((dir (file-local-name (expand-file-name default-directory)))
            (default-directory (concat "/sudo:root@localhost:" dir))
            (args (eshell-flatten-list args)))
       (eshell-named-command (car args) (cdr args)))))

  (defun bc-eshell-goto-prompt ()
    "Goto current prompt and continue editting."
    (interactive)
    (goto-char (point-max))
    (evil-insert 1))

  (defun bc-eshell-clear-buffer ()
    "Eshell version of `cls'."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))

  ;; taken from doom-emacs at https://github.com/hlissner/doom-emacs/blob/develop/modules/term/eshell/autoload/evil.el
  (evil-define-operator eshell/evil-change (beg end type register yank-handler delete-func)
    "Like `evil-change' but will not delete/copy the prompt."
    (interactive "<R><x><y>")
    (save-restriction
      (narrow-to-region eshell-last-output-end (point-max))
      (evil-change (max beg (point-min))
                   (if (eq type 'line) (point-max) (min (or end (point-max)) (point-max)))
                   type register yank-handler delete-func)))

  (evil-define-operator eshell/evil-change-line (beg end type register yank-handler)
    "Change to end of line."
    :motion evil-end-of-line
    (interactive "<R><x><y>")
    (eshell/evil-change beg end type register yank-handler #'evil-delete-line))

  (evil-define-operator eshell/evil-delete (beg end type register yank-handler)
    "Like `evil-delete' but will not delete/copy the prompt."
    (interactive "<R><x><y>")
    (save-restriction
      (narrow-to-region eshell-last-output-end (point-max))
      (evil-delete (if beg (max beg (point-min)) (point-min))
                   (if (eq type 'line) (point-max) (min (or end (point-max)) (point-max)))
                   type register yank-handler)))

  (evil-define-operator eshell/evil-delete-line (_beg end type register yank-handler)
    "Change to end of line."
    :motion nil
    :keep-visual t
    (interactive "<R><x>")
    (eshell/evil-delete (point) end type register yank-handler)))

(use-package em-term
  :ensure nil
  :after eshell
  :config
  (dolist (p '("alsamixer" "htop" "ssh" "top"))
      (add-to-list 'eshell-visual-commands p)))

(use-package xterm-color
  :after eshell
  :init
  (defun bc-eshell--set-term-envvar ()
    "Set TERM to term-256color."
    (setenv "TERM" "xterm-256color"))
  :config
  (add-hook 'eshell-mode-hook #'bc-eshell--set-term-envvar)
  (add-hook 'eshell-before-prompt-hook (lambda () (setq xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

;; sudo edit files
(use-package sudo-edit
  :defer t
  :config
  (sudo-edit-indicator-mode))

(provide 'bc-eshell)
;;; bc-eshell.el ends here
