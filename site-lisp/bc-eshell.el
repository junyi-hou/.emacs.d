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
        eshell-destroy-buffer-when-process-dies t)

  (setenv "PAGER" "cat")

  (defun bc-eshell--setkey ()
    "Customize key in eshell-mode."
    (general-define-key
     :states '(normal visual motion)
     :keymaps 'eshell-mode-map
     "A" 'bc-eshell-goto-prompt
     "H" 'eshell-bol
     "S" 'bc-eshell-toggle-sudo)

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
    (bc-core--split-window)
    (other-window 1)
    (let ((default-directory "~"))
      (eshell 'Z)
      (goto-char (point-max))
      (evil-insert-state)))

  (defun bc-eshell-goto-prompt ()
    "Goto current prompt and continue editting."
    (interactive)
    (goto-char (point-max))
    (evil-insert 1))

  (defun bc-eshell-clear-buffer ()
    "Eshell version of `cls'."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(use-package em-term
  :ensure nil
  :after eshell
  :config
  (dolist (p '("alsamixer" "htop" "ssh" "top"))
      (add-to-list 'eshell-visual-commands p)))

;; FIXME: need to call su before call sudo, otherwise causing recursive load
(use-package em-tramp
  :ensure nil
  :after eshell
  :init
  (setq password-cache t
        password-cache-expiry 3600)
  (add-to-list 'eshell-modules-list #'eshell-tramp))

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
