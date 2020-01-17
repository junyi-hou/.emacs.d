;;; gatsby:eshell.el --- my eshell setting

;;; Commentary:

;;; Code:

(require 'gatsby:core)

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
        tramp-histfile-override "/dev/null")

  (setenv "PAGER" "cat")

  (defun gatsby:eshell--setkey ()
    "Customize key in eshell-mode."
    (general-define-key
     :states '(normal visual motion)
     :keymaps 'eshell-mode-map
     "A" 'gatsby:eshell-goto-last-prompt
     "H" 'eshell-bol
     "S" 'gatsby:eshell-toggle-sudo
     "c" 'eshell/evil-change
     "C" 'eshell/evil-change-line
     "d" 'eshell/evil-delete
     "D" 'eshell/evil-delete-line
     "RET" 'gatsby:eshell-open-file-at-point)

    (general-define-key
     :states '(normal visual motion emacs insert)
     :keymaps 'eshell-mode-map
     :prefix "C-c"
     "C-l" 'gatsby:eshell-clear-buffer)

    (general-define-key
     :states '(normal visual motion)
     :keymaps 'eshell-mode-map
     :prefix "SPC"
     "q" 'kill-buffer-and-window))

  (add-hook 'eshell-mode-hook #'gatsby:eshell--setkey)

  (unless (file-exists-p (no-littering-expand-var-file-name "eshell/alias"))
    (require 'em-alias)
    ;; if alias file does not exists, define aliases
    (eshell/alias "su" "eshell/su $*")
    (eshell/alias "sudo" "eshell/sudo $*")
    (eshell/alias "cls" "gatsby:eshell-clear-buffer")
    (eshell/alias "ll" "ls -Aloh --color=always"))

  ;; functions

  (defun gatsby:eshell-goto-last-prompt ()
    "Goto current prompt and continue editting."
    (interactive)
    (goto-char (point-max))
    (evil-insert 1))

  (defun gatsby:eshell--open-or-cd (path)
    "Cd to PATH if path is a directory ((file-name-directory path) => t), otherwise open PATH via `find-file'."
    (interactive)
    (if (file-directory-p path)
        (progn
          (gatsby:comint-goto-last-prompt)
          (insert (concat "cd " path))
          (eshell-send-input)
          (evil-normal-state))
      (find-file path)))

  (defun gatsby:eshell-open-file-at-point ()
    "Try to open file at point.  If not file is found, fallback to `evil-ret'."
    (interactive)
    (let ((filename (symbol-name (symbol-at-point))))
      (cond
       ((file-readable-p filename)
        (gatsby:eshell--open-or-cd filename))
       ((file-readable-p (expand-file-name filename))
        (gatsby:eshell--open-or-cd (expand-file-name filename)))
       (t
        (evil-ret)))))

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

  (defun gatsby:eshell--change-buffer-title ()
    "Change the title of eshell buffer to reflect $pwd."
    (rename-buffer (format "%s: %s" eshell-buffer-name (directory-file-name default-directory)) 'unique))

  (add-hook 'eshell-mode-hook #'gatsby:eshell--change-buffer-title)
  (add-hook 'eshell-directory-change-hook #'gatsby:eshell--change-buffer-title)

  (defun gatsby:eshell-toggle-sudo ()
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

  (defun gatsby:eshell-open-here ()
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

  (defun gatsby:eshell-open-home ()
    "Open a new shell in ~ using a new window.  If there is already a eshell buffer open for that directory, switch to that buffer."
    (interactive)
    (gatsby:core-split-window)
    (other-window 1)
    (let ((default-directory "~"))
      (eshell 'Z)
      (goto-char (point-max))
      (evil-insert-state)))

  (defun eshell/mkcd (dir &rest _)
    "Run \"mkdir dir\" then \"cd dir\""
    (interactive)
    (eshell/mkdir dir)
    (eshell/cd dir))

  (defun gatsby:eshell-clear-buffer ()
    "Eshell version of `cls'."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  ;; zathura integration
  (defun eshell/ff (&rest files)
    "Open FILES in emacs."
    (setq files (eshell-flatten-list files))
    (mapc 'find-file files))

  (defun eshell/FF (&rest files)
    "Open FILES in a new window in emacs."
    (setq files (eshell-flatten-list files))
    (gatsby:core-split-window)
    (other-window 1)
    (mapc 'find-file files))

  (with-eval-after-load 'exwm
    (when (executable-find "zathura")
      (defun gatsby:eshell--separate-pdf (files)
        "Separate pdf from other types of files in FILES. Return a cons cell of all pdf files and other files."
        (let ((out (cons '() '())))
          (mapc (lambda (file)
                  (if (string= (file-name-extension file) "pdf")
                      (push file (car out))
                    (push file (cdr out))))
                files)
          out))

      (defun gatsby:eshell--use-zathura (ff &rest files)
        "Open FILES in emacs, for pdf files in FILES, use zathura to open them."
        (setq files (eshell-flatten-list files))
        (let* ((separated-files (gatsby:eshell--separate-pdf files))
               (pdfs (car separated-files))
               (others (cdr separated-files)))
          (cond ((and pdfs others)
                 (start-process-shell-command "zathura"
                                              nil
                                              (format "tabbed -c zathura %s -e"
                                                      (string-join pdfs " ")))
                 (gatsby:core-split-window)
                 (other-window 1)
                 (apply ff others))
                (pdfs (start-process-shell-command "zathura"
                                                   nil
                                                   (format "tabbed -c zathura %s -e"
                                                           (string-join pdfs " "))))
                (others (when (eq ff 'eshell/FF)
                          (gatsby:core-split-window)
                          (other-window 1))
                        (apply ff others)))))

      (advice-add #'eshell/ff :around #'gatsby:eshell--use-zathura)
      (advice-add #'eshell/FF :around #'gatsby:eshell--use-zathura)))

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
    (eshell/evil-delete (point) end type register yank-handler))

  :general
  (:keymaps '(motion normal visual emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "os" 'gatsby:eshell-open-here
   "oS" 'gatsby:eshell-open-home))

;; TODO use vterm to replace ansi-term
(use-package em-term
  :straight (em-term :type built-in)
  :after eshell
  :config
  (dolist (p '("alsamixer" "htop" "ssh" "top"))
    (add-to-list 'eshell-visual-commands p)))

(use-package em-tramp
  :straight (:type built-in)
  :after eshell
  :config
  ;; load tramp
  (require 'tramp)

  ;; fix tramp PATH problem
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; override eshell/sudo
  (defun gatsby:eshell-sudo (&rest commands)
    "Use `tramp' run COMMAND in /sudo::`default-directory'.  Does not have any flags so won't get error if -i or --user is given."
    (setq commands (eshell-flatten-list commands))
    (if (not commands)
        (gatsby:eshell-su)
      (throw 'eshell-external
             (let ((user "root")
                   (host (or (file-remote-p default-directory 'host) "localhost"))
                   (dir (file-local-name (expand-file-name default-directory)))
                   (prefix (file-remote-p default-directory))
                   (sudo? (string-equal "sudo" (file-remote-p default-directory 'method))))
               (cond (sudo?
                      (eshell-named-command (car commands) (cdr commands)))
                     (prefix
                      (let ((default-directory (format "%s|sudo:%s@%s:%s"
                                                       (substring prefix 0 -1) user host dir)))
                        (eshell-named-command (car commands) (cdr commands))))
                     (t
                      (let ((default-directory (format "/sudo:%s@%s:%s" user host dir)))
                        (eshell-named-command (car commands) (cdr commands)))))))))

  (advice-add #'eshell/sudo :override #'gatsby:eshell-sudo)

  ;; override eshell/su
  (defun gatsby:eshell-su (&rest _)
    "toggle between `default-directory' and /sudo::`default-directory'."
    (let ((user "root")
          (host (or (file-remote-p default-directory 'host) "localhost"))
          (dir (file-local-name (expand-file-name default-directory)))
          (prefix (file-remote-p default-directory))
          (sudo? (string-equal "sudo" (file-remote-p default-directory 'method))))
      (if sudo?
          ;; in sudo mode, go back to non-sudo
          (let ((new-prefix (replace-regexp-in-string
                             (format "[|/]sudo:root@%s" host) ""
                             prefix)))
            (eshell/cd (if (string= ":" new-prefix) dir (format "%s%s" new-prefix dir))))
        ;; in non-sudo mode, go to sudo
        (if prefix
            (eshell/cd
             (format "%s|sudo:%s@%s:%s" (substring prefix 0 -1) user host dir))
          (eshell/cd (format "/sudo:%s@%s:%s" user host dir))))))

  (advice-add #'eshell/su :override #'gatsby:eshell-su)

  ;; better `eshell/cd'
  (defun gatsby:eshell-cd (cd &rest args)
    "Make `eshell/cd' tramp-aware."
    (let* ((host (file-remote-p default-directory))
           (home (format "%s/home/%s" host (file-remote-p default-directory 'user))))
      (if (and (not args)
               host)
          (if (file-exists-p home)
              (funcall cd home)
            (funcall cd (format "%s/" host)))
        (funcall cd args))))

  (advice-add #'eshell/cd :around #'gatsby:eshell-cd))

;; sudo edit files
(use-package sudo-edit
  :defer t
  :config
  (sudo-edit-indicator-mode)
  :general
  (:keymaps 'normal
   :prefix "SPC"
   "se" 'sudo-edit))

(provide 'gatsby:eshell)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; gatsby:eshell.el ends here
