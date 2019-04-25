;;; bc-eshell.el -- my eshell setting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package eshell
  :init
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t))

(use-package company-shell
  :after company
  :commands (company-shell company-shell-env))

(require 'bc-core)
(require 'bc-autocomplete)

(defun bc-eshell--format-path-name (path)
  "Formatting a given PATH.

For a given absolute path:
1. replace $HOME with ~
2. short handing all previous directories with their first letter

return the formatted path name."
  (bc-eshell--shorten-path (bc-eshell--replace-home path)))

(defun bc-eshell--replace-home (path)
  "Replace home in PATH with tilde (~) character."
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length path) home-len)
         (equal home (substring path 0 home-len)))
        (concat "~" (substring path home-len))
      path)))

(defun bc-eshell--shorten-path (path)
  "Shorten all directory names in PATH except the last two."
  (let ((p-lst (split-string path "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                               (substring elm 0 1)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      path)))  ;; Otherwise, we just return the PATH

(defun bc-eshell--open (dir)
  "Open an eshell in directory DIR."
  (let* ((default-directory dir)
         (name (bc-eshell--format-path-name dir)))
    (eshell)
    (rename-buffer (concat "*" name "*"))))

(defun bc-eshell-open-here ()
  "Open a new shell in the pwd of the current buffer.  If there is already a eshell buffer open for the pwd, switch to that buffer."
  (interactive)
  (let* ((dir (file-name-directory (or (buffer-file-name) default-directory)))
         (height (/ (window-total-height) 3)))
    (split-window-vertically (- height))
    (evil-window-down 1)
    (if (get-buffer (concat "*" (bc-eshell--format-path-name dir) "*"))
        (switch-to-buffer (concat "*" (bc-eshell--format-path-name dir) "*"))
      (bc-eshell--open dir))))

(defun bc-eshell-open-file-in-parent-buffer (file)
  "Open FILE from eshell in the window above the current eshell buffer."
    (evil-window-up 1)
    (find-file file))

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

(defun bc-eshell-cd (dir)
  "Change directory to DIR.  Modify the eshell buffer name accordingly."
  (interactive)
  (let ((dir (if (or (file-name-absolute-p dir)
                     (string-equal (substring dir 0 1) "-")
                     (string-equal (substring dir 0 1) "."))
                 dir
               (expand-file-name dir))))
   (eshell/cd dir)
   (rename-buffer (concat "*" (bc-eshell--format-path-name dir) "*"))))


;; settings
;; a bug in eshell mode makes direct defining keymap impossible, need to run
;; hook

;; alias
;; (eshell/alias "ff" "bc-eshell-open-file-in-parent-buffer $1")
;; (eshell/alias "cls" "bc-eshell-clear-buffer")
;; (eshell/alias "l" "ls -AlohG --color=always")
;; (eshell/alias "cd" "bc-eshell-cd $1")

(defun bc-eshell--keymaps ()
  "Define keymap for eshell."
  (general-define-key
   :states 'normal
   :keymaps 'eshell-mode-map
   "A" 'bc-eshell-goto-prompt)

  (general-define-key
   :states 'normal
   :keymaps 'eshell-mode-map
   :prefix "SPC"
   "q" 'kill-buffer-and-window))

(defun bc-eshell--autocomplete ()
  "Enable `company-mode' for `eshell-mode'."
  (interactive)
   (make-local-variable 'company-backends)
   (setq company-backends '((company-files company-capf company-yasnippet company-shell company-shell-env)))
   (company-mode 1))


(add-hook 'eshell-mode-hook #'bc-eshell--keymaps)
(add-hook 'eshell-mode-hook #'bc-eshell--autocomplete)


(provide 'bc-eshell)
;;; bc-eshell.el ends here
