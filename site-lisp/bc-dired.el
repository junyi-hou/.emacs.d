;;; bc-dired.el --- settings for dired mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package dired
  :ensure nil
  :init
  ;; functions
  
  (defun bc-dired--mark-one (cmd)
    "Run command CMD on the file under the cursor."
    (let ((inhibit-read-only t)
          (marked-files (mapcar (lambda (x) (cons x "*")) (dired-get-marked-files))))
      (dired-unmark-all-marks)
      (dired-mark 1)
      (when (commandp cmd)
        (funcall cmd))
      (dired-unmark-all-marks)
      (dired-mark-remembered marked-files)))

  :config
  (evil-set-initial-state 'dired-mode 'motion)

  :general
  (:keymaps 'dired-mode-map
   :state 'motion

   ;; movement
   "j" 'dired-next-line
   "k" 'dired-previous-line
   "J" (lambda () (interactive) (dired-next-line 3))
   "K" (lambda () (interactive) (dired-previous-line 3))
   ">" 'dired-next-subdir
   "<" 'dired-prev-subdir
   "b" 'dired-up-directory

   ;; files
   "<return>" 'dired-find-file
   "<C-return>" 'dired-find-file-other-window
   "n" 'find-file
   "N" 'dired-create-directory
   "t" 'dired-show-file-type
   "y" 'dired-copy-filename-as-kill
   "c" (lambda () (interactive)
         (bc-dired--mark-one 'dired-do-copy))
   "r" (lambda () (interactive)
         (bc-dired--mark-one 'dired-do-rename))
   "d" (lambda () (interactive)
         (bc-dired--mark-one 'dired-do-delete))
   "s" (lambda () (interactive)
         (bc-dired--mark-one 'dired-do-symlink))
   "o" (lambda () (interactive)
         (bc-dired--mark-one 'dired-do-chown))
   "m" (lambda () (interactive)
         (bc-dired--mark-one 'dired-do-chmod))

   ;; marks
   "m" 'dired-mark
   "u" 'dired-unmark
   "C" 'dired-do-copy
   "D" 'dired-do-delete
   "R" 'dired-do-rename
   "S" 'dired-do-symlink
   "M" 'dired-do-chmod
   "O" 'dired-do-chown
   "Z" 'dired-compress
   "M-j" 'dired-next-marked-file
   "M-k" 'dired-prev-marked-file

   "v" 'revert-buffer))

(provide 'bc-dired)
;;; bc-dired.el ends here
