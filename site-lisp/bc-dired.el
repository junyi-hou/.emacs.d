;;; bc-dired.el --- settings for dired mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package dired
  :straight (dired :type built-in)
  :defer t
  :config
  (evil-set-initial-state 'dired-mode 'motion)
  (setq dired-listing-switches "-alh")

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

  :general
  (:keymaps 'dired-mode-map
   :states 'motion

   ;; sort
   "s" 'dired-sort-toggle-or-edit

   ;; movement
   "j" 'dired-next-line
   "k" 'dired-previous-line
   "J" (lambda () (interactive) (dired-next-line 3))
   "K" (lambda () (interactive) (dired-previous-line 3))
   ">" 'dired-next-subdir
   "<" 'dired-prev-subdir
   "<backspace>" 'dired-up-directory

   ;; files
   "<return>" 'dired-find-file
   "<C-return>" 'dired-find-file-other-window
   "f" 'find-file
   "F" 'dired-create-directory
   "t" 'dired-show-file-type
   "y" 'dired-copy-filename-as-kill
   "c" (lambda () (interactive)
         (bc-dired--mark-one 'dired-do-copy))
   "r" (lambda () (interactive)
         (bc-dired--mark-one 'dired-do-rename))
   "d" (lambda () (interactive)
         (bc-dired--mark-one 'dired-do-delete))
   "o" (lambda () (interactive)
         (bc-dired--mark-one 'dired-do-chown))
   "m" (lambda () (interactive)
         (bc-dired--mark-one 'dired-do-chmod))

   ;; marks
   "m" 'dired-mark
   "u" 'dired-unmark
   "U" 'dired-unmark-all-marks
   "C" 'dired-do-copy
   "D" 'dired-do-delete
   "R" 'dired-do-rename
   "M" 'dired-do-chmod
   "O" 'dired-do-chown
   "Z" 'dired-compress
   "M-j" 'dired-next-marked-file
   "M-k" 'dired-prev-marked-file)

  (:keymaps 'dired-mode-map
   :states 'motion
   :prefix "SPC"
   "r" 'revert-buffer))

(provide 'bc-dired)
;;; bc-dired.el ends here
