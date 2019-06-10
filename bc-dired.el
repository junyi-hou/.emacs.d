;;; bc-dired.el -- settings for dired mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package dired
  :ensure nil
  :config
  (evil-set-initial-state 'dired-mode 'motion)
  (setq dired-mode-map (make-sparse-keymap))
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
   "C-<return>" 'dired-find-file-other-window
   "n" 'find-file
   "N" 'dired-create-directory
   "t" 'dired-show-file-type
   "y" 'dired-copy-filename-as-kill
   "c" (lambda () (interactive)
         (dired-unmark-all-marks) (dired-mark 1) (dired-do-copy))
   "r" (lambda () (interactive)
         (dired-unmark-all-marks) (dired-mark 1) (dired-do-rename))
   "d" (lambda () (interactive)
         (dired-unmark-all-marks) (dired-mark 1) (dired-do-delete))
   "s" (lambda () (interactive)
         (dired-unmark-all-marks) (dired-mark 1) (dired-do-symlink))
   "o" (lambda () (interactive)
         (dired-unmark-all-marks) (dired-mark 1) (dired-do-chown))
   "m" (lambda () (interactive)
         (dired-unmark-all-marks) (dired-mark 1) (dired-do-chmod))

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
