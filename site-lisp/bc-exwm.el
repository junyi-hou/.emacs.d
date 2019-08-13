;;; bc-exwm.el --- emacs X windowm manager config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'tramp)

(use-package exwm
  :init
  (setq exwm-workspace-number 2)

  ;; functions

  (defun bc-exwm--flatten (l)
    "Flatten list L."
    (mapcan (lambda (x) (if (listp x) x nil)) l))

  (defun bc-exwm--get-all-bins ()
    "Traverse the $PATH variable and collect all executable"
    (bc-exwm--flatten
     (seq-concatenate
      'list
      (mapcar
       (lambda (path)
         (if (file-exists-p path)
             (mapcar
              #'car
              (directory-files-and-attributes path nil "^[^\.]"))
           '()))
       (split-string (getenv "PATH") ":")))))
  
  (defun bc-exwm-launch-x-with-ivy ()
    "Use `ivy-mode' as interface for launching x applications in exwm."
    (interactive)
    (ivy-read "run: "
    (bc-exwm--get-all-bins)
    :action (lambda (x)
              (start-process-shell-command x nil x))))

  (defun bc-exwm-amixer-adjust-volume (&optional up x)
    "Change volume by X percent, increase volume if UP is not-nil, otherwise decrease volume.  If X is nil, change by 5%."
    (let* ((x (or x 5))
           (direction (if up "+" "-")))
      (let ((inhibit-message t))
        (shell-command
         (concat "amixer -q sset Master " (format "%d" x) "%" direction)))
    (message (concat "volume " direction))))

  (defun bc-exwm-adjust-backlight (&optional up)
    "Adjust backlight, if UP is non-nil, increase the brightness, otherwise decrease."
    (let* ((bl-file "/sys/class/backlight/intel_backlight/brightness")
           (current-level (string-to-number (with-temp-buffer
                                              (insert-file-contents bl-file)
                                              (buffer-string))))
           (direction (if up "+" "-"))
           (new-level (if up
                          (+ current-level (* 0.05 current-level))
                        (- current-level (* 0.05 current-level)))))
      (let ((inhibit-message t))
        (write-region
         (format "%d" new-level)
         nil
         (concat "/sudo:root@localhost:" bl-file)))
      (message (concat "brightness " direction))))

  :general

  (:keymaps '(motion normal visual)
  "s-d" #'bc-exwm-launch-x-with-ivy
  "<XF86AudioLowerVolume>" (lambda () (interactive)
                             (bc-exwm-amixer-adjust-volume))
  "<XF86AudioRaiseVolume>" (lambda () (interactive)
                             (bc-exwm-amixer-adjust-volume t))
  "<XF86MonBrightnessUp>" (lambda () (interactive)
                            (bc-exwm-adjust-backlight t))
  "<XF86MonBrightnessDown>" (lambda () (interactive)
                              (bc-exwm-adjust-backlight))))


(provide 'bc-exwm)
;;; bc-exwm.el ends here
