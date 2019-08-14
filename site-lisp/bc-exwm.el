;;; bc-exwm.el --- emacs X windowm manager config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'tramp)

(use-package exwm
  :init

  ;; rename buffer to window title
  (defun bc-exwm-rename-buffer-to-title ()
    (exwm-workspace-rename-buffer exwm-title))
  (add-hook 'exwm-update-title-hook 'ambrevar/exwm-rename-buffer-to-title)
  (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

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
    ;; TODO: filter out some bins
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
           ;;; HACK: why isn't (direction (if up '+ '-)) working???
           (direction (if up "+" "-"))
           (new-level (if up
                          (+ current-level 25)
                        (- current-level 25)))
           (new-level (if (> 0 new-level) 0 new-level)))
      (let ((inhibit-message t))
        (write-region
         (format "%d" new-level)
         nil
         (concat "/sudo:root@localhost:" bl-file)))
      (message (concat "brightness " direction))))

  (require 'exwm-config)
  (exwm-config-default)

  :general

  (:keymaps '(motion normal visual insert emacs)
  "s-d" 'bc-exwm-launch-x-with-ivy
  "s-r" 'exwm-reset
  "s-x" 'exwm-input-toggle-keyboard
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
