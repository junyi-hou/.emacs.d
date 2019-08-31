;;; bc-exwm.el --- emacs X windowm manager config -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO: exwm-xrandr does not play well with emacs daemon -- when daemon started
;; the monitor does not connected yet and therefore the script will not run

;;; Code:

(require 'exwm)

;;; ===============================
;;  functions
;;; ===============================

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
  ;; TODO: some bins need to be called with arguments, how to deal with that?
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

(defun bc-exwm-switch-to-workplace-confirm (index)
  "Switch to workspace INDEX, if it doesn't exists, ask whether to create it."
  (interactive
   (list
    (cond
     ((integerp current-prefix-arg)
      current-prefix-arg)
     (t 0))))
  (let ((total (exwm-workspace--count))
        (inhibit-message t))
    (if (< index total)
        (exwm-workspace-switch index)
      (if (y-or-n-p (format "workspace %d doesn't exists, create?" index))
          (exwm-workspace-switch-create index)))))

;;; ===============================
;;  multi-monitor setup
;;; ===============================

(require 'exwm-randr)

(defconst bc-exwm--default-monitor "eDP1"
  "The internal screen output.")
(defvar bc-exwm--relative-layout nil
  "How the position of external monitor relates to `bc-exwm--default-monitor'.")

(defun bc-exwm--external-monitor-p ()
  "Return the name of (first) attached external monitor.  If there is no, return nil."
  (let* ((xrandr-output-regexp "\n\\([^ ]+\\) connected "))
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror 2)
      (let* ((output (match-string 1)))
        (unless (string-equal bc-exwm--default-monitor output)
          output)))))

(defun bc-exwm--turn-off-external-monitor ()
  "Turn off all external monitors."
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) disconnected "))
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (while (and (re-search-forward xrandr-output-regexp nil 'noerror)
                  (not (string-equal (match-string 1) bc-exwm--default-monitor)))
        (call-process
         "xrandr" nil nil nil
         "--output" (match-string 1) "--off")))))

(defun bc-exwm-turn-on-external-monitor (position)
  "Turn on the monitor identified by `bc-exwm--external-monitor-p'.  POSITION determines the relative position of the new monitor to the builtin monitor `bc-exwm--default-monitor'.  If POSITION is given, use it, otherwise read it from input."
  (interactive
   (list (ivy-read
          "Relative position: "
          '("--right-of" "--left-of" "--above" "--below" "--same-as")
          :action 'identity)))
  (setq bc-exwm--relative-layout position)
  (let ((ext-mon (bc-exwm--external-monitor-p)))
    (when ext-mon
      (bc-exwm--assign-workspaces ext-mon)
      (call-process
       "xrandr" nil nil nil
       "--output" "eDP1" "--auto"
       "--output" ext-mon position "eDP1" "--auto"))))

(defun bc-exwm--assign-workspaces (ext-mon)
  "Assigning workspaces 1 - 8 to EXT-MON and 0 to eDP1."
  (setq exwm-randr-workspace-monitor-plist
        (seq-reduce
         'append
         (mapcar (lambda (i) `(,i ,ext-mon)) (number-sequence 1 8))
         `(0 ,bc-exwm--default-monitor))))

;; automatically adjust display when external monitor plug in/out
(defun bc-exwm--auto-adjust-display ()
  "Automatically adjust display by calling `bc-exwm--turn-off-external-monitor' and `bc-exwm-turn-on-external-monitor'."
  (if (bc-exwm--external-monitor-p)
      (call-interactively 'bc-exwm-turn-on-external-monitor)
    (bc-exwm--turn-off-external-monitor)))

(add-hook 'exwm-randr-screen-change-hook #'bc-exwm--auto-adjust-display)

;; TODO use C-{hjkl} to move across workspaces as well
(defmacro bc-exwm--switch-workspaces (switch-window-fn))

;;; ===============================
;;  Settings
;;; ===============================

;; rename buffer to window title
(defun bc-exwm-rename-buffer-to-title ()
  (exwm-workspace-rename-buffer exwm-title))

(add-hook 'exwm-update-title-hook 'bc-exwm-rename-buffer-to-title)
(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

;; settings
;; initial workspace - start only 2
(setq exwm-workspace-number 2)

;; leader key in exwm buffer M-SPC
(evil-set-initial-state 'exwm-mode 'emacs)
(push ?\s-\  exwm-input-prefix-keys)

(setq exwm-input-global-keys
      `(;; app launcher
        ([?\s-d] . bc-exwm-launch-x-with-ivy)
        ;; to line mode
        ([?\s-r] . exwm-reset)
        ;; toggle between line and char mode
        ([?\s-x] . exwm-input-toggle-keyboard)
        ;; switch to and from workspaces
        ;; workspaces start from 0, but keymaps should start from 1
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (bc-exwm-switch-to-workplace-confirm (- ,i 1)))))
                  (number-sequence 1 9))
        ;; multimedia keys
        ([XF86AudioLowerVolume] . (lambda () (interactive)
                                    (bc-exwm-amixer-adjust-volume)))
        ([XF86AudioRaiseVolume] . (lambda () (interactive)
                                    (bc-exwm-amixer-adjust-volume t)))
        ([XF86MonBrightnessUp] . (lambda () (interactive)
                                   (bc-exwm-adjust-backlight t)))
        ([XF86MonBrightnessDown] . (lambda () (interactive)
                                     (bc-exwm-adjust-backlight)))))

;; simulation key
(setq exwm-input-simulation-keys
      '(;; c-g = esc
        ([?\C-g] . [escape])
        ;; yank; paste: find a solution
        ([?\s-y] . [?\C-c])
        ([?\s-p] . [?\C-v])))

;; line-mode keybinding
(general-define-key
 :keymaps 'exwm-mode-map
 "C-h" 'evil-window-left
 "C-j" 'evil-window-down
 "C-k" 'evil-window-up
 "C-l" 'evil-window-right)

;;; ===============================
;;  launch exwm
;;; ===============================

(server-start)
(exwm-enable)
(exwm-randr-enable)

(when (bc-exwm--external-monitor-p)
  (call-interactively #'bc-exwm--external-monitor-p))

(provide 'bc-exwm)
;;; bc-exwm.el ends here
