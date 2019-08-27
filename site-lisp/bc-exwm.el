;;; bc-exwm.el --- emacs X windowm manager config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'exwm)
(require 'tramp)

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

(defun bc-exwm--external-monitor-p ()
  "Return the port of (first) attached external monitor.  If there is no, return nil."
  (let* ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
         (default-output "eDP1"))
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror 2)
      (let* ((port (match-string 1)))
        (unless (string-equal default-output port)
          port)))))

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
      '(;; arrow keys
        ([?\M-j] . [down])
        ([?\M-k] . [up])
        ([?\M-h] . [left])
        ([?\M-l] . [right])
        ;; c-g = esc
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

;; multi-screen support
(let ((external-monitor? (bc-exwm--external-monitor-p)))
  (when external-monitor?
    (require 'exwm-randr)
    ;; if external monitor is attached, the internal monitor needs only
    ;; 1 workspace
    (setq exwm-randr-workspace-monitor-plist
          (seq-reduce
           'append
           `((0 "eDP1")
             ,@(mapcar (lambda (i)
                         `(,i ,external-monitor?))
                       (number-sequence 1 9)))
           '()))
    ;; call xrandr
    (call-process
     "xrandr" nil nil nil
     "--output" "eDP1" "--auto"
     "--output" external-monitor? "--right-of" "eDP1" "--auto")
    (exwm-randr-enable)
    ))

(server-start)
(exwm-enable)

;; TODO:
;; advice c-h to change workspace focus
;; abstract xrandr as a function with arguments orientation and external-monitor?

(provide 'bc-exwm)
;;; bc-exwm.el ends here
