;;; bc-exwm.el --- emacs X windowm manager config -*- lexical-binding: t; -*-

;;; Commentary:

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
         (direction (if up '+ '-))
         (new-level (funcall direction current-level 25))
         (new-level (if (> 0 new-level) 0 new-level)))
    (let ((inhibit-message t))
      (write-region
       (format "%d" new-level)
       nil
       (concat "/sudo:root@localhost:" bl-file)))
    (message (concat "brightness " (symbol-name direction)))))

(defun bc-exwm-switch-to-workspace-confirm (index)
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

(defun bc-exwm--get-xwindow-buffer ()
  "Return a list of buffers containing currently opened Xwindows."
  (seq-filter
   (lambda (buffer)
     (with-current-buffer buffer
       (equal major-mode 'exwm-mode)))
   (buffer-list)))

(defun bc-exwm-switch-to-xwindow (xwindow)
  "Display XWINDOW in the current window."
  (interactive
   (list
    (ivy-read
     "swtich to: "
     (mapcar 'buffer-name (bc-exwm--get-xwindow-buffer))
     :action (lambda (x) (get-buffer x)))))
  (let ((buffer (seq-find (lambda (x)
                            (string= (buffer-name x) xwindow))
                          (bc-exwm--get-xwindow-buffer))))
    (exwm-workspace-move-window exwm-workspace-current-index (exwm--buffer->id buffer))))

;;; ===============================
;;  multi-monitor setup
;;; ===============================

(require 'exwm-randr)

(defconst bc-exwm--default-monitor "eDP1"
  "The internal screen output.")

(defvar bc-exwm--relative-layout nil
  "How the position of external monitor relates to `bc-exwm--default-monitor'.")

(defvar bc-exwm--external-monitor-workspace-index nil
  "The index of the workspace displayed on the external monitor.")

(defconst bc-exwm--direction-pairs-alist
  '(("left" . "right")
    ("right" . "left")
    ("up" . "down")
    ("down" . "up"))
  "Pairs of direction, arranged as (dir . dir-opposite).")

(defun bc-exwm--monitor-status ()
  "Return lists of external monitor ports separately according to whether they are connected or not, i.e., (list of connected ports, list of disconnected ports)."
  (let* ((xrandr-output-regexp "\n\\([^ ]+\\) \\(dis\\)?connected ")
         (connected)
         (disconnected))
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (while (re-search-forward xrandr-output-regexp (point-max) 'noerror)
        (if (string-match-p "disconnected" (match-string 0))
            (push (match-string 1) disconnected)
          (push (match-string 1) connected)))
      ;; return
      `(,(seq-filter
          (lambda (mon)
            (not (string= mon bc-exwm--default-monitor)))
          connected)
        ,disconnected))))

(defun bc-exwm--assign-workspaces (monitor)
  "Assigning workspaces 1 - 8 to MONITOR and 0 to `bc-exwm--default-monitor'."
  (setq exwm-randr-workspace-monitor-plist
        (seq-reduce
         'append
         (mapcar (lambda (i) `(,i ,monitor)) (number-sequence 1 8))
         `(0 ,bc-exwm--default-monitor))))

(defun bc-exwm--turn-on-external-monitor (monitor position)
  "Turn on external MONITOR.  POSITION determines the relative position of MONITOR to the builtin monitor `bc-exwm--default-monitor'."
  (bc-exwm--assign-workspaces monitor)
  (call-process
   "xrandr" nil nil nil
   "--output" monitor position "eDP1" "--auto"))

(defun bc-exwm--tracking-external-monitor-workspace-index (index &optional index-range)
  "Update `bc-exwm--external-monitor-workspace-index' to INDEX if INDEX is in INDEX-RANGE.

This function should be called after `exwm-workspace-switch' is called.

HACK: only work in conjecture of `bc-exwm--assign-workspaces' and with 1 external monitors."
  (let ((index-range (or index-range (number-sequence 2 8))))
    (when (and (car (bc-exwm--monitor-status))
               (member index index-range))
      (setq bc-exwm--external-monitor-workspace-index index))))

(advice-add 'exwm-workspace-switch :after 'bc-exwm--tracking-external-monitor-workspace-index)

(defun bc-exwm--xrandr-to-direction (xrandr-argument)
  "Translate XRANDR-ARGUMENT to direction"
  (string-match "\\(right\\|left\\|above\\|below\\)" xrandr-argument)
  (let ((result (match-string 1 xrandr-argument)))
    (cond
     ((string= result "above") "up")
     ((string= result "below") "down")
     (t result))))

(defun bc-exwm--windmove-most (dir)
  "Move to the DIR most window of the `selected-frame'."
  (ignore-errors
    (while t
      (windmove-do-window-select (obarray-get obarray dir)))))

(defun bc-exwm--adviced-p (predicates fun)
  "Return FUN's advices that satisfies PREDICATES.  If there is no advice that satisfies PREDICATES or there is no advice at all, return nil.

Adapted from https://emacs.stackexchange.com/questions/33020/how-can-i-remove-an-unnamed-advice/33021#33021."
  (let (result)
    (advice-mapc
     (lambda (ad props)
       (when (funcall predicates ad)
         (push ad result)))
     fun)
    (nreverse result)))

(defun bc-exwm--windmove-advice-remove ()
  "Remove advice of windmove-{left,right,up,down} so it is what it was."
  (dolist (fun '(windmove-left windmove-right windmove-up windmove-down))
    (let ((advice (bc-exwm--adviced-p
                  (lambda (f) (string-match-p "bc-exwm--" (symbol-name f)))
                  fun)))
      (when advice
        (advice-remove fun (car advice))))))

(defun bc-exwm--windmove-advice-add (position)
  "Generate functions based on external monitor's relative POSITION and advicing them to windmove-{left, right, up, down}."
  (let* ((dir (bc-exwm--xrandr-to-direction position))
         (opp-dir (cdr (assoc dir bc-exwm--direction-pairs-alist)))
         (windmove-same (obarray-get obarray (concat "windmove-" dir)))
         (windmove-oppo (obarray-get obarray (concat "windmove-" opp-dir))))

    (defun bc-exwm--advice-windmove-same (windmove &rest args)
      (if (eq exwm-workspace-current-index 0)
          ;; in internal monitor
          (condition-case nil
              ;; if not error, business as usual
              (apply windmove args)
            ;; if there is error -- at the boundary window of the external monitor,
            ;; need to move to the opposite-most window of the internal monitor
            (error
             (progn
               (exwm-workspace-switch bc-exwm--external-monitor-workspace-index)
               (bc-exwm--windmove-most opp-dir))))
        ;; in external monitor, not affected
        (apply windmove args)))

    (defun bc-exwm--advice-windmove-oppo (windmove &rest args)
      (if (eq exwm-workspace-current-index 0)
          ;; in internal monitor, not affected
          (apply windmove args)
        ;; in external monitor
        (condition-case nil
            ;; if not error, business as usual
            (apply windmove args)
          ;; if there is error -- at the boundary window of the internal monitor,
          ;; need to move to the opposite-most window of the external monitor
          (error
           (progn
             (exwm-workspace-switch 0)
             (bc-exwm--windmove-most dir))))))

    (advice-add windmove-same :around #'bc-exwm--advice-windmove-same)
    (advice-add windmove-oppo :around #'bc-exwm--advice-windmove-oppo)))

;; automatically adjust display when external monitor plug in/out
(defun bc-exwm--auto-adjust-display ()
  "Automatically adjust display.

This function first scan for video port status via `bc-exwm--monitor-status', then use xrandr to turn on/off screens, finally, according to the relative position of the internal/external monitor, advice windmove functions."
  (let* ((port-status (bc-exwm--monitor-status))
         (connected (caar port-status))
         (disconnected (cadr port-status)))

    (mapc (lambda (monitor)
            (call-process
             "xrandr" nil nil nil
             "--output" monitor "--off"))
          disconnected)

    (bc-exwm--windmove-advice-remove)

    (when connected
      (let ((position (ivy-read
                    "Relative position: "
                    '("--right-of" "--left-of" "--above" "--below" "--same-as")
                    :action 'identity)))
        (setq bc-exwm--relative-layout position)
        (bc-exwm--turn-on-external-monitor connected position)
        (bc-exwm--windmove-advice-add position)))))

(add-hook 'exwm-randr-screen-change-hook #'bc-exwm--auto-adjust-display)


;;; ===============================
;;  org-integration
;;; ===============================

(with-eval-after-load 'org
  (progn
   (defmacro bc-exwm--capture (app type link description &rest args)
     "Capture LINK of TYPE with DESCRIPTION in exwm APP."
     `(lambda ()
        (when (and (equal major-mode 'exwm-mode)
                   (equal exwm-class-name ,app))
          (org-store-link-props
           :type ,type
           :link ,link
           :description ,description))))

   (org-link-set-parameters
    "zathura"
    :store (bc-exwm--capture "Zathura" "file" (buffer-name) ""))

   ;; HACK - need to tell qutebrowser to set window-title = url
   (org-link-set-parameters
    "qutebrowser"
    :store (bc-exwm--capture "qutebrowser" "http" (buffer-name) ""))))

;;; ===============================
;;  Settings
;;; ===============================

;; rename buffer to window title
(defun bc-exwm-rename-buffer-to-title ()
  (exwm-workspace-rename-buffer exwm-title))

(add-hook 'exwm-update-title-hook 'bc-exwm-rename-buffer-to-title)
(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

;; initial workspace - start only 2
(setq exwm-workspace-number 2)

;; leader key in exwm buffer M-SPC
(evil-set-initial-state 'exwm-mode 'emacs)

;; make sure the exwm buffer is always in emacs state
(defun bc-exwm--back-emacs-state ()
  "Automatically switch back to `evil-emacs-state' if not already in it."
  (when (and (equal major-mode 'exwm-mode)
             (not (evil-emacs-state-p)))
    (evil-emacs-state)))

(add-hook
 'exwm-manage-finish-hook
 (defun bc-exwm--hook ()
   (add-hook 'post-command-hook #'bc-exwm--back-emacs-state nil t)))

;; keys

;; keys that should be interpreted by emacs in
;; addition to simulate and global keys
(push ?\s-\  exwm-input-prefix-keys)
(setq exwm-input-prefix-keys (delete ?\C-u exwm-input-prefix-keys))

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
                        (bc-exwm-switch-to-workspace-confirm (- ,i 1)))))
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
        ;; mouse clicks
        ([left-click] . [left-click])
        ([right-click] . [right-click])
        ([\C-right-click] . [\C-right-click])
        ([\C-left-click] . [\C-left-click])))

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

(use-package exwm-edit
  ;; C-c ' in X windows
  :quelpa (exwm-edit
           :repo "junyi-hou/exwm-edit"
           :fetcher github
           :stable nil)
  :init
  (add-hook 'exwm-edit-compose-hook #'evil-insert-state)

  (defun bc-exwm--auto-select-all (&rest args)
    "Advice `exwm-edit--compose' to send fake <C-a> to the exwm window."
    (exwm-input--fake-key ?\C-a)
    (sleep-for 0.05))

  (advice-add 'exwm-edit--compose :before #'bc-exwm--auto-select-all))

(provide 'bc-exwm)
;;; bc-exwm.el ends here
