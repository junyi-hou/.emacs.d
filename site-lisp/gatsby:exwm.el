;;; gatsby:exwm.el --- emacs X windowm manager config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package exwm
  :init
  (defun gatsby:exwm-amixer-adjust-volume (&optional up x)
    "Change volume by X percent, increase volume if UP is not-nil, otherwise decrease volume.  If X is nil, change by 5%."
    (let* ((x (or x 5))
           (direction (if up "+" "-")))
      (let ((inhibit-message t))
        (shell-command
         (concat "amixer -q sset Master " (format "%d" x) "%" direction)))
      (message (concat "volume: " (with-temp-buffer
                                    (insert (shell-command-to-string "amixer sget Master"))
                                    (goto-char 0)
                                    (search-forward-regexp "\\(\\[\\([[:digit:]]+\\)%\\]\\)")
                                    (match-string 2))))))

  (defun gatsby:exwm-amixer-toggle-mute ()
    "Toggle mute."
    (interactive)
    (let* ((new-state (with-temp-buffer
                        (insert (shell-command-to-string "amixer sset Master toggle"))
                        (goto-char 0)
                        (re-search-forward "\\[\\(off\\|on\\)\\]")
                        (match-string 1))))
      (message "Master volume %s" new-state)))

  (defun gatsby:exwm-adjust-backlight (&optional up)
    "Adjust backlight, if UP is non-nil, increase the brightness, otherwise decrease."
    (let* ((bl-file "/sys/class/backlight/intel_backlight/brightness")
           (bl-max "/sys/class/backlight/intel_backlight/max_brightness")
           (current-level (string-to-number (with-temp-buffer
                                              (insert-file-contents bl-file)
                                              (buffer-string))))
           (direction (if up '+ '-))
           (new-level (funcall direction current-level 50))
           (new-level (if (> 0 new-level) 0 new-level)))
      (let ((inhibit-message t))
        (write-region
         (format "%d" new-level)
         nil
         (concat "/sudo:root@localhost:" bl-file)))
      (message "brightness %2d"
               (let ((current (with-temp-buffer
                                (insert-file-contents bl-file)
                                (string-to-number (buffer-string))))
                     (max (with-temp-buffer
                            (insert-file-contents bl-max)
                            (float (string-to-number (buffer-string))))))
                 (* 100 (/ current max))))))

  (defun gatsby:exwm-switch-to-workspace-confirm (index)
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

  (defun gatsby:exwm--get-xwindow-buffer ()
    "Return a list of buffers containing currently opened Xwindows."
    (--filter (with-current-buffer it (equal major-mode 'exwm-mode))
              (buffer-list)))

  (defun gatsby:exwm-switch-to-xwindow ()
    "Choose a xwindow from `ivy-read' and display it in the current window or a new window in the current frame."
    (interactive)
    (ivy-read
     "swtich to: "
     (mapcar 'buffer-name (gatsby:exwm--get-xwindow-buffer))
     :action
     (lambda (x)
       (when ivy-current-prefix-arg
         (gatsby:core--split-window)
         (other-window 1))
       (exwm-workspace-move-window
        (selected-frame)
        (exwm--buffer->id (get-buffer x))))))

  ;; FIXME unusable
  (when (executable-find "zathura")
    (defun gatsby:exwm-open-pdf-in-zathura (ff &rest args)
      "If selected file is a pdf, open in `zathura'. Otherwise open using FF."
      (let ((filename (car args)))
        (if (string= "pdf" (file-name-extension filename))
            (progn
              (gatsby:core-split-window)
              (other-window 1)
              (start-process-shell-command "zathura"
                                           nil
                                           (format "zathura '%s'" filename)))
          (apply ff args))))

    (advice-add #'find-file-noselect :around #'gatsby:exwm-open-pdf-in-zathura))

  :config  
  ;;  multi-monitor setup

  (require 'exwm-randr)

  (defconst gatsby:exwm--default-monitor "eDP1"
    "The internal screen output.")

  (defvar gatsby:exwm--relative-layout nil
    "How the position of external monitor relates to `gatsby:exwm--default-monitor'.")

  (defvar gatsby:exwm--external-monitor-workspace-index nil
    "The index of the workspace displayed on the external monitor.")

  (defconst gatsby:exwm--direction-pairs-alist
    '(("left" . "right")
      ("right" . "left")
      ("up" . "down")
      ("down" . "up"))
    "Pairs of direction, arranged as (dir . dir-opposite).")

  (defun gatsby:exwm--monitor-status ()
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
        `(,(--filter (not (string= it gatsby:exwm--default-monitor)) connected)
          ,disconnected))))

  (defun gatsby:exwm--assign-workspaces (monitor)
    "Assigning workspaces 1 - 8 to MONITOR and 0 to `gatsby:exwm--default-monitor'."
    (setq exwm-randr-workspace-monitor-plist
          (-concat `(0 ,gatsby:exwm--default-monitor)
                   (--mapcat `(,it ,monitor) (number-sequence 1 8)))))

  (defun gatsby:exwm--turn-on-external-monitor (monitor position)
    "Turn on external MONITOR.  POSITION determines the relative position of MONITOR to the builtin monitor `gatsby:exwm--default-monitor'."
    (gatsby:exwm--assign-workspaces monitor)
    (call-process
     "xrandr" nil nil nil
     "--output" monitor position "eDP1" "--auto"))

  (defun gatsby:exwm--tracking-external-monitor-workspace-index (index &optional index-range)
    "Update `gatsby:exwm--external-monitor-workspace-index' to INDEX if INDEX is in INDEX-RANGE.

This function should be called after `exwm-workspace-switch' is called.

HACK: only work in conjecture of `gatsby:exwm--assign-workspaces' and with 1 external monitors."
    (let ((index-range (or index-range (number-sequence 1 8))))
      (when (and (car (gatsby:exwm--monitor-status))
                 (member index index-range))
        (setq gatsby:exwm--external-monitor-workspace-index index))))

  (advice-add 'exwm-workspace-switch
              :after
              'gatsby:exwm--tracking-external-monitor-workspace-index)

  (defun gatsby:exwm--xrandr-to-direction (xrandr-argument)
    "Translate XRANDR-ARGUMENT to direction"
    (string-match "\\(right\\|left\\|above\\|below\\)" xrandr-argument)
    (let ((result (match-string 1 xrandr-argument)))
      (cond
       ((string= result "above") "up")
       ((string= result "below") "down")
       (t result))))

  (defun gatsby:exwm--windmove-most (dir)
    "Move to the DIR most window of the `selected-frame'."
    (ignore-errors
      (while t
        (windmove-do-window-select (obarray-get obarray dir)))))

  (defun gatsby:exwm--adviced-p (predicates fun)
    "Return FUN's advices that satisfies PREDICATES.  If there is no advice that satisfies PREDICATES or there is no advice at all, return nil.

Adapted from https://emacs.stackexchange.com/questions/33020/how-can-i-remove-an-unnamed-advice/33021#33021."
    (let (result)
      (advice-mapc
       (lambda (ad props)
         (when (funcall predicates ad)
           (push ad result)))
       fun)
      (nreverse result)))

  (defun gatsby:exwm--windmove-advice-remove ()
    "Remove advice of windmove-{left,right,up,down} so it is what it was."
    (dolist (fun '(windmove-left windmove-right windmove-up windmove-down))
      (let ((advice (gatsby:exwm--adviced-p
                     (lambda (f) (string-match-p "gatsby:exwm--" (symbol-name f)))
                     fun)))
        (when advice
          (advice-remove fun (car advice))))))

  (defun gatsby:exwm--windmove-advice-add (position)
    "Generate functions based on external monitor's relative POSITION and advicing them to windmove-{left, right, up, down}."
    (let* ((dir (gatsby:exwm--xrandr-to-direction position))
           (opp-dir (cdr (assoc dir gatsby:exwm--direction-pairs-alist)))
           (windmove-same (obarray-get obarray (concat "windmove-" dir)))
           (windmove-oppo (obarray-get obarray (concat "windmove-" opp-dir))))

      (defun gatsby:exwm--advice-windmove-same (windmove &rest args)
        (if (eq exwm-workspace-current-index 0)
            ;; in internal monitor
            (condition-case nil
                ;; if not error, business as usual
                (apply windmove args)
              ;; if there is error -- at the boundary window of the external monitor,
              ;; need to move to the opposite-most window of the internal monitor
              (error
               (progn
                 (exwm-workspace-switch gatsby:exwm--external-monitor-workspace-index)
                 (gatsby:exwm--windmove-most opp-dir))))
          ;; in external monitor, not affected
          (apply windmove args)))

      (defun gatsby:exwm--advice-windmove-oppo (windmove &rest args)
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
               (gatsby:exwm--windmove-most dir))))))

      (advice-add windmove-same :around #'gatsby:exwm--advice-windmove-same)
      (advice-add windmove-oppo :around #'gatsby:exwm--advice-windmove-oppo)))

  ;; automatically adjust display when external monitor plug in/out
  (defun gatsby:exwm--auto-adjust-display ()
    "Automatically adjust display.

This function first scan for video port status via `gatsby:exwm--monitor-status', then use xrandr to turn on/off screens, finally, according to the relative position of the internal/external monitor, advice windmove functions."
    (let* ((port-status (gatsby:exwm--monitor-status))
           (connected (caar port-status))
           (disconnected (cadr port-status)))

      (mapc (lambda (monitor)
              (call-process
               "xrandr" nil nil nil
               "--output" monitor "--off"))
            disconnected)

      (gatsby:exwm--windmove-advice-remove)

      (when connected
        (let ((position (ivy-read
                         "Relative position: "
                         '("--right-of" "--left-of" "--above" "--below" "--same-as")
                         :action 'identity)))
          (setq gatsby:exwm--relative-layout position)
          (gatsby:exwm--turn-on-external-monitor connected position)
          (gatsby:exwm--windmove-advice-add position)

          ;; finally, init the default workspace on the external monitor
          (setq gatsby:exwm--external-monitor-workspace-index 1)))))

  (add-hook 'exwm-randr-screen-change-hook #'gatsby:exwm--auto-adjust-display)
  
  ;;  org-integration

  (with-eval-after-load 'org
    (progn
      (defmacro gatsby:exwm--capture (app type link description &rest args)
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
       :store (gatsby:exwm--capture "Zathura" "file" (buffer-name) ""))

      ;; HACK - need to tell qutebrowser to set window-title = url
      (org-link-set-parameters
       "qutebrowser"
       :store (gatsby:exwm--capture "qutebrowser" "http" (buffer-name) ""))))
  
  ;;  Settings

  ;; rename buffer to window title
  (defun gatsby:exwm-rename-buffer-to-title ()
    (exwm-workspace-rename-buffer exwm-title))

  (add-hook 'exwm-update-title-hook 'gatsby:exwm-rename-buffer-to-title)
  (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

  ;; initial workspace - start only 2
  (setq exwm-workspace-number 2)

  ;; leader key in exwm buffer M-SPC
  (evil-set-initial-state 'exwm-mode 'emacs)

  ;; make sure the exwm buffer is always in emacs state
  (defun gatsby:exwm--back-emacs-state ()
    "Automatically switch back to `evil-emacs-state' if not already in it."
    (when (and (equal major-mode 'exwm-mode)
               (not (evil-emacs-state-p)))
      (evil-emacs-state)))

  (add-hook
   'exwm-manage-finish-hook
   (defun gatsby:exwm--hook ()
     (add-hook 'post-command-hook #'gatsby:exwm--back-emacs-state nil t)

     (defun gatsby:exwm--delete-window-when-kill ()
       (when (< (length (window-list)) 2)
         (switch-to-buffer-other-window "*scratch*")
         (other-window -1))
       (delete-window))

     (add-hook 'kill-buffer-hook #'gatsby:exwm--delete-window-when-kill nil t)))
  
  ;; keys

  ;; keys that should be interpreted by emacs in
  ;; addition to simulate and global keys
  (push ?\s-\  exwm-input-prefix-keys)
  (setq exwm-input-prefix-keys (delete ?\C-u exwm-input-prefix-keys))

  (setq exwm-input-global-keys
        `(;; to line mode
          ([?\s-r] . exwm-reset)
          ;; toggle between line and char mode
          ([?\s-x] . exwm-input-toggle-keyboard)
          ;; switch to and from workspaces
          ;; workspaces start from 0, but keymaps should start from 1
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (gatsby:exwm-switch-to-workspace-confirm (- ,i 1)))))
                    (number-sequence 1 9))
          ;; the last workspace (9) is s-h
          ([?\s-h] . (lambda () (interactive) (gatsby:exwm-switch-to-workspace-confirm 9)))
          ;; multimedia keys
          ([XF86AudioLowerVolume] . (lambda () (interactive)
                                      (gatsby:exwm-amixer-adjust-volume)))
          ([XF86AudioMute] . gatsby:exwm-amixer-toggle-mute)
          ([XF86AudioRaiseVolume] . (lambda () (interactive)
                                      (gatsby:exwm-amixer-adjust-volume t)))
          ([XF86MonBrightnessUp] . (lambda () (interactive)
                                     (gatsby:exwm-adjust-backlight t)))
          ([XF86MonBrightnessDown] . (lambda () (interactive)
                                       (gatsby:exwm-adjust-backlight)))))

  ;; simulation key
  (setq exwm-input-simulation-keys
        '(;; c-g = esc
          ([?\C-g] . [escape])
          ;; mouse clicks
          ([left-click] . 'mouse-1)
          ([right-click] . 'mouse-2)
          ;; for tabbed
          ([?\C-\S-l] . [?\C-\S-l]) ;; next tab
          ([?\C-\S-h] . [?\C-\S-h]) ;; prev tab
          ))

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

  :general
  (:keymaps '(motion normal visual emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "lb" (lambda ()
          (interactive)
          (start-process-shell-command (getenv "BROWSER") nil (getenv "BROWSER")))
   "lp" (lambda ()
          "Open a pdf file, with `ivy-current-prefix-arg', open the file in zathura, otherwise open file in pdf-tools."
          (interactive)
          (ivy-read
           "open pdf file: "
           #'read-file-name-internal
           ;; use predicate to filter out non-pdfs
           :predicate
           (lambda (x)
             (or (string= "pdf" (file-name-extension x))
                 (string= (substring x (1- (length x))) "/")))
           :initial-input "~/downloads/"
           :action (lambda (x)
                     (start-process-shell-command "zathura"
                                                  nil
                                                  (format "zathura '%s'" x)))))))

;; TODO: improve this
(use-package exwm-edit
  ;; C-c ' in X windows
  :straight
  (exwm-edit :repo "junyi-hou/exwm-edit" :host github)
  :init
  (add-hook 'exwm-edit-compose-hook #'evil-insert-state)

  (defun gatsby:exwm--auto-select-all (&rest args)
    "Advice `exwm-edit--compose' to send fake <C-a> to the exwm window."
    (exwm-input--fake-key ?\C-a)
    (sleep-for 0.05))

  (advice-add 'exwm-edit--compose :before #'gatsby:exwm--auto-select-all))

(provide 'gatsby:exwm)
;;; gatsby:exwm.el ends here
