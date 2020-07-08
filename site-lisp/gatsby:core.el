;;; gatsby:core.el --- settings that should be loaded for every buffer each time emacs starts -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; GC
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          (defun gatsby:core--reset-gc ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.2)))

;; fix potential credential issues
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; extend max message length
(setq message-log-max 10000)

;; turn off bell
(setq-default visible-bell t
              ring-bell-function 'ignore)

;; mini window should be mini
(setq-default max-mini-window-height 1)

;; smooth scroll
(setq-default scroll-step 1
              scroll-conservatively 10000
              auto-window-vscroll nil)

;; split windows
(defun gatsby:core--split-vertical (window)
  "Return t if should split WINDOW vertically, otherwise return nil."
  (let* ((h (window-height window))
         (w (window-width window))
         (ratio (/ (float h) w)))
    (cond
     ((< ratio 0.15) t)
     ((< (/ (float w) 2) 80) nil)
     (t t))))

(defun gatsby:core-split-window (&optional window)
  "Split WINDOW side-by-side, if WINDOW width < 90, split it top-and-down.  If SWITCH is non-nil, switch to the newly splitted window."
  (let ((window (or window (selected-window))))
    (if (gatsby:core--split-vertical window)
        (split-window-right)
      (split-window-below))))

(setq-default split-window-preferred-function 'gatsby:core-split-window)

;; always use y-or-n-p
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default inhibit-splash-screen t
              inhibit-startup-message t
              inhibit-startup-echo-area-message t)

;; cannot delete *scratch* buffer
(defun gatsby:core--unkillable-scratch ()
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        (insert initial-scratch-message)
        nil)
    t))

(add-hook 'kill-buffer-query-functions
          #'gatsby:core--unkillable-scratch)

;; indentation settings
(setq-default indent-tabs-mode nil
              tab-width 4
              python-indent-offset 4
              electric-indent-inhibit t  ; don't change indentation for me
              indicate-empty-lines nil)

;; bs kill whole tab
(setq-default backward-delete-char-untabify-method 'hungry)

;; color log appropriately
(defun gatsby:core--apply-ansi-color ()
  "Color the whole buffer using ansi code."
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-to-list 'auto-mode-alist '("\\.log\\'" . gatsby:core--apply-ansi-color))

;; disable menu tool and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)


;;; essential packages

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; regularly refresh package repos
(let* ((update-record (expand-file-name "var/straight-last-repo-update"
                                        user-emacs-directory))
       (last-update (if (file-exists-p update-record)
                        (with-temp-buffer
                          (insert-file-contents update-record)
                          (string-to-number (buffer-string)))
                      1))
       (today (time-to-days (current-time))))
  (when (> (- today last-update) 7)
    (straight-pull-package "melpa")
    (straight-pull-package "gnu-elpa-mirror")
    (straight-pull-package "emacsmirror-mirror")

    ;; update update-time
    (with-temp-buffer
      (insert (int-to-string today))
      (write-region (point-min) (point-max) update-record))))

;; install use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      straight-vc-git-default-protocol 'https)


(use-package no-littering
  ;; do not litter my .emacs.d
  :defines
  (no-littering-etc-directory
   no-littering-var-directory)
  :demand t
  :config
  (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "var/" user-emacs-directory)
        auto-save-file-name-transforms  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup/")))
        custom-file (no-littering-expand-etc-file-name "custom.el"))

  (load custom-file 'noerror))
(use-package paren
  ;; highlight matching paren
  :config
  (show-paren-mode 1))
(use-package subword
  ;; better camelCase support
  :config
  (global-subword-mode 1))
(use-package simple
  ;; word wrapping
  :straight (:type built-in)
  :config
  (global-visual-line-mode 1))
(use-package autorevert
  ;; automatically refresh file when it changes
  :config
  (global-auto-revert-mode 1))
(use-package recentf
  :custom
  (recentf-save-file       "~/.emacs.d/var/recentf")
  (recentf-max-saved-items 100)
  (recentf-exclude         `("/tmp/" "/ssh:" ,no-littering-var-directory))
  :config
  (recentf-mode 1))
(use-package hideshow
  :init
  ;; `hs-show-block' is recursively open,
  ;; so implement open my self
  ;; FIXME if hs-show-block fails, it will instead close the whole block
  (defun gatsby:core-hs-show-block ()
    "Open only one level of hidden block."
    (interactive)
    (let ((inhibit-message t))
      (hs-show-block)
      (call-interactively 'hs-hide-level)))

  ;; fix it in evil
  (with-eval-after-load 'evil
    (setq evil-fold-list
          (add-to-list 'evil-fold-list
                       `((hs-minor-mode)
                         :open-all hs-show-all
                         :close-all hs-hide-all
                         :close hs-hide-block
                         :toggle hs-toggle-hiding
                         :open gatsby:core-hs-show-block
                         :open-rec hs-show-block))))

  ;; hideshow mode enhancement
  ;; by default, `hs-minor-mode' only keep the first line of the hidden block
  ;; shown. This means function signature will be hidden if it spans over multiple
  ;; lines. I find this sub-optimal. So I take advantage of
  ;; `hs-adjust-block-beginning' and modify the beginning of the hiding point of each
  ;; block.
  (defcustom gatsby:core-hs-block-beginning-regexp-alist
    '((python-mode . ":\n"))
    "Alist of the regexp for the beginning of hiding point (or the end of the banner).")

  (defun gatsby:core--hs-adjust-block-begin (block-beginning)
    "Adjust the starting point of the hiding block to the end of function/class signature according to `gatsby:core-hs-block-beginning-regexp-alist'. If the current major-mode is not in the list, return BLOCK-BEGINNING."
    (interactive)
    (if-let* ((regexp (alist-get major-mode gatsby:core-hs-block-beginning-regexp-alist)))
        (save-excursion
          (re-search-forward regexp nil 'noerror)
          (1- (point)))
      block-beginning))

  (defun gatsby:core--hs-move-point-to-block-begin (&rest _)
    "Move point according to `gatsby:core--hs-block-begin', to make sure I can open the block at point."
    (when (alist-get major-mode gatsby:core-hs-block-beginning-regexp-alist)
      (goto-char (gatsby:core--hs-adjust-block-begin (point)))))

  (defun gatsby:core--hs-set-adjust-block-beginning ()
    "`hs-adjust-block-beginning' is automatically bind locally to `nil'. Set it properly instead"
    (setq hs-adjust-block-beginning #'gatsby:core--hs-adjust-block-begin))

  ;; put them in effect
  (advice-add #'hs-show-block :before #'gatsby:core--hs-move-point-to-block-begin)

  ;; don't fold comments
  (setq hs-hide-comments-when-hiding-all nil)

  ;; after revert-buffer, properly hide blocks
  (defun gatsby:core--hs-fix (&rest _)
    "Advising `revert-buffer' to properly show/hide blocks."
    (hs-show-all)
    (hs-hide-all))

  (advice-add #'revert-buffer :after #'gatsby:core--hs-fix)

  :hook
  (prog-mode . hs-hide-all)
  (prog-mode . hs-minor-mode)
  (hs-minor-mode . gatsby:core--hs-set-adjust-block-beginning))
(use-package prescient
  :init
  (setq prescient-save-file (concat no-littering-var-directory "prescient-save.el"))
  :config
  (prescient-persist-mode))
(use-package page-break-lines
  :config (global-page-break-lines-mode))
(use-package eldoc-box
  :hook ((text-mode prog-mode) . eldoc-box-hover-mode)
  :config
  (defun gatsby:eldoc-box--get-frame (buffer)
    "Overriding `eldoc-box--get-frame'.

The original function creates a visible frame at the bottom right corner of the screen, which is super annoying. This function fix that."
    (if eldoc-box--inhibit-childframe
        ;; if inhibit display, do nothing
        eldoc-box--frame
      (let* ((after-make-frame-functions nil)
             (before-make-frame-hook nil)
             (parameter (append eldoc-box-frame-parameters
                                `((default-minibuffer-frame . ,(selected-frame))
                                  (minibuffer . ,(minibuffer-window))
                                  (left-fringe . ,(frame-char-width)))))
             window frame
             (main-frame (selected-frame)))
        (if (and eldoc-box--frame (frame-live-p eldoc-box--frame))
            (progn (setq frame eldoc-box--frame)
                   (setq window (frame-selected-window frame))
                   ;; in case the main frame changed
                   (set-frame-parameter frame 'parent-frame main-frame))
          (setq window (display-buffer-in-child-frame
                        buffer
                        `((child-frame-parameters . ,parameter))))
          (setq frame (make-frame parameter))
          (with-selected-frame frame
            (delete-other-windows)
            (switch-to-buffer buffer)
            (setq window (get-buffer-window))))

        (set-face-attribute 'fringe frame :background nil :inherit 'eldoc-box-body)
        (set-window-dedicated-p window t)
        (redirect-frame-focus frame (frame-parent frame))
        (set-face-attribute 'internal-border frame :inherit 'eldoc-box-border)
        ;; set size
        (eldoc-box--update-childframe-geometry frame window)
        (setq eldoc-box--frame frame)
        (run-hook-with-args 'eldoc-box-frame-hook main-frame)
        (make-frame-visible frame))))
  (advice-add #'eldoc-box--get-frame :override #'gatsby:eldoc-box--get-frame)

  (defun gatsby:eldoc-box--position (width height)
    "Display `eldoc-box' in the bottom right corner of the `selected-window'."
    (let* ((edge (window-inside-pixel-edges))
           (y (- (nth 3 edge) 5 height))
           (x (- (nth 2 edge) 2 width)))
      (cons x y)))

  (defun gatsby:eldoc-box--update-parent-frame (frame)
    "Update frame parameters of `eldoc-box--frame' when it is called.  Run with `eldoc-box-frame-hook'."
    (set-frame-parameter eldoc-box--frame 'parent-frame frame))

  (add-hook 'eldoc-box-frame-hook #'gatsby:eldoc-box--update-parent-frame)

  (setq eldoc-box-position-function #'gatsby:eldoc-box--position
        eldoc-box-cleanup-interval 0.5)
  (setf (alist-get 'internal-border-width eldoc-box-frame-parameters) 2))
(use-package general
  ;; for key binding
  :demand t
  :hook (emacs-lisp-mode . gatsby:lisp--fix-indent)
  :init

  (defun gatsby:lisp--fix-indent ()
    "Fix https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned"
    (setq-local lisp-indent-function #'gatsby:lisp-indent-function))

  ;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
  (defun gatsby:lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state)))))))))
(use-package so-long
  :straight
  (emacs-so-long
   :repo "hlissner/emacs-so-long"
   :host github)
  :config
  (global-so-long-mode))

(provide 'gatsby:core)
;;; gatsby:core.el ends here
