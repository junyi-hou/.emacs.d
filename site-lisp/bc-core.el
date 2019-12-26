;;; bc-core.el --- settings that should be loaded for every buffer each time emacs starts -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; GC
(use-package gcmh
  :straight
  (gcmh :host gitlab :repo "koral/gcmh")
  :init
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)

  (add-hook 'after-init-hook
            (defun bc-core--reset-gc ()
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.2)))

  (gcmh-mode 1))

;; turn off bell
(setq-default visible-bell t
              ring-bell-function 'ignore)

;; mini window should be mini
(setq-default max-mini-window-height 1)

;; smooth scroll
(setq-default scroll-step 1
              scroll-conservatively 10000
              auto-window-vscroll nil)

(defun bc-core--split-vertical (window)
  "Return t if should split WINDOW vertically, otherwise return nil."
  (let* ((h (window-height window))
         (w (window-width window))
         (ratio (/ (float h) w)))
    (cond
     ((< ratio 0.15) t)
     ((< (/ (float w) 2) 80) nil)
     (t t))))

(defun bc-core-split-window (&optional window)
  "Split WINDOW side-by-side, if WINDOW width < 90, split it top-and-down."
  (let ((window (or window (selected-window))))
    (if (bc-core--split-vertical window)
        (split-window-right)
      (split-window-below))))

(setq-default split-window-preferred-function 'bc-core-split-window)

;; always use y-or-n-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; in-house packages that needs to be enable at all time
(setq-default inhibit-splash-screen t
              inhibit-startup-message t
              inhibit-startup-echo-area-message t)

;; cannot delete *scratch* buffer
(defun bc-core--unkillable-scratch ()
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        (insert initial-scratch-message)
        nil)
    t))

(add-hook 'kill-buffer-query-functions
          #'bc-core--unkillable-scratch)

;; disable menu tool and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)

;; other minor modes I always want
(show-paren-mode 1)          ; highlight matching paren
(global-visual-line-mode 1)  ; word wrapping
(global-subword-mode 1)      ; better camelCase support
(global-auto-revert-mode 1)  ; automatically refresh file when it changes
(use-package recentf
  :hook (after-init . recentf-mode)
  :init (setq
         recentf-save-file       "~/.emacs.d/var/recentf"
         recentf-max-saved-items 100
         recentf-exclude         '("/tmp/" "/ssh:"))
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory))
(use-package hideshow
  :init
  ;; don't make me move to the beginning of line before expanding the block
  (advice-add #'hs-show-block :before #'beginning-of-visual-line)
  ;; don't fold comments
  (setq hs-hide-comments-when-hiding-all nil)

  ;; after revert-buffer, properly hide blocks
  (defun bc-core--hs-fix (&rest _)
    "Advising `revert-buffer' to properly show/hide blocks."
    (hs-show-all)
    (hs-hide-all))

  (advice-add #'revert-buffer :after #'bc-core--hs-fix)

  :hook
  (prog-mode . hs-hide-all)
  (prog-mode . hs-minor-mode))
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
  (defun bc-eldoc--box-position (_ height)
    "Display `eldoc-box' in the bottom left corner of the `selected-window'."
    (let* ((window (selected-window))
           (y (- (nth 3 (window-inside-pixel-edges window)) 5  height))
           (x (window-pixel-left window)))
      (cons x y)))

  (setq eldoc-box-position-function #'bc-eldoc--box-position))
(use-package general
  :demand t
  :hook (emacs-lisp-mode . bc-lisp--fix-indent)
  :init

  (defun bc-lisp--fix-indent ()
    "Fix https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned"
    (setq-local lisp-indent-function #'bc-lisp-indent-function))

  ;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
  (defun bc-lisp-indent-function (indent-point state)
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
(use-package beacon
  :init
  (setq beacon-blink-when-window-scrolls nil)
  :config
  (beacon-mode 1))

;; indentation settings
(setq-default indent-tabs-mode nil
              tab-width 4
              python-indent-offset 4
              electric-indent-inhibit t  ; don't change indentation for me
              indicate-empty-lines nil)

(setq-default backward-delete-char-untabify-method 'hungry)  ; bs kill whole tab

(provide 'bc-core)
;;; bc-core.el ends here
