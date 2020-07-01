;;; gatsby:lisp.el --- setup ide features (auto-completion, code-jump, linter) for lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby:core)
(require 'gatsby:comint)

(use-package elisp-mode
  :straight (:type built-in)
  :init
  ;; fix tab width
  (defun gatsby:lisp--set-tab-width ()
    (setq-local tab-width 2))

  (defconst gatsby:lisp-hs-block-start-keywords
    (rx (* blank) "(" (or
                       ;; definitions should always get folded
                       "defun" "defmacro" "defcustom" "defconst" "defvar" "defvar-local"
                       ;; I also want to fold the followings in my use-pacakge definitions
                       ":keymaps" "evil-define" "with-eval-after-load"
                       "dired-rainbow-define"))
    "Regexp of forms that should be folded by `hs-minor-mode'.
All forms that start at the `beginning-of-line' will be folded. Other forms should be folded only when it matches these keywords.")

  (defun gatsby:lisp--setup-hs-block-start-regexp ()
    "Fold only definitions in elisp mode"
    (setq hs-block-start-regexp
          (concat "^\\((\\|" gatsby:lisp-hs-block-start-keywords "\\)")))

  :hook
  (emacs-lisp-mode . gatsby:lisp--set-tab-width)
  (emacs-lisp-mode . gatsby:lisp--setup-hs-block-start-regexp))

(use-package ielm
  :straight (:type built-in)
  :hook (ielm-mode . company-mode)
  :init

  (defun gatsby:ielm-repl (&optional buf-name)
    "Start an ielm REPL."
    (interactive)
    (let* (old-point
           (buf-name (or buf-name "*ielm*"))
           (buf (get-buffer-create buf-name)))
      (unless (comint-check-proc buf-name)
        (with-current-buffer buf
          (unless (zerop (buffer-size)) (setq old-point (point)))
          (inferior-emacs-lisp-mode)))
      (when old-point (push-mark old-point))
      buf))

  (advice-add #'ielm :override #'gatsby:ielm-repl)

  (defun gatsby:lisp-start-or-pop-to-repl ()
    "Pop to the associated REPL, if such REPL does not exist, start one."
    (interactive)
    (unless gatsby:comint-repl-buffer
      (gatsby:comint--start-repl 'ielm))
    (gatsby:comint--pop-to-repl))
  
  (defun gatsby:lisp-associate-repl ()
    "Associate current buffer to a REPL"
    (interactive)
    (gatsby:comint-associate-repl 'inferior-emacs-lisp-mode))

  (defun gatsby:lisp-eval-sexp-or-region ()
    (interactive)
    (if (region-active-p)
        (gatsby:comint--eval-region 'ielm-return (region-beginning) (region-end))
      (gatsby:comint--eval-last-sexp 'ielm-return)))

  :general
  (:keymaps '(emacs-lisp-mode-map
              lisp-interaction-mode-map)
   :states '(motion normal visual)
   :prefix "SPC"
   "rr" 'gatsby:lisp-eval-sexp-or-region
   "rh" 'helpful-symbol
   "rz" 'gatsby:lisp-associate-repl
   "ro" 'gatsby:lisp-start-or-pop-to-repl)

  (:keymaps 'inferior-emacs-lisp-mode-map
   :states '(normal motion visual)
   "SPC" 'nil)

  (:keymaps 'inferior-emacs-lisp-mode-map
   :states 'insert
   "<return>" 'ielm-return)

  (:keymaps 'inferior-emacs-lisp-mode-map
   :states '(normal motion visual)
   :prefix "SPC"
   "q" 'gatsby:comint-exit-repl))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (scheme-mode . aggressive-indent-mode))

(use-package helpful
  :init
  (evil-set-initial-state 'helpful-mode 'motion)
  :general
  (:keymaps '(motion normal visual emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   ;; helps
   "hf" 'helpful-callable
   "hk" 'helpful-key
   "hv" 'helpful-variable
   "hm" 'describe-mode))

(use-package easy-escape
  :hook ((emacs-lisp-mode lisp-mode ielm-mode) . easy-escape-minor-mode))

(use-package geiser
  ;; provide code-completion and documentation
  :hook
  (scheme-mode . geiser-mode)
  :custom
  (scheme-program-name "guile")
  (geiser-default-implementation 'guile)
  (geiser-repl-mode-map (make-sparse-keymap))
  :init
  ;;; ===============================
  ;;  REPL settings
  ;;; ===============================

  (defun gatsby:scheme--to-repl-buffer (impl)
    "Same as `geiser-repl--to-repl-buffer', but instead of pop/switch to the REPL buffer, return the REPL buffer."
    (unless (and (eq major-mode 'geiser-repl-mode)
                 (eq geiser-impl--implementation impl)
                 (not (get-buffer-process (current-buffer))))
      (let* ((old (geiser-repl--repl/impl impl geiser-repl--closed-repls))
             (old (and (buffer-live-p old)
                       (not (get-buffer-process old))
                       old))
             (buf (or old (generate-new-buffer (geiser-repl--buffer-name impl)))))
        (unless old
          (with-current-buffer buf
            (geiser-repl-mode)
            (geiser-impl--set-buffer-implementation impl)
            (geiser-syntax--add-kws t)))
        buf)))

  (defun gatsby:scheme--start-repl (impl address)
    "Same as `geiser-repl--start-repl', but return the REPL buffer instead."
    (message "Starting Geiser REPL ...")
    (when (not address) (geiser-repl--check-version impl))
    (let ((buffer (current-buffer))
          (repl-buffer (geiser-repl--to-repl-buffer impl)))
      (setq geiser-repl--last-scm-buffer buffer)
      (with-current-buffer repl-buffer
        (goto-char (point-max))
        (geiser-repl--autodoc-mode -1)
        (let* ((prompt-rx (geiser-repl--prompt-regexp impl))
               (deb-prompt-rx (geiser-repl--debugger-prompt-regexp impl))
               (prompt (geiser-con--combined-prompt prompt-rx deb-prompt-rx)))
          (unless prompt-rx
            (error "Sorry, I don't know how to start a REPL for %s" impl))
          (geiser-repl--save-remote-data address)
          (geiser-repl--start-scheme impl address prompt)
          (geiser-repl--quit-setup)
          (geiser-repl--history-setup)
          (add-to-list 'geiser-repl--repls (current-buffer))
          (geiser-repl--set-this-buffer-repl (current-buffer))
          (setq geiser-repl--connection
                (geiser-con--make-connection (get-buffer-process (current-buffer))
                                             prompt-rx
                                             deb-prompt-rx))
          (geiser-repl--startup impl address)
          (geiser-repl--autodoc-mode 1)
          (geiser-company--setup geiser-repl-company-p)
          (add-hook 'comint-output-filter-functions
                    'geiser-repl--output-filter
                    nil
                    t)
          (set-process-query-on-exit-flag (get-buffer-process (current-buffer))
                                          geiser-repl-query-on-kill-p)
          (message "%s up and running!" (geiser-repl--repl-name impl))))
      repl-buffer))

  (advice-add #'geiser-repl--to-repl-buffer :override
              #'gatsby:scheme--to-repl-buffer)
  (advice-add #'geiser-repl--start-repl :override
              #'gatsby:scheme--start-repl)

  (defun gatsby:scheme-start-or-pop-to-repl ()
    "Pop to `gatsby:scheme-repl-buffer'.  If `gatsby:scheme-repl-buffer' is nil, start a new repl."
    (interactive)
    (unless gatsby:comint-repl-buffer
      (gatsby:comint--start-repl 'run-geiser geiser-default-implementation))
    (gatsby:comint--pop-to-repl))

  (defun gatsby:scheme-associate-repl ()
    "Associate current buffer to a REPL"
    (interactive)
    (gatsby:comint-associate-repl 'geiser-repl-mode))

  (defun gatsby:scheme-eval-sexp-or-region ()
    (interactive)
    (if (region-active-p)
        (gatsby:comint--eval-region 'geiser-repl--maybe-send (region-beginning) (region-end))
      (gatsby:comint--eval-last-sexp 'geiser-repl--maybe-send)))

  :general
  (:keymaps 'geiser-repl-mode-map
   :states '(normal motion visual)
   "SPC" 'nil)

  (:keymaps 'geiser-repl-mode-map
   :states 'insert
   "<return>" 'geiser-repl--maybe-send)

  (:keymaps 'geiser-repl-mode-map
   :states '(normal motion visual)
   :prefix "SPC"
   "q" 'gatsby:comint-exit-repl)

  (:keymaps 'scheme-mode-map
   :states '(motion normal visual)
   :prefix "SPC"
   "rr" 'gatsby:scheme-eval-sexp-or-region
   "rh" 'geiser-doc-symbol-at-point
   "rz" 'gatsby:scheme-associate-repl
   "ro" 'gatsby:scheme-start-or-pop-to-repl))

(provide 'gatsby:lisp)
;;; gatsby:lisp.el ends here
