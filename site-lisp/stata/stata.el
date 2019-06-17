;;; stata.el -- providing major mode for editing do files -*- lexical-binding: t; -*-

;;; Commentary:
;; the syntax table is borrowed directly from ado-mode (https://github.com/louabill/ado-mode)
;; REPL integration is taken from emacs-juyter and stata_kernel

;;; Code:

(require 'dev-jupyter)
(require 'stata-font-lock)

(defgroup stata-mode nil
  "Stata mode: major mode for editing do files."
  :group 'local
  :prefix "stata-mode-")

(defcustom stata-mode-hook nil
  "Hook for Stata mode."
  :type '(hook)
  :options '(turn-on-auto-fill)
  :group 'stata-mode)

(defcustom stata-mode-stata-home "/usr/local/stata14/"
  "Installation directory for Stata."
  :type 'string
  :group 'stata-mode)

(defcustom stata-mode-ado-home "~/Documents/bin/ado"
  "The ado directory for Stata."
  :type 'string
  :group 'stata-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.do\\'" . stata-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ado\\'" . stata-mode))

;; syntax table
(defvar stata-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\\ "." tbl) ;nullify escape meaning
    (modify-syntax-entry ?\$ "." tbl)
    (modify-syntax-entry ?` "(\'" tbl)
    (modify-syntax-entry ?\' ")`" tbl)
    (modify-syntax-entry ?+ "." tbl)
    (modify-syntax-entry ?- "." tbl)
    (modify-syntax-entry ?= "." tbl)
    (modify-syntax-entry ?% "." tbl)
    (modify-syntax-entry ?< "." tbl)
    (modify-syntax-entry ?> "." tbl)
    (modify-syntax-entry ?& "." tbl)
    (modify-syntax-entry ?| "." tbl)
    (modify-syntax-entry ?~ "." tbl)
    ;;--------- begin cut-and-paste from  lisp/progmodes/c-langs.el
    (modify-syntax-entry ?/  ". 124b" tbl)
    (modify-syntax-entry ?*  ". 23n"   tbl)
    (modify-syntax-entry ?\n "> b"  tbl)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b" tbl)
    ;;--------- end cut-and-paste ------------------
    tbl)
  "Syntax table used while in `stata-mode'.")

;; stata-mode-map: define keymaps
(defvar stata-mode-map (make-sparse-keymap) "Keymap for `stata-mode'.")

(define-derived-mode stata-mode prog-mode "Stata"
  "Major mode for editing do and ado files.

\\{stata-mode-map}"

  (setq-local comment-column 40)
  (setq-local comment-end " \*/")
  (setq-local comment-start "/\* ")
  (setq-local comment-start-skip "/\\*+ *")
  (setq-local comment-use-syntax t)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local paragraph-separate (concat  "[ \t\f]*$\\|" page-delimiter))
  (setq-local paragraph-start (concat "[ \t\f]*$\\|" page-delimiter))

  (setq font-lock-defaults
        '(stata-mode-font-lock-keywords nil nil ((?\. . "w"))))

  (run-mode-hooks 'stata-mode-hook))

(defalias 'stata-open-or-switch-to-repl
  (lambda () (interactive)
    (dev-python--enable-venv)
    (dev-jupyter-open-or-switch-to-repl "stata"))
  "Open a jupyter repl for stata interpreter.")

(defalias 'stata-send-line-or-region
  (lambda () (interactive)
    (dev-jupyter-send-line-or-region "stata"))
  "Send current line or visually selected region to stata interpreter.")

(defalias 'stata-send-buffer-or-region
  (lambda () (interactive)
    (dev-jupyter-send-buffer-or-region "stata"))
  "Send buffer or visually selected region to stata interpreter.")

(defalias 'stata-open-or-switch-to-remote-repl
   (lambda () (interactive)
     (dev-jupyter-open-remote-repl "stata"))
   "Open a stata interpreter in the remote location")

(defalias 'stata-attach-buffer
  (lambda () (interactive)
    (dev-jupyter--attach-buffer "stata"))
  "Attach current code buffer to an existing stata REPL")


(provide 'stata)
;;; stata.el ends here
