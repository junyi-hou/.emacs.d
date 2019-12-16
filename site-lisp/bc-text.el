;;; bc-text.el --- text editing modes -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package markdown-mode
  :defer t
  :mode ("INSTALL\\'"
         "CONTRIBUTORS\\'"
         "LICENSE\\'"
         "README\\'"
         "\\.markdown\\'"
         "\\.md\\'"))

(use-package auctex
  :defer t

  :custom-face
  (font-latex-italic-face ((t (:underline nil :inherit 'italic))))
  (font-latex-slide-title-face ((t (:height 1.0 :inherit 'font-lock-function-name-face))))

  :init
  (setq TeX-view-program-selection '((output-pdf "Zathura"))
        TeX-source-correlate-start-server t

        ;; do not mess up my indentation
        LaTeX-item-indent 0
        ;; disable moving sub/super scripts
        tex-fontify-script nil
        font-latex-fontify-script nil
        font-latex-fontify-sectioning 1.0

        ;; latex command - enable syntax and shell-escape
        LaTeX-command "latex -syntax=1 --shell-escape"

        ;; auto-close
        LaTeX-electric-left-right-brace t
        TeX-electric-math (cons "$" "$")
        LaTeX-syntactic-comments nil

        ;; other settings
        TeX-parse-self t
        TeX-auto-save t

        ;; allow folding beamer slides
        TeX-outline-extra '(("^\\\\begin{frame}" 2)))

  ;; External pdf-viewer and exwm integration
  (when (and (featurep 'exwm)
             (not (eq (cadr (assq 'output-pdf TeX-view-program-selection))
                      "PDF Tools")))
    (defun bc-ide-latex-compile (&rest _)
      "Open a new buffer to display complied pdf."
      (unless (member (concat (file-name-sans-extension (buffer-file-name))
                              ".pdf")
                      (mapcar (lambda (wd)
                                (buffer-name (window-buffer wd)))
                              (window-list)))
        (bc-core-split-window)
        (other-window 1)))

    (defun bc-ide-latex-inverse-search ()
      "Inverse search the center of the window."
      (interactive)
      (let* ((corner (window-absolute-pixel-edges))
             (left (car corner))
             (top (cadr corner))
             (right (caddr corner))
             (bottom (cadddr corner))
             (x (/ (+ left right) 2))
             (y (/ (+ top bottom) 2)))
        (ignore-errors (evil-previous-line))
        (set-mouse-absolute-pixel-position x y)
        ;; TODO: get C-left-click's event number
        (exwm-input--fake-key 'C-mouse-1)
        (recenter nil)))

    (advice-add #'TeX-command-run-all :before #'bc-ide-latex-compile)

    (general-define-key
     :keymaps 'exwm-mode-map
     "<C-return>" 'bc-ide-latex-inverse-search))

  ;; use xelatex for chinese support
  ;; (setq-default TeX-engine 'xetex)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (defun bc-ide-latex--set-tab-width ()
    "Set tab width in latex buffer."
    (setq-local tab-width 2))

  (defun bc-ide-latex--fix-indent ()
    "Fix indentation for the current buffer."
    ;; fix indent for the whole buffer before save
    (add-hook
     'before-save-hook
     (defun indent-buffer ()
       "Indent the current buffer"
       (indent-region 0 (point-max)))
     nil t))

  ;; home-made electric-mode
  (defun bc-ide-latex-electric-single-quote ()
    "Automatically close single quotes"
    (interactive)
    (insert "`'")
    (backward-char 1))

  (defun bc-ide-latex-electric-double-quote (&rest _)
    "Automatically close single quotes"
    (insert "''")
    (backward-char 2))

  (advice-add #'TeX-insert-quote :after #'bc-ide-latex-electric-double-quote)

  (defconst bc-ide-latex-pair-macro-regexp
    "\\(.\\{1\\}\\(right\\|[bB]ig\\(g\\)?\\)\\)?")

  (defconst bc-ide-latex-pair-regexp
    "\\(.?}\\|)\\|\\]\\|'\\)")

  (defconst bc-ide-latex-pairs
    '(("\\}" . "\\\\{")
      ("}" . "{")
      (")" . "(")
      ("]" . "\\[")
      ("'" . "`")
      ("$" . "$")))

  (defun bc-ide-latex-electric-delete ()
    "If point is inside an empty pair, delete the whole pair.  Otherwise call `backward-delete-char'."
    (interactive)
    (if (looking-at
         (format "%s%s" bc-ide-latex-pair-macro-regexp bc-ide-latex-pair-regexp))
        ;; now the point is just before the closing pairs
        (let ((pair-end (match-end 0))
              (empty? (save-excursion
                        (re-search-backward
                         (format "%s%s\\="
                                 (if (match-string-no-properties 1)
                                     (replace-regexp-in-string
                                      "right" "left"
                                      (format "\\%s" (match-string-no-properties 1)))
                                   "")
                                 (alist-get (match-string-no-properties 4)
                                            bc-ide-latex-pairs
                                            nil nil 'string=))
                         (- (point) 10)
                         'noerror))))
          (if empty?
              (delete-region (match-beginning 0) pair-end)
            (call-interactively #'backward-delete-char-untabify)))
      (call-interactively #'backward-delete-char-untabify)))

  :hook
  (LaTeX-mode . TeX-PDF-mode)
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . bc-ide-latex--set-tab-width)
  (LaTeX-mode . bc-ide-latex--fix-indent)

  :general
  (:keymaps 'LaTeX-mode-map
   :states '(normal visual motion)
   "SPC" nil)

  (:keymaps 'LaTeX-mode-map
   :states 'insert
   "<return>" 'newline
   "<backspace>" 'bc-ide-latex-electric-delete
   "`" 'bc-ide-latex-electric-single-quote)

  (:keymaps 'LaTeX-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "rr" 'TeX-command-run-all))

(use-package man
  :defer t
  :general
  (:keymaps 'Man-mode-map
   :states 'motion
   "zc" 'outline-hide-entry
   "zo" 'outline-show-entry))

(use-package outline
  ;; `hideshow' does not play well with some modes,
  ;; use outline-minor-mode in those modes instead.
  :hook
  (LaTeX-mode . outline-hide-body)
  (LaTeX-mode . outline-minor-mode)
  (Man-mode . outline-hide-body)
  (Man-mode . outline-minor-mode)
  :init
  (setq outline-blank-line t))

(use-package reftex
  ;; TODO: ivy-integrate this
  :after 'auctex
  :defer t
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setq reftex-cite-prompt-optional-args t)

  :general
  (:keymaps 'reftex-mode-map
   :states '(motion normal visual)
   :prefix "SPC"
   "rl" 'reftex-label
   "ri" 'reftex-reference)

  (:keymaps 'reftex-mode-map
   :states 'insert
   :prefix "C-c"
   "l" 'reftex-label
   "i" 'reftex-reference))

(use-package ivy-bibtex
  :after auctex
  :defer t
  :init
  (defun bc-ide-latex--get-project-bib-file (proj_root)
    "Scan PROJ_ROOT/reference directory and return a list of .bib files.  If PROJ_ROOT is not given, use the current project root returned by `project-current'."
    (let ((proj_root (or proj_root (cdr (project-current)))))
      (directory-files (concat proj_root "reference") nil ".*\\.bib")))

  (defun bc-ide-latex-load-bib-file ()
    "Add bibtex file to `ivy-bibtex' library for the current .tex file.  This function scans both the directory of the current .tex file and the PROJ_ROOT/reference/ directory for .bib file."
    (let* ((proj? (cdr (project-current)))
           (local-bib (concat
                      (expand-file-name (file-name-base (buffer-file-name))) ".bib")))
      (make-local-variable bibtex-completion-bibliography)
      (when (file-exists-p local-bib)
        (add-to-list 'bibtex-completion-bibliography local-bib))
      (when proj?
        (append
         'bibtex-completion-bibliography
         (bc-ide-latex--get-project-bib-file proj?)))))

  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation
        bibtex-completion-cite-commands '("citep" "citet" "citep*" "citet*")
        bibtex-completion-cite-default-command "citep"
        bibtex-completion-cite-prompt-for-optional-arguments nil)
  :general
  (:keymaps 'LaTeX-mode-map
   :states '(normal visual motion insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "rc" 'ivy-bibtex)

  (:keymaps 'LaTeX-mode-map
   :states 'insert
   :prefix "C-c"
   "c" 'ivy-bibtex))

(use-package company-auctex
  :after auctex
  :hook
  (LaTeX-mode . company-auctex-init))

(provide 'bc-text)
;;; bc-text.el ends here
