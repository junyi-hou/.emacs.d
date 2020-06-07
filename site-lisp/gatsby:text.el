;;; gatsby:text.el --- text editing modes -*- lexical-binding: t; -*-

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

(use-package man
  :defer t
  :general
  (:keymaps 'Man-mode-map
   :states 'motion
   "zc" 'outline-hide-entry
   "zo" 'outline-show-entry))

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

        ;; latex command - enable synctax and shell-escape
        LaTeX-command "latex -synctax=1 --shell-escape"

        ;; auto-close
        LaTeX-electric-left-right-brace t
        TeX-electric-math (cons "$" "$")
        LaTeX-syntactic-comments nil

        ;; other settings
        TeX-parse-self t
        TeX-auto-save t

        ;; allow folding beamer slides
        TeX-outline-extra '(("^\\(% \\)?\\\\begin{frame}" 2)))

  ;; External pdf-viewer and exwm integration
  (when (and (featurep 'exwm)
             (not (eq (cadr (assq 'output-pdf TeX-view-program-selection))
                      "PDF Tools")))

    (defun gatsby:ide-latex-compile (compile-fn &rest args)
      "Open a new buffer to display complied pdf."
      (let* ((pdf-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))
             (buffer-list (buffer-list)))
        (cond ((member pdf-file
                       (mapcar (lambda (wd) (buffer-name (window-buffer wd)))
                               (apply #'append (mapcar #'window-list (frame-list)))))
               ;; pdf-file exists and visible
               (apply compile-fn args))
              ((member pdf-file (mapcar 'buffer-name buffer-list))
               ;; pdf-file exists but not visible
               (apply compile-fn args)
               (pop-to-buffer (get-buffer pdf-file)))
              (t ;; pdf-file has not been opened
               (gatsby:core-split-window)
               (other-window 1)
               (apply compile-fn args)))))

    (defun gatsby:ide-latex-inverse-search ()
      "Inverse search the center of the window.  If in Zathura, send key <return>, but make sure to recenter mouse first.  Otherwise just send <return> without moving mouse."
      (interactive)
      ;; if in zathura window, set the mouse position
      ;; otherwise do not need to
      (when (equal exwm-class-name "Zathura")
        (let* ((corner (window-absolute-pixel-edges))
               (left (car corner))
               (top (cadr corner))
               (right (caddr corner))
               (bottom (cadddr corner))
               (x (/ (+ left right) 2))
               (y (/ (+ top bottom) 2)))
          (set-mouse-absolute-pixel-position x y)))
      ;; get keycode using (read-char "x")
      (exwm-input--fake-key 96))

    (advice-add #'TeX-command-run-all :around #'gatsby:ide-latex-compile)

    (general-define-key
     :keymaps 'exwm-mode-map
     "`" 'gatsby:ide-latex-inverse-search))

  ;; use xelatex for chinese support
  ;; (setq-default TeX-engine 'xetex)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (defun gatsby:ide-latex--set-tab-width ()
    "Set tab width in latex buffer."
    (setq-local tab-width 2))

  (defun gatsby:ide-latex--fix-indent ()
    "Fix indentation for the current buffer."
    ;; fix indent for the whole buffer before save
    (add-hook
     'before-save-hook
     (defun indent-buffer ()
       "Indent the current buffer"
       (indent-region 0 (point-max)))
     nil t))

  ;; home-made electric-mode
  (defun gatsby:ide-latex-electric-single-quote ()
    "Automatically close single quotes"
    (interactive)
    (insert "`'")
    (backward-char 1))

  (defun gatsby:ide-latex-electric-double-quote (&rest _)
    "Automatically close single quotes"
    (insert "''")
    (backward-char 2))

  (advice-add #'TeX-insert-quote :after #'gatsby:ide-latex-electric-double-quote)

  (defconst gatsby:ide-latex-pair-macro-regexp
    "\\(.\\{1\\}\\(right\\|[bB]ig\\(g\\)?\\)\\)?")

  (defconst gatsby:ide-latex-pair-regexp
    "\\(.?}\\|)\\|\\]\\|'\\)")

  (defconst gatsby:ide-latex-pairs
    '(("\\}" . "\\\\{")
      ("}" . "{")
      (")" . "(")
      ("]" . "\\[")
      ("'" . "`")
      ("$" . "$")))

  (defun gatsby:ide-latex-electric-delete ()
    "If point is inside an empty pair, delete the whole pair.  Otherwise call `backward-delete-char'."
    (interactive)
    (if (looking-at
         (format "%s%s" gatsby:ide-latex-pair-macro-regexp gatsby:ide-latex-pair-regexp))
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
                                            gatsby:ide-latex-pairs
                                            nil nil 'string=))
                         (- (point) 10)
                         'noerror))))
          (if empty?
              (delete-region (match-beginning 0) pair-end)
            (call-interactively #'backward-delete-char-untabify)))
      (call-interactively #'backward-delete-char-untabify)))

  ;; add hideshow mode support
  ;; WONT FIX: replace hideshow with other code-folding mode based on tree-sitter
  ;; right now just set `hs-block-start-regexp' to work with `gatsby:selectrum-jump-to-hs-header'.
  (defconst gatsby:text-latex-env-should-hide
    '("frame" "lemma" "proposition" "proof")
    "A list of latex env that should be folded.")

  (defun gatsby:latex--setup-hs-block-start ()
    (let ((env-list (append '(or) gatsby:text-latex-env-should-hide)))
      (setq hs-block-start-regexp
            (eval
             `(rx line-start (* blank)
                  (or (group "\\begin{" ,env-list)
                      (group "\\" (or (group (* "sub") "section")
                                     "appendix"
                                     "bibliography"))))))))

  :hook
  (LaTeX-mode . TeX-PDF-mode)
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . gatsby:latex--setup-hs-block-start)
  (LaTeX-mode . gatsby:ide-latex--set-tab-width)
  (LaTeX-mode . gatsby:ide-latex--fix-indent)

  :general
  (:keymaps 'LaTeX-mode-map
   :states '(normal visual motion)
   "SPC" nil)

  (:keymaps 'LaTeX-mode-map
   :states 'insert
   "<return>" 'newline
   "<backspace>" 'gatsby:ide-latex-electric-delete
   "`" 'gatsby:ide-latex-electric-single-quote)

  (:keymaps 'LaTeX-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "rr" 'TeX-command-run-all))

(use-package company-auctex
  :after auctex
  :hook
  (LaTeX-mode . company-auctex-init))

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


(provide 'gatsby:text)
;;; gatsby:text.el ends here
