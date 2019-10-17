;;; bc-ide-latex.el --- ide for latex editing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package auctex
  :defer t

  :custom-face
  (font-latex-italic-face ((t (:underline nil :inherit 'italic))))
  (font-latex-slide-title-face ((t (:height 1.0 :inherit 'font-lock-function-name-face))))

  :init
  (setq
   ;; use zathura to view pdf
   TeX-view-program-selection '((output-pdf "PDF Tools"))
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
   TeX-electric-sub-and-superscript t

   ;; other settings
   TeX-parse-self t
   TeX-auto-save t)

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
   :states '(normal visual motion)
   :prefix "SPC"
   "rr" 'TeX-command-run-all))

;; use eaf instead?
(use-package pdf-tools
  :defer t
  :config
  (setq pdf-view-display-size 'fit-page)
  (evil-set-initial-state 'pdf-view-mode 'motion)

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
      (pdf-sync-backward-search-mouse nil)
      (recenter nil)))

  ;; the following 4 functions are taken from evil-collection
  ;; see https://github.com/emacs-evil/evil-collection/blob/master/evil-collection-pdf.el
  (defun bc-ide-latex-pdf-view-next-line-or-next-page (&optional count)
    "'evil' wrapper include a count argument to `pdf-view-next-line-or-next-page'"
    (interactive "P")
    (if count
        (dotimes (_ count nil)
	        (pdf-view-next-line-or-next-page 1))
      (pdf-view-next-line-or-next-page 1)))

  (defun bc-ide-latex-pdf-view-previous-line-or-previous-page (&optional count)
    "'evil' wrapper include a count argument to `pdf-view-previous-line-or-previous-page'"
    (interactive "P")
    (if count
        (dotimes (_ count nil)
	        (pdf-view-previous-line-or-previous-page 1))
      (pdf-view-previous-line-or-previous-page 1)))

  (defun bc-ide-latex-pdf-view-goto-page (&optional page)
    "`evil' wrapper around `pdf-view-last-page'."
    (interactive "P")
    (if page
        (pdf-view-goto-page page)
      (pdf-view-last-page)
      (image-eob)))

  (defun bc-ide-latex-pdf-view-goto-first-page (&optional page)
    "`evil' wrapper around `pdf-view-first-page'."
    (interactive "P")
    (if page
        (pdf-view-goto-page page)
      (pdf-view-first-page)
      (image-bob)))

  (defun bc-ide-latex-pdf-view-in-zathura ()
    "View the current file in zathura."
    (interactive)
    (let ((file buffer-file-name)
          (page (pdf-view-current-page)))
      (bc-exwm-launch (concat "zathura " file " -P " (format "%s" page)))))

  ;; :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :mode ("\\.pdf\\'" . pdf-tools-install)

  :general
  (:keymaps 'pdf-view-mode-map
   :states 'motion
   "j" 'bc-ide-latex-pdf-view-next-line-or-next-page
   "k" 'bc-ide-latex-pdf-view-previous-line-or-previous-page
   "h" 'image-backward-hscroll
   "l" 'image-forward-hscroll

   "H" 'image-bol
   "L" 'image-eol
   "J" 'pdf-view-next-page
   "K" 'pdf-view-previous-page

   "C-d" (lambda () (interactive)
           (pdf-view-scroll-up-or-next-page 20))
   "C-u" (lambda () (interactive)
           (pdf-view-scroll-down-or-previous-page 20))

   "gg" 'bc-ide-latex-pdf-view-goto-first-page
   "gl" 'pdf-view-goto-label
   "G" 'bc-ide-latex-pdf-view-goto-page

   (kbd "<C-down-mouse-1>") 'pdf-view-mouse-extend-region
   (kbd "<M-down-mouse-1>") 'pdf-view-mouse-set-region-rectangle
   (kbd "<down-mouse-1>")  'pdf-view-mouse-set-region

   (kbd "<C-return>") 'bc-ide-latex-inverse-search

   "/" 'isearch-forward
   "?" 'isearch-backward
   "n" 'isearch-repeat-forward
   "N" 'isearch-repeat-backward

   "o" 'pdf-outline
   "+" 'pdf-view-enlarge
   "-" 'pdf-view-shrink
   "=" 'pdf-view-fit-page-to-window
   "SPC" nil)

  (:keymaps 'pdf-view-mode-map
   :states 'visual
   "SPC" nil
   "y" 'pdf-view-kill-ring-save)

  (:keymaps 'pdf-view-mode-map
   :states '(motion visual)
   :prefix "SPC"
   "q" 'kill-buffer-and-window
   "z" 'bc-ide-latex-pdf-view-in-zathura)

  (:keymaps 'pdf-outline-mode
   :states 'normal
   "<return>" 'pdf-outline-follow-link))

;; TODO: ivy-integrate this
(use-package reftex
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

(provide 'bc-ide-latex)
;;; bc-ide-latex.el ends here
