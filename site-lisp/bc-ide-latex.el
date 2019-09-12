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
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :mode ("\\.pdf\\'" . pdf-tools-install)

  :general
  (:keymaps 'pdf-view-mode-map
   :states '(motion normal visual)
   "j" 'pdf-view-scroll-up-or-next-page
   "k" 'pdf-view-scroll-down-or-previous-page
   "J" 'pdf-view-next-page
   "K" 'pdf-view-previous-page
   "gg" 'pdf-view-first-page
   "G" 'pdf-view-last-page
   "/" 'isearch-forward
   "?" 'isearch-backward
   "n" 'isearch-repeat-forward
   "N" 'isearch-repeat-backward
   "o" 'pdf-outline
   "+" 'pdf-view-enlarge
   "-" 'pdf-view-shrink
   "SPC" nil)

  (:keymaps 'pdf-view-mode-map
   :states '(motion normal visual)
   :prefix "SPC"
   "q" 'kill-buffer-and-window))

;; (use-package reftex
;;   :after 'auctex
;;   :defer t
;;   :hook (LaTeX-mode . reftex-mode)
;;   :config
;;   (setq reftex-cite-prompt-optional-args t)

;;   :general
;;   (:keymaps 'reftex-mode-map
;;    :states '(motion normal visual)
;;    :prefix "SPC"
;;    "rl" 'reftex-label
;;    "ri" 'reftex-reference)
;;   (:keymaps 'reftex-mode-map
;;    :states 'insert
;;    :prefix "C-c"
;;    "l" 'reftex-label
;;    "i" 'reftex-reference))

(use-package ivy-bibtex
  :after auctex
  :defer t
  :hook
  (LaTeX-mode . bc-latex-load-bib-file)
  :init
  (defun bc-latex-load-bib-file ()
    "Add bibtex file to `ivy-bibtex' library for the current .tex file.

It search the current directory for a bib file named \"example-bib.bib\". If such file exists, it will automatically add it to `bibtex-completion-bibliography'. If such file does not exists, it prompt to ask you whether you want to choose the bib file manually."
    (let* ((bib-file
            (concat
             (expand-file-name (file-name-base (buffer-file-name)))
             "-bib.bib")))
      (if (file-exists-p bib-file)
          (setq-local bibtex-completion-bibliography bib-file)
        (if (y-or-n-p "Cannot find bib file, feed one? ")
            (ivy-read "bib file:"
                      #'read-file-name-internal
                      :action
                      (lambda (x)
                        (setq-local bibtex-completion-bibliography x)))))))

  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation
        bibtex-completion-cite-commands '("citep" "citet" "citep*" "citet*")
        bibtex-completion-cite-default-command "citep"
        bibtex-completion-cite-prompt-for-optional-arguments nil)
  :general
  (:keymaps 'LaTeX-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
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
