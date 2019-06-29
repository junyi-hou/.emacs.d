;;; bc-ide-latex.el --- ide for latex editing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-company)

(use-package tex-site
  :ensure auctex
  :defer t
  :config
  (setq
  ;; use emacs to view pdf
   TeX-view-program-selection '((output-pdf "PDF Tools"))
   TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
   TeX-source-correlate-start-server t

   ;; disable moving sub/super scripts
   tex-fontify-script nil
   font-latex-fontify-script nil
   font-latex-fontify-sectioning 1.0

   ;; latex command - enable syntax and shell-escape
   LaTeX-command "latex -syntax=1 --shell-escape -interaction=nonstopmode"

   ;; auto-close
   LaTeX-electric-left-right-brace t

   ;; other settings
   TeX-parse-self t
   TeX-auto-save t)

  (face-spec-set
   'font-latex-slide-title-face
   '((t :inherit  'font-lock-function-name-face
        :height 1.0
        :weight normal)))

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (add-hook 'LaTeX-mode-hook #'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook #'company-mode)
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'bc-latex-ivy-bibtex-load-bib-file)

  :general
  (:keymaps 'LaTeX-mode-map
   :states '(normal visual motion)
   "SPC" nil)

  (:keymaps 'LaTeX-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "rr" 'TeX-command-run-all)
  )

(use-package reftex
  :after 'tex-site
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
  :after tex-site
  :defer t
  :config
  (setq ivy-re-builders-alist '((ivy-bibtex . ivy--regex-ignore-order)
                                (t . ivy--regex-plus))
        ivy-bibtex-default-action 'ivy-bibtex-insert-citation
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

(use-package pdf-tools
  :config
  (setq pdf-view-display-size 'fit-page)
  (evil-set-initial-state 'pdf-view-mode 'motion)
  (add-hook 'pdf-view-mode-hook (lambda () (auto-revert-mode -1)))
  :defer t
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

(use-package company-auctex
  :after tex-site
  :hook
  (LaTeX-mode . company-auctex-init))

(defun bc-latex-ivy-bibtex-load-bib-file ()
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


(provide 'bc-ide-latex)
;;; bc-ide-latex.el ends here
