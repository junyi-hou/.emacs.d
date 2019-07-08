;;; bc-org.el --- my org-mode config

;;; Commentary:

;;; Code:

(use-package org

  :init

  ;; functions

  (defun bc-org--complete-keywords ()
    "Allow company to complete org keywords after ^#+"
    (add-hook
     'completion-at-point-functions
     'pcomplete-completions-at-point nil t))

  (defun bc-org-insert-date ()
    "Insert today's date in org-mode's format."
    (insert (concat "<"
                    (shell-command-to-string "echo -n $(date +%Y-%m-%d)")
                    ">")))

  (defun bc-org-insert-date-time ()
    "Insert today's date-time in org-mode's format."
    (insert (concat "<"
                    (shell-command-to-string "echo -n $(date +%Y-%m-%d)")
                    " "
                    (shell-command-to-string "echo -n $(date +%H:%M)")
                    ">")))

  :config

  (use-package company-auctex
    :init
    (defun bc-org-company-add-latex-backends ()
      (setq-local company-backends
                  '((company-auctex-labels
                     company-auctex-bibs
                     company-auctex-environments
                     company-auctex-macros
                     company-auctex-symbols
                     company-capf
                     company-yasnippet
                     company-files)
                    (company-dabbrev
                     company-abbrev))))
    :hook
    (org-mode . bc-org-company-add-latex-backends))

  ;; general config
  (setq org-cycle-emulate-tab nil
        org-export-with-toc nil)

  (add-hook 'org-mode-hook #'company-mode)
  (add-hook 'org-mode-hook (lambda () (setq tab-width 2)))
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'bc-org--complete-keywords)

  ;; src block settings
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)

  (unless bc-venv-current-venv
    (bc-venv--enable-venv)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (latex . t)
       (shell . t)
       (python . t)
       (jupyter . t))))

  ;; (advice-add 'org-edit-src-code :after (lambda ()
  ;;                                         (ignore-errors (eglot-ensure))))

  ;; export to latex
  (setq org-latex-packages-alist '(("" "setspace")
                                   ("" "pdflscape")
                                   ("" "multirow")
                                   ("" "multicol")
                                   ("" "booktabs")
                                   ("" "amsthm")
                                   ("" "amssymb")
                                   ("" "listingsutf8")
                                   ("" "minted")
                                   ("top=1in, bottom=1in, left=1in, right=1in" "geometry")
                                   ("" "natbib")))
  (setq
   org-latex-listings 'minted
   org-latex-pdf-process
   '("pdflatex -shell-escape -output-directory %o %f"
     "biber %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
   )
  
  ;; org-capture

  ;; (add-hook 'org-capture-mode-hook #'evil-insert-state)
  ;; (setq org-capture-templates
  ;;       `(("t"
  ;;          "A todo item with deadline"
  ;;          entry
  ;;          (file "todo.org"),
  ;;          (concat "* TODO %?\n"
  ;;                  "CREATED:  %t\n")
  ;;          :empty-lines 1)
  ;;         )
  ;;       )

  :general
  (:keymaps 'org-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "re" 'org-export-dispatch
   "ro" 'org-edit-special
   "rr" 'org-ctrl-c-ctrl-c
   "rf" 'org-footnote)

  (:keymaps 'org-mode-map
   :states '(normal visual motion)
   "<up>" 'org-previous-visible-heading
   "<down>" 'org-next-visible-heading)

  (:keymaps 'org-mode-map
   :states '(insert normal visual motion)
   "C-d" 'org-next-visible-heading
   "C-u" 'org-previous-visible-heading))


(provide 'bc-org)
;;; bc-org.el ends here
