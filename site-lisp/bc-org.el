;;; bc-org.el --- my org-mode config

;;; Commentary:

;;; Code:

(use-package org
  :defer t
  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda () (setq tab-width 2)))
  (org-mode . bc-org--complete-keywords)

  :init
  ;; functions

  ;; taken from https://github.com/dakra/dmacs/blob/master/init.org#org
  (defun bc-org-remove-all-results ()
    "Remove results from every code block in buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (org-babel-remove-result))))

  (defun bc-org--complete-keywords ()
    "Allow company to complete org keywords after ^#+"
    (add-hook
     'completion-at-point-functions
     'pcomplete-completions-at-point nil t))

  (defun bc-org-insert-date ()
    "Insert today's date in org-mode's format."
    (insert (concat "<" (format-time-string "%Y-%m-%d") ">")))

  (defun bc-org-insert-date-time ()
    "Insert today's date-time in org-mode's format."
    (insert (concat "<" (format-time-string "%Y-%m-%d %H:%M") ">")))

  :config

  ;; general config
  (setq org-cycle-emulate-tab nil
        org-export-with-toc nil
        org-highlight-latex-and-related '(latex entities script))

  ;; babel
  ;; load interpreters
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (shell . t)
     (python . t)
     (jupyter . t)))

  ;; src block settings
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)

  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images t)

  ;; export to latex and other formats
  (setq org-latex-packages-alist '(("" "setspace")
                                   ;; https://github.com/gpoore/minted/issues/92
                                   ("cache=false" "minted")
                                   ("" "pdflscape")
                                   ("" "multirow")
                                   ("" "multicol")
                                   ("" "booktabs")
                                   ("" "amsthm")
                                   ("" "amssymb")
                                   ("" "listingsutf8")
                                   ("top=1in, bottom=1in, left=1in, right=1in" "geometry")
                                   ("" "natbib")))
  (setq
   org-latex-listings 'minted
   org-latex-pdf-process
   '("pdflatex -shell-escape -output-directory %o %f"
     "biber %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; org capture/agenda
  (setq org-directory "~/org"
        org-default-notes-file (concat org-directory "/notes.org"))

  (setq org-capture-templates
        '(("t" "todo item" entry (file "~/org/todo.org")
            "* TODO %t %?\n  %F" :empty-lines 1)
          ("r" "research notes" entry
           (file (concat (cdr (project-current)) "note.org"))
           "* NOTE %?\n  %t\n  %F")))
  :general
  (:keymaps 'org-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "re" 'org-export-dispatch
   "ro" 'org-edit-special
   "rr" 'org-ctrl-c-ctrl-c
   "rf" 'org-footnote
   "rc" 'bc-org-remove-all-results)

  (:keymaps 'org-mode-map
   :states '(normal visual motion insert)
   "C-e" 'org-next-visible-heading
   "C-y" 'org-previous-visible-heading
   "<up>" 'org-previous-visible-heading
   "<down>" 'org-next-visible-heading))


(provide 'bc-org)
;;; bc-org.el ends here
