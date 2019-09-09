;;; bc-org.el --- my org-mode config

;;; Commentary:

;;; Code:

(use-package org
  :demand t
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

  (defun bc-org--export-headline ()
    "Return the first level headline of an org file."
    (set-mark (point-min))
    (goto-char (point-max))
    (org-map-entries
     (lambda ()
       (let ((heading (org-heading-components)))
         ;; return heading text if there is any, otherwise return the todo keyword
         (if (nth 4 heading)
             (nth 4 heading)
           (nth 2 heading))))
     nil
     'region-start-level))

  (defun bc-org--capture (filename &optional headline)
    "Create a temporary org capture entry pointing to FILENAME under HEADLINE, capture the current content there and then"
    (let ((headline (or headline
                        (ivy-read
                         "Headline: "
                         (org-babel-with-temp-filebuffer filename
                           (bc-org--export-headline))
                         :action 'identity)))
          (templates org-capture-templates))
      (add-to-list
       'org-capture-templates
       `("t" "customized capture item" entry (file+headline ,filename ,headline)
         "* %t %?\n%a" :empty-lines 1))
      (org-capture nil "t")
      (evil-insert-state)
      (setq org-capture-templates templates)))

  (defun bc-org-capture-todo ()
    "Capture to ~/org/todo.org."
    (interactive)
    (bc-org--capture "~/org/todo.org"))

  (defun bc-org-capture-project-notes ()
    "Capture to current-project-root/notes.org."
    (interactive)
    (let* ((project-root (cdr (project-current)))
           (notes (concat project-root "notes.org")))
      (unless project-root
        (user-error "No project found"))
      (unless (file-exists-p notes)
        (if (y-or-n-p "File notes.org does not exists, create? ")
            (with-temp-buffer
              (write-file notes))
          (user-error "notes.org does not exists, abort")))
      (bc-org--capture notes)))

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
        '(("m" "mail item" entry (file+headline "~/org/todo.org" "MAIL")
           "* %t %?\n%a" :empty-lines 1)))
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

;; (use-package calfw
;;   :config
;;   (setq calendar-month-name-array
;;         '("January" "February" "March"     "April"   "May"      "June"
;;           "July"    "August"   "September" "October" "November" "December")
;;         calendar-day-name-array
;;         '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
;;           "Saturday")
;;         calendar-week-start-day 1)

;;   (setq cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap)

;;   ;; grid frame
;;   (setq cfw:fchar-junction ?╋
;;         cfw:fchar-vertical-line ?┃
;;         cfw:fchar-horizontal-line ?━
;;         cfw:fchar-left-junction ?┣
;;         cfw:fchar-right-junction ?┫
;;         cfw:fchar-top-junction ?┯
;;         cfw:fchar-top-left-corner ?┏
;;         cfw:fchar-top-right-corner ?┓))

;; (use-package calfw-org
;;   :after org)

(provide 'bc-org)
;;; bc-org.el ends here
