;;; gatsby:org.el --- my org-mode config

;;; Commentary:

;;; Code:
(require 'gatsby:core)

(use-package org
  :defer t
  :hook
  (org-mode . org-indent-mode)
  (org-mode . gatsby:org--fix-indent)
  (org-mode . gatsby:org--complete-keywords)

  :init
  (defconst gatsby:org-foldable
    '(example-block export-block src-block table)
    "A list of `org-element' that are consider foldable, and hence can be folded/expanded by `gatsby:org-hide-block' and `gatsby:org-show-block'.")
  
  (defun gatsby:org-hide-block ()
    "Hide current block, if it is inside element defined in `gatsby:org-foldable', first try to fold the element.  Fall back to `evil-close-fold'."
    (interactive)
    (let ((element (org-element-at-point)))
      (if (memq (car element) gatsby:org-foldable)
          (progn
            (goto-char (plist-get (cadr element) :begin))
            (org-hide-block-toggle t))
        (evil-close-fold))))

  (defun gatsby:org-show-block ()
    "Show current block."
    (interactive)
    (condition-case _
        (org-hide-block-toggle 'off)
      (error (evil-open-fold))))

  ;; taken from https://github.com/dakra/dmacs/blob/master/init.org#org
  (defun gatsby:org-remove-all-results ()
    "Remove results from every code block in buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (org-babel-remove-result))))

  ;;; ===============================
  ;;  hooks
  ;;; ===============================
  
  (defun gatsby:org--fix-indent ()
    (setq tab-width 2))

  (defun gatsby:org--complete-keywords ()
    "Allow company to complete org keywords after ^#+"
    (add-hook
     'completion-at-point-functions
     'pcomplete-completions-at-point nil t))

  ;; general config
  (setq org-cycle-emulate-tab nil
        org-export-with-toc nil
        org-use-sub-superscripts t
        org-hide-leading-stars nil
        org-indent-mode-turns-on-hiding-stars nil
        org-highlight-latex-and-related '(latex entities script)
        org-startup-with-latex-preview t)

  ;; babel
  ;; load interpreters
  (require 'gatsby:jupyter)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (shell . t)
     (python . t)
     (scheme . t)
     (jupyter . t)))

  ;; src block settings
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window
        org-src-ask-before-returning-to-edit-buffer nil)

  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images t)

  ;; export to latex and other formats
  (setq org-export-backends '(ascii html latex md)
        org-export-coding-system 'utf-8)

  (setq org-latex-packages-alist
        '(("" "setspace")
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

  (setq org-latex-listings 'minted
        org-latex-pdf-process
        '("pdflatex -shell-escape -output-directory %o %f"
          "biber %b"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  :general
  (:keymaps 'org-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "re" 'org-export-dispatch
   "ro" 'org-edit-special
   "rr" 'org-ctrl-c-ctrl-c
   "rf" 'org-footnote
   "rc" 'gatsby:org-remove-all-results)

  (:keymaps 'org-mode-map
   :states '(normal visual motion emacs insert)
   :prefix "C-c"
   "C-'" 'org-edit-special)

  (:keymaps 'org-mode-map
   :states '(normal visual motion)
   "zo" 'gatsby:org-show-block
   "zc" 'gatsby:org-hide-block)

  (:keymaps 'org-src-mode-map
   :states '(normal visual motion insert emacs)
   "C-c" nil)

  (:keymaps 'org-src-mode-map
   :states '(normal visual motion insert emacs)
   :prefix "C-c"
   "C-'" 'org-edit-src-exit)

  (:keymaps 'org-mode-map
   :states '(normal visual motion insert)
   "<up>" 'org-previous-visible-heading
   "<down>" 'org-next-visible-heading))

(use-package org-capture
  :straight (:type built-in)
  :commands (gatsby:org-capture-todo gatsby:org-capture-project-notes)
  :init
  (defun gatsby:org--export-headline ()
    "Return the first level headline of an org file."
    (set-mark (point-min))
    (goto-char (point-max))
    (prog1
        (org-map-entries
         (lambda ()
           (let ((heading (org-heading-components)))
             ;; return heading text if there is any, otherwise return the todo keyword
             (if (nth 4 heading)
                 (nth 4 heading)
               (nth 2 heading))))
         nil
         'region-start-level)
      ;; kill active region, and reset hs-mode
      (deactivate-mark)
      (hs-hide-all)
      (goto-char 1)))

  (defun gatsby:org--capture (filename &optional headline)
    "Create a temporary org capture entry pointing to FILENAME under HEADLINE, capture the current content there and then"
    (require 'org-capture)
    (let ((headline (or headline
                        (completing-read "Headline: "
                                         (org-babel-with-temp-filebuffer filename
                                           (gatsby:org--export-headline)))))
          (saved-templates org-capture-templates))
      (add-to-list
       'org-capture-templates
       `("t" "customized capture item" entry (file+headline ,filename ,headline)
         "* %t %?\n%a" :empty-lines 1))
      (org-capture nil "t")
      (evil-insert-state)
      (setq org-capture-templates saved-templates)))

  (defun gatsby:org-capture-todo ()
    "Capture to ~/org/todo.org."
    (interactive)
    (gatsby:org--capture "~/org/todo.org"))

  (defun gatsby:org-capture-project-notes ()
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
      (gatsby:org--capture notes)))

  ;; org capture/agenda
  (setq org-directory "~/org"
        org-default-notes-file (concat org-directory "/notes.org"))

  :general
  (:keymaps '(normal visual motion insert emacs)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "ct" 'gatsby:org-capture-todo
   "cr" 'gatsby:org-capture-project-notes))

(use-package ob-async :after 'org)

(provide 'gatsby:org)
;;; gatsby:org.el ends here
