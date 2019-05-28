;;; bc-org.el -- my orgmode config

;;; Commentary:

;;; Code:

;; org-capture

(use-package org
  :config

  ;; general config
  (setq org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files '("~/org/")
        org-agenda-skip-scheduled-if-done t
        org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE")
          (sequence "|" "CANCELED" "SOMEDAY"))

        ;; src block settings
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)

  (add-hook 'org-capture-mode-hook #'evil-insert-state)
  (add-hook 'org-mode-hook #'company-mode)

  (setq org-capture-templates
        `(("t"
           "A todo item with deadline"
           entry
           (file "todo.org"),
           (concat "* TODO %?\n"
                   "CREATED:  %t\n")
           :empty-lines 1)
          )
        )

  ;; babel blocks
  (unless bc-venv-current-venv
    (bc-venv--enable-venv)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (jupyter . t))))

  (advice-add 'org-edit-src-code :after (lambda () (ignore-errors (eglot-ensure))))

  :general
  (:keymaps 'org-mode-map
   :states 'normal
   ;; do not need `evil-jump-item'
   "<tab>" 'org-cycle)

  (:keymaps 'org-mode-map
   :states '(normal visual motion)
   "<up>" 'org-previous-visible-heading
   "<down>" 'org-next-visible-heading
   "U" 'outline-up-heading
   "r" 'org-refile)

  (:keymaps 'org-mode-map
   :states '(insert normal visual motion)
   "C-d" 'org-forward-heading-same-level
   "C-u" 'org-backward-heading-same-level
   "M-j" 'org-shiftdown
   "M-k" 'org-shiftup
   "M-h" 'org-shiftleft
   "M-l" 'org-shiftright)

  (:keymaps '(normal motion visual)
   :prefix "SPC"
   "yc" 'counsel-org-capture
   "ya" 'org-agenda))


;; functions

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


(provide 'bc-org)
;;; bc-org.el ends here
