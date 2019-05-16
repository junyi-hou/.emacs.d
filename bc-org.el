;;; bc-org.el -- my orgmode config

;;; Commentary:

;;; Code:

;; org-capture

(use-package org
  :config
  (setq org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files '("~/org/")
        org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

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
          ("s"
           "A scheduled item"
           entry
           (file "todo.org"),
           (concat "* scheduled TODO %?\n"
                   "CREATED: %t\n")
           :empty-lines 1)
          ("a" "test"
           entry
           (file "todo.org"),
           (concat "* test\n"
                   "%^T\n"
                   "%?"))
          )
        )
  :general
  (:keymaps 'org-mode-map
   :states 'normal
   "<tab>" 'org-cycle ; do not need `evil-jump-item'
   )

  (:keymaps 'org-mode-map
   :states '(normal visual motion)
   "J" 'org-forward-heading-same-level
   "K" 'org-backward-heading-same-level
   "<up>" 'org-previous-visible-heading
   "<down>" 'org-next-visible-heading
   "U" 'outline-up-heading
   "r" 'org-refile
   )

  (:keymaps 'org-mode-map
   :states '(insert normal visual motion)
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
  "insert today's date in org-mode's format"
  (insert (concat "<"
                  (shell-command-to-string "echo -n $(date +%Y-%m-%d)")
                  ">")))

(defun bc-org-insert-date-time ()
  "insert today's date-time in org-mode's format"
  (insert (concat "<"
                  (shell-command-to-string "echo -n $(date +%Y-%m-%d)")
                  " "
                  (shell-command-to-string "echo -n $(date +%H:%M)")
                  ">")))

(provide 'bc-org)
;;; bc-org.el ends here
