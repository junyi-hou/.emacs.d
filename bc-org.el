;;; bc-org.el -- my orgmode config



;; org-capture

(use-package org
  :config
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  :general
  (:keymap 'normal
   :prefix "SPC"
   "yc" 'org-capture))

(provide 'bc-org)
;;; bc-org.el ends here
