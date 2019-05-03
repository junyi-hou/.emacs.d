;;; bc-ide-latex.el -- ide for latex editing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-autocomplete)
(require 'bc-linter)
(require 'bc-jump)

(use-package tex
  :defer t
  :ensure auctex
  :config
  ;; use emacs to view pdf
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)
  :general
  (:keymaps 'tex-mode-map
   :states '(normal visual motion)
   "SPC" nil)
  
  (:keymaps 'tex-mode-map
   :states '(normal visual motion)
   :prefix "SPC"
   "rr" 'tex-compile
   "ro" 'tex-view)
)

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq pdf-view-display-size 'fit-page)
  (evil-set-initial-state 'pdf-view-mode 'motion)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  :general
  (:keymaps 'pdf-view-mode-map
   :states '(motion normal visual)
   "j" 'pdf-view-scroll-up-or-next-page
   "k" 'pdf-view-scroll-down-or-previous-page
   "J" 'pdf-view-next-page
   "K" 'pdf-view-previous-page
   "+" 'pdf-view-enlarge
   "-" 'pdf-view-shrink
   "gg" 'pdf-view-first-page
   "G" 'pdf-view-last-page
   "/" 'isearch-forward
   "?" 'isearch-backward
   "SPC" nil)

  (:keymaps 'pdf-view-mode-map
   :states '(motion normal visual)
   :prefix "SPC"
   "q" 'kill-this-buffer))

(use-package company-auctex
  :after '(company tex)
  :config (add-hook 'tex-mode-hook #'company-auctex-init))

(provide 'bc-ide-latex)
;;; bc-ide-latex.el ends here
