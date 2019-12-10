;;; bc-ide-stata.el --- stata settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ado-mode
  :straight (ado-mode
             :repo "louabill/ado-mode"
             :host github)
  :init
  (define-derived-mode jupyter-stata-mode ado-mode "Jupyter-Stata"
    "This allows `org-mode' to correctly highlights `jupyter-stata' src blocks without using `ess-mode'. To do so, please install `jupyter-emacs' package to enable `ob-jupyter', and the stata kernel (pip install stata-kernel --user) to enable jupyter to talk to stata kernel."))

(provide 'bc-ide-stata)
;;; bc-ide-stata.el ends here
