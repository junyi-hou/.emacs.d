;;; bc-ide-gentoo.el --- gentoo specific editing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ebuild-mode
  :ensure nil
  :defer t
  :config
  (require 'ebuild-mode))

;; TODO: generate functions/yasnippets from ebuild-mode.el

(provide 'bc-ide-gentoo)
;;; bc-ide-gentoo.el ends here
