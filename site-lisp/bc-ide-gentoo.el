;;; bc-ide-gentoo.el --- gentoo specific editing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.eclass\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("package\\.accept_keywords" . sh-mode))
(add-to-list 'auto-mode-alist '("package\\.mask" . sh-mode))
(add-to-list 'auto-mode-alist '("package\\.use" . sh-mode))

;; TODO: generate functions/yasnippets to build ebuild

(provide 'bc-ide-gentoo)
;;; bc-ide-gentoo.el ends here
