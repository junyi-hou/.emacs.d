;;; gatsby:gentoo.el --- gentoo specific editing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.eclass\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("package\\.accept_keywords" . sh-mode))
(add-to-list 'auto-mode-alist '("package\\.mask" . sh-mode))
(add-to-list 'auto-mode-alist '("package\\.use" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.log\\'" . gatsby:gentoo--apply-ansi-color))

(defun gatsby:gentoo--apply-ansi-color ()
  "Color the whole buffer using ansi code."
  (ansi-color-apply-on-region (point-min) (point-max)))

;; TODO: generate functions/yasnippets to build ebuild

(provide 'gatsby:gentoo)
;;; gatsby:gentoo.el ends here
