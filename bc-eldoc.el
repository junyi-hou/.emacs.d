;;; bc-eldoc.el --- eldoc settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eldoc
  :defer t
  :init
  (setq-default eldoc-idle-delay 2)
  :config
  (use-package eldoc-posframe
    :after eldoc
    :load-path "~/Documents/projects/posframe-collection/"
    :hook (eldoc-mode . eldoc-posframe-mode)))


(provide 'bc-eldoc)
;;; bc-eldoc.el ends here
