;;; gatsby:os.el --- os specific setting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package i3
  :straight (i3-mode :repo "junyi-hou/i3-mode" :host github
                     :files ("i3.el" "i3-call"))
  :config
  (i3-mode 1))



(provide 'gatsby:os)
;;; gatsby:os.el ends here
