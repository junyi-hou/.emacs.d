;;; bc-os.el --- os specific settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(cond
 ((string= system-type "darwin")
  (progn
    (mac-auto-operator-composition-mode)

    (add-to-list 'initial-frame-alist '(fullscreen . maximized))

    (use-package exec-path-from-shell
      :config
      (exec-path-from-shell-initialize))))

 ((string-match-p "linux" (symbol-name system-type))
  (progn
    (require 'bc-exwm))))

(provide 'bc-os)
;;; bc-os.el ends here
