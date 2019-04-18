;;; ide-stata.el -- create IDE for stata -*- lexical-binding: t; -*-

;;; Commentary:
;; a large portion of this file is taken from ESS, including the
;; stata-major-mode, the syntax highlight, etc.
;; REPL integration is done through emacs-jupyter


;;; Code:

(require 'stata)

;; settings

(general-define-key
 :states '(motion normal visual)
 :keymaps 'stata-mode-map
 :prefix "SPC"
 "rr" 'stata-send-buffer-or-region
 "rl" 'stata-send-line-or-region
 "ro" 'stata-open-or-switch-to-repl
 "rO" 'stata-open-or-switch-to-remote-repl
 "rc" 'stata-attach-buffer
 "rh" 'jupyter-inspect-at-point)

(provide 'ide-stata)
;;; ide-stata.el ends here
