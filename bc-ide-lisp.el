;;; bc-ide-lisp.el -- setup ide features (auto-completion, code-jump, linter) for lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-autocomplete)
(require 'bc-linter)
(require 'bc-jump)

;; load stuffs
(require 'hl-defined)

(custom-set-faces
 '(hdefd-functions ((t (:inherit font-lock-function-name-face))))
 '(hdefd-variables ((t (:inherit font-lock-constant-face))))
 )

(eval-after-load 'dash '(dash-enable-font-lock)) ; syntax highlight for dash

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (company-mode 1)
                                  (dumb-jump-mode 1)
                                  (flycheck-mode 1)
                                  (hdefd-highlight-mode 1)))


(provide 'bc-ide-lisp)
;;; bc-ide-lisp.el ends here
