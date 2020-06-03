;;; gatsby:style.el --- sytle guide: spell and grammar check -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby:core)

(use-package flyspell-correct
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)

  :init
  (defun gatsby:style-flyspell-correct-selectrum (candidates word)
    "Run `completing-read' for the given CANDIDATES

List of CANDIDATES is given by flyspell for the WORD."
    (let* ((selectrum-should-sort-p nil)
           (save "[SAVE]")
           (accept-session "[ACCEPT (session)]")
           (accept-buffer "[ACCEPT (buffer)]")
           (skip "[SKIP]")
           (result (completing-read
                    (format "Correcting '%s': " word)
                    (append (list save accept-session accept-buffer skip)
                            candidates)
                    nil nil nil nil (car candidates))))
      (cond
       ((string= result save)
        (cons 'save word))
       ((string= result accept-session)
        (cons 'session word))
       ((string= result accept-buffer)
        (cons 'buffer word))
       ((string= result skip)
        (cons 'skip word))
       (t
        result))))

  (setq flyspell-correct-interface #'gatsby:style-flyspell-correct-selectrum)

  :general
  (:keymaps '(motion normal visual)
   :prefix "SPC"
   "sp" 'flyspell-correct-wrapper)
  (:keymaps 'insert
   :prefix "C-c"
   "s" 'flyspell-correct-wrapper))

(provide 'gatsby:style)
;;; gatsby:style.el ends here
