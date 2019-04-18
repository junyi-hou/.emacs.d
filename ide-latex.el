;;; ide-latex.el -- provide IDE features for editing latex files

;;; comentary:
;; instead of having a separate mode, use org mode to edit org file and export to latex/pdf file

;;; Code:

;; define latex regex yas-snippets
;; mostly math related

(defconst dev-latex--regexp-math-atom
  "\\([[:digit:]]*\\(\\\\?[[:alpha:]]+[[:space:]]?\\)*\\)"
  "Regexp for a math atom.
example:
10\\pi; \\alpha\\beta.")

(defconst dev-latex--regexp-subscripts
  "\\(\\\\?[[:alpha:]]+\\)\\([[:digit:]]+\\)"
  "Regexp for subscripts consists with a letter and a digit list.
example:
a1 => a_{1}")

(defconst dev-latex--regexp-superscripts
  "\\(\\\\?[[:alpha:]]+\\)^\\([[:digit:]]+\\)"
  "Regexp for subscripts consists with a letter and a digit list.
example:
a^2 => a^{2}")

(defcustom dev-latex-regexp-table
  (let ((hash (make-hash-table :test 'equal)))
    (puthash dev-latex--regexp-subscripts '("subscript" '(1 2)) hash)
    (puthash dev-latex--regexp-superscripts '("superscript" '(1 2)) hash)
    hash)
  "Hash table for yas to match in the latex mode."
  :group 'development
  :type 'hash-table)

;; keymaps

;; for automatic filled snippets
(general-define-key
 :keymaps 'yas-minor-mode-map
 "M-f" (lambda () (interactive)
         (dev-autocomplete-yas-expand-regexp dev-latex-regexp-table)))
 

(provide 'ide-latex)
;;; ide-latex.el ends here
