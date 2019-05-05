;;; bc-autocomplete.el -- autocomplete settings using company and yasnippet -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package yasnippet
  :commands
  (yas-minor-mode yas-reload-all yas-expand yas-next-field yas-prev-field
   yas-expand-snippet yas-lookup-snippet)
  :general
  (:keymaps 'yas-minor-mode-map
            "<tab>" nil
            "TAB" nil
            "M-f" 'yas-expand
            "M-j" 'yas-next-field
            "M-k" 'yas-prev-field))

(use-package company
  :commands
  (company-mode company-complete company-complete-common)
  :general
  (:keymaps 'company-active-map
            "<tab>" 'company-complete-common
            "TAB" 'company-complete-common
            "M-j" 'company-select-next
            "M-k" 'company-select-previous-or-abort)

  :config
  (setq company-idle-delay 0
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-minimum-prefix-length 2
        company-backends '((company-files company-capf company-yasnippet)
                           (company-dabbrev company-dabbrev-code))))

(use-package company-posframe
  :after company
  :commands company-posframe-mode)

(use-package company-statistics :after company)

;; function:

(defun bc-autocomplete--yas-match-regexp (hash-table str)
  "Attempt to translate STR to a `yas-snippet' name through the HASH-TABLE.

If there is a successful match, return a list consists of 1. the `yas-snippet' name associated with the regexp; 2 extract the strings in defined in the groups as a list.  Return an empty list if match failed.

example:
hash-table: (\"([a-z]+?)(_)?([a-z]*)\" -> '(\"defun\" '(1,4)))

aa_bb => '(\"defun\" \"aa_bb\" '(\"aa\" \"bb\"))
aa    => '(\"defun\" \"aa\" '(\"aa\"))
12a   => '()"

  (let* ((match '()))
    (maphash (lambda (k v)
               (when (string-match k str)
                 (let* ((name (car v))
                        (group (car (cdr v)))
                        (group-string '()))
                   (while (and group (match-end (car group)))
                     (add-to-list 'group-string
                                  (match-string (car group) str) t)
                     (setq group (cdr group)))
                   (add-to-list 'match (list name group-string) t))))
             hash-table)
    ;; return the first match
    (car match)))

;;;###autoload
(defun bc-autocomplete-yas-expand-regexp (&optional thing hash-table)
  "Attempt to expand a `yas-snippet' given THING at point using `bc-autocomplete--yas-match-regexp'.

This function first attempts to match STR against the regexp defined as keys in the HASH-TABLE.  If there is a successful match, it will expand the snippet, defined as the car of the value associated with the keyword regexp in HASH-TABLE.  Furthermore, it will put the part of matches into the fields of the snippets according to the grouping defined in the `cdr' of the value associated with the keyword regexp.  If there is no regexp match found in HASH-TABLE, try `yas-expand'.

example:
hash-table: (\"([a-z]+?)(_)?([a-z]*)\" -> '(\"defun\" '(1,4)))
snippet: (defun $1 ($2))

aa_bb => (defun aa (bb))
aa    => (defun aa ())
12a   => (yas-expand-snippet (yas-lookup-snippet \"12a\"))"

  (interactive)
  (let* ((hash-table (or hash-table (make-hash-table)))
         (thing (or thing 'sentence))
         (str (thing-at-point thing t))
         (match (bc-autocomplete--yas-match-regexp hash-table str)))
    (if match
        (let* ((snippet-name (car match))
               (group (car (cdr match))))
          (kill-region (car (bounds-of-thing-at-point thing))
                       (cdr (bounds-of-thing-at-point thing)))
          (yas-expand-snippet (yas-lookup-snippet snippet-name))
          (while group
            (insert (car group))
            (yas-next-field)
            (setq group (cdr group))))
      (yas-expand))))

;; settings

(add-hook 'company-mode-hook #'company-posframe-mode)
(add-hook 'company-mode-hook #'yas-minor-mode)

(provide 'bc-autocomplete)
;;; bc-autocomplete.el ends here
