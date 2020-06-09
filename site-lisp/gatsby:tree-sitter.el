;;; gatsby:tree-sitter.el --- config for tree-sitter -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package tree-sitter
  :straight (tree-sitter :host github
                         :repo "ubolonton/emacs-tree-sitter"
                         :files ("lisp/*.el" "src" "Cargo.toml" "Cargo.lock"))
  :init

  ;; selectum-jump-to-header
  ;; using tree-sitter
  (defun gatsby:ts-selectum-jump--to-header (node)
    "Query PATTERNS in NODE using tree-sitter, return the list of line at which the PATTERN is found."
    (let ((pos
           (mapcar
            (lambda (n) (ts-node-start-position
                         (ts-get-nth-named-child node n)))
            (number-sequence 0 (1- (ts-count-named-children node))))))

      (thread-last pos
        (seq-filter #'identity)
        (mapcar
         (lambda (p) (save-excursion
                       (goto-char p)
                       (buffer-substring (line-beginning-position)
                                         (1- (line-end-position)))))
         ))))

  (defun gatsby:ts-selectum-jump-to-header ()
    ""
    (interactive)
    )


  
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter
  :straight (tree-sitter-langs :host github
                               :repo "ubolonton/emacs-tree-sitter"
                               :files ("langs/*.el" "langs/queries")))

(provide 'gatsby:tree-sitter)
;;; gatsby:tree-sitter.el ends here
