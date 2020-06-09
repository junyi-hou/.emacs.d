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

  ;; (defvar-local gatsby:ts-query-pattern nil)
  
  ;; (defun gatsby:ts--node-with-text (node)
  ;;   "Convert NODE into (line text . node)."
  ;;   (let ((pos (ts-node-start-position node)))
  ;;     (save-excursion
  ;;       (goto-char pos)
  ;;       `(,(buffer-substring (line-beginning-position) (1- (line-end-position))) .
  ;;         ,node))))

  ;; (defun gatsby:ts--list-direct-children (node pattern)
  ;;   "List direct children of NODE of PATTERN."
  ;;   (let (  ;; HACK: convert pattern to type for filtering
  ;;         (type (seq-map (lambda (p) (symbol-name (car p))) pattern))
  ;;         )
  ;;     (thread-last (number-sequence 0 (1- (ts-count-named-children node)))
  ;;       (seq-map (lambda (n) (ts-get-nth-named-child node n)))
  ;;       (seq-remove (lambda (node) (not
  ;;                                   (member (ts-node-type node) type))))
  ;;       (seq-map (lambda (node) (gatsby:ts--node-with-text node))))))

  ;; (defun gatsby:ts--list-all-subnodes (node pattern)
  ;;   "List all PATTERN under NODE."
  ;;   (let ((query (ts-make-query tree-sitter-language pattern)))
  ;;     (thread-last (ts-query-captures query node #'ignore)
  ;;       (seq-map #'cdr)
  ;;       (seq-map (lambda (node) (gatsby:ts--node-with-text node))))))

  ;; (defun gatsby:ts--jump-to-header-recursively (node &optional candidates)
  ;;   (let* ((enable-recursive-minibuffers t)
  ;;          (candidates (or candidates
  ;;                          (gatsby:ts--list-direct-children node
  ;;                                                           gatsby:ts-query-pattern)))
  ;;          ;; (select (completing-read "Jump to: "
  ;;          ;;                          (mapcar #'car candidates)))
  ;;          (select (caar candidates))
  ;;          (selected-node (alist-get select candidates)))
  ;;     ;; is there other nodes below it?
  ;;     (if-let ((nodes (gatsby:ts--list-direct-children selected-node
  ;;                                                      gatsby:ts-query-pattern)))
  ;;         ;; yes
  ;;         (gatsby:ts--jump-to-header-recursively selected-node nodes)
  ;;       ;; no
  ;;       selected-node)))

  ;; (defun gatsby:ts--jump-to-header (node)
  ;;   (let* ((candidates (gatsby:ts--list-all-subnodes node gatsby:ts-query-pattern))
  ;;          (select (completing-read "Jump to: "
  ;;                                   (mapcar #'car candidates))))
  ;;     (alist-get select candidates)))

  ;; (defun gatsby:ts-jump-to-header (arg)
  ;;   (interactive "p")
  ;;   (let* ((selectrum-should-sort-p nil)
  ;;          (root (ts-root-node tree-sitter-tree))
  ;;          (selected-node (if arg (gatsby:ts--jump-to-header-recursively root)
  ;;                           (gatsby:ts--jump-to-header root))))
  ;;     (goto-char (ts-node-start-position selected-node))))
  
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter
  :straight (tree-sitter-langs :host github
                               :repo "ubolonton/emacs-tree-sitter"
                               :files ("langs/*.el" "langs/queries")))

(provide 'gatsby:tree-sitter)
;;; gatsby:tree-sitter.el ends here
