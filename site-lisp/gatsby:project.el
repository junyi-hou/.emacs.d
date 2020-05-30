;;; gatsby:project.el --- find-file-in-project -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby:core)

(use-package project
  :init

  (with-eval-after-load 'vc-hooks
    (setq vc-follow-symlinks t))

  ;; variables
  (defconst gatsby:project-saved-project
    '("~/.emacs.d/")
    "A list of project roots.")

  (defconst gatsby:project-search-path
    '("~/research/" "~/projects/")
    "A list of path to search for projects.")

  ;; function
  (defun gatsby:project-get-root (&optional dir)
    "Return the project root of DIR. If DIR is not given, use `default-directory'.  If there is no project found at DIR, return nil."
    (let ((project (project-current nil dir)))
      (when (eq 'vc (car project))
        ;; make sure we identify the project using git
        (cdr project))))

  (defun gatsby:project-get-file-list (dir)
    "Return the file list of the project at DIR via \"git ls-files -zco --exclude-standard\"."
    (let ((default-directory (gatsby:project-get-root dir)))
      (split-string
       (shell-command-to-string "git ls-files -co --exclude-standard")
       "\n")))

  (defun gatsby:project-find-file (&optional dir)
    "Find file in project at DIR.  If DIR does not contain a project, fall backs to `counsel-find-file'."
    (interactive)
    (let* ((default-directory (or dir default-directory))
           (proj? (gatsby:project-get-root dir))
           (collection (when proj? (gatsby:project-get-file-list proj?))))
      (if proj?
          ;; inside a project
          (ivy-read
           (concat "Find file in " proj? ": ")
           collection
           :action (lambda (file) (find-file (concat (gatsby:project-get-root) file))))
        ;; outside of project
        (call-interactively #'counsel-find-file))))

  (defun gatsby:project--search-path ()
    "Search paths registered in `gatsby:project-search-path' and return a list of projects identified by git."
    (--mapcat (--filter (gatsby:project-get-root it)
                        (directory-files it 'full "[^\.]"))
              gatsby:project-search-path))

  (defun gatsby:project-switch-project ()
    "Switch to project."
    (interactive)
    (let ((enable-recursive-minibuffers t)
          (projs (-concat gatsby:project-saved-project (gatsby:project--search-path))))
      (ivy-read
       "Switch to:"
       projs
       :action
       (lambda (project) (gatsby:project-find-file (file-name-as-directory project))))))

  :general
  (:keymaps '(normal motion visual emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "op" 'gatsby:project-switch-project
   "of" 'gatsby:project-find-file))

(provide 'gatsby:project)
;;; gatsby:project.el ends here
