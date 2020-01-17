;;; bc-project.el --- find-file-in-project -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-core)

(use-package project
  :init

  (with-eval-after-load 'vc-hooks
    (setq vc-follow-symlinks t))

  ;; variables
  (defconst bc-project-saved-project
    '("~/.emacs.d/")
    "A list of project roots.")

  (defconst bc-project-search-path
    '("~/research/" "~/projects/")
    "A list of path to search for projects.")

  ;; function
  (defun bc-project-get-root (&optional dir)
    "Return the project root of DIR. If DIR is not given, use `default-directory'.  If there is no project found at DIR, return nil."
    (let ((project (project-current nil dir)))
      (when (eq 'vc (car project))
        ;; make sure we identify the project using git
        (cdr project))))

  (defun bc-project-get-file-list (dir)
    "Return the file list of the project at DIR via \"git ls-files -zco --exclude-standard\"."
    (let ((default-directory (bc-project-get-root dir)))
      (split-string
       (shell-command-to-string "git ls-files -co --exclude-standard")
       "\n")))

  (defun bc-project-find-file (&optional dir)
    "Find file in project at DIR.  If DIR does not contain a project, fall backs to `counsel-find-file'."
    (interactive)
    (let* ((default-directory (or dir default-directory))
           (proj? (bc-project-get-root dir))
           (collection (when proj? (bc-project-get-file-list proj?))))
      (if proj?
          ;; inside a project
          (ivy-read
           (concat "Find file in " proj? ": ")
           collection
           :action (lambda (file) (find-file (concat (bc-project-get-root) file))))
        ;; outside of project
        (call-interactively #'counsel-find-file))))

  (defun bc-project--search-path ()
    "Search paths registered in `bc-project-search-path' and return a list of projects identified by git."
    (apply #'append
           (mapcar
            (lambda (path)
              (seq-filter
               (lambda (dir) (bc-project-get-root dir))
               (directory-files path 'full "[^\.]")))
            bc-project-search-path)))

  (defun bc-project-switch-project ()
    "Switch to project."
    (interactive)
    (let ((enable-recursive-minibuffers t)
          (projs (append bc-project-saved-project (bc-project--search-path))))
      (ivy-read
       "Switch to:"
       projs
       :action
       (lambda (project) (bc-project-find-file (file-name-as-directory project))))))

  :general
  (:keymaps '(normal motion visual emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "op" 'bc-project-switch-project
   "of" 'bc-project-find-file))

(provide 'bc-project)
;;; bc-project.el ends here
