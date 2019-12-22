;;; bc-config.el --- init config files -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun bc-config--create-symlink (from to)
  "Create symlink from FROM to TO."
  (cond ((not (file-exists-p from))
         (user-error "Cannot find config file %s" from))
        ((file-exists-p to)
         (user-error "config file at %s already existed" to))
        (t (make-symbolic-link from to))))

(defun bc-config--init-xdg-config (pkg)
  "Symlink config file of PKG to XDG_CONFIG_HOME or \"~/.config/\" if XDG_CONFIG_HOME is not set."
  (let* ((source (expand-file-name
                  (format "%sconfig/%s" user-emacs-directory pkg)))
         (target (format "%s/%s"
                         (or (getenv "XDG_CONFIG_HOME")
                             (format "%s/.config" (getenv "HOME")))
                         pkg)))
    (bc-config--create-symlink source target)))

(defun bc-config--init-home-config (pkg)
  "Symlink config file of PKG to \"$HOME\"."
  (let* ((source (expand-file-name
                  (format "%sconfig/home/%s" user-emacs-directory pkg)))
         (target (format "%s/%s" (getenv "HOME") pkg)))
    (bc-config--create-symlink source target)))

(defun bc-init-config ()
  "Init config using config files found at `user-emacs-directory/config'."
  (interactive)
  (mapc 'bc-config--init-xdg-config
        (directory-files (format "%sconfig" user-emacs-directory) nil "[^.home]"))
  (mapc 'bc-config--init-home-config
        (directory-files (format "%sconfig/home" user-emacs-directory)
                         nil
                         "\\.[a-zA-Z0-9]+")))

(provide 'bc-config)
;;; bc-config.el ends here
