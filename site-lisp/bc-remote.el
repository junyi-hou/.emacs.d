;;; bc-remote.el --- settings of working with remote machines -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar bc-remote-servers
  '(("junyi" "10.10.10.106#9127"))
  "A list of remote servers, with car being the user name and cdr the host")

(defvar bc-default-remote-server (car bc-remote-servers)
  "The default remote server.")

(defmacro with-remote-server (server &rest body)
  "Run BODY in SERVER."
  `(let ((default-directory ,server))
     ,@body))

(defmacro with-default-remote-server (&rest body)
  "Run BODY on `bc-default-remote-server'."
  `(let* ((server ,(apply #'format (cons "/ssh:%s@%s:" bc-default-remote-server))))
     (with-remote-server server ,@body)))

;; eshell integration
(with-eval-after-load 'eshell
  (progn
    (defvar-local bc-remote--eshell-local-dir nil)

    (defun eshell/ssh (&rest args)
    "ssh into a server if given.  If not, prompt user to pick one from `bc-remote-servers'."
    (if (file-remote-p default-directory)
        (if bc-remote--eshell-local-dir
            (eshell-named-command "cd" `(,bc-remote--eshell-local-dir))
          (eshell-named-command "cd"))
      (unless args
        (setq args `(,(ivy-read "connecting to: "
                                (mapcar
                                 (lambda (remote)
                                   (apply #'format (cons "/ssh:%s@%s:" remote)))
                                 bc-remote-servers)
                                :action 'identity))))
      (setq bc-remote--eshell-local-dir default-directory)
      (eshell-named-command "cd" args)))

    ))


(provide 'bc-remote)
;;; bc-remote.el ends here
