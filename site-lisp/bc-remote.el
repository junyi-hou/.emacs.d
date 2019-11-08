;;; bc-remote.el --- settings of working with remote machines -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar bc-remote-servers
  '(("junyi" "10.10.10.106#9127"))
  "A list of remote servers, with car being the user name and cdr the host")

(defmacro with-remote-temp-buffer (&optional server body)
  "Execute BODY in remote SERVER.")

;; eshell integration
(with-eval-after-load 'eshell
  (defun eshell/ssh (&rest args)
    "ssh into a server if given.  If not, prompt user to pick one from `bc-remote-servers'."
    (unless args
      (setq args `(,(ivy-read "connecting to: "
                             (mapcar
                              (lambda (remote)
                                (apply #'format (cons "/ssh:%s@%s:" remote)))
                              bc-remote-servers)
                             :action 'identity))))
    (eshell-named-command "cd" args)))

(provide 'bc-remote)
;;; bc-remote.el ends here
