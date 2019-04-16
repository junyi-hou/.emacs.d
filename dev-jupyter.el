;;; dev-jupyter.el -- jupyter frontend for REPL -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; load pkgs

(require 'dev-ivy)
(use-package f :commands f-entries)
(use-package tramp)
(use-package jupyter
  :commands
  (jupyter-repl-client
   jupyter-repl-run
   jupyter-repl-ret))


;; customizable variables

(defcustom dev-jupyter-remote-kernel-locations
  '("ssh://junyihou.sytes.net:9127/var/run/user/1000/jupyter/")
  "A list of remote kernel locations."
  :type 'list)


;; functions


(defun dev-jupyter-connect-kernel (kernel-type repl-name)
  "Connect to jupyter kernel found in `dev-jupyter-remote-kernel-locations`.  with KERNEL-TYPE.  The repl buffer name is given by REPL-NAME."
  (interactive)
  ;; step 1: concat all file lists
  (let* ((kernel-list '()))
    (dolist (dir dev-jupyter-remote-kernel-locations)
      (setq kernel-list (append kernel-list (f-entries dir))))
    ;; step 2: use ivy to list all selections
    (ivy-read "Choose kernel:" kernel-list
              :caller 'dev-jupyter-select-repl
              :action (lambda (x)
                        (jupyter-connect-repl x kernel-type)))))

(defun dev-jupyter-send-code (string buffer)
  "Send STRING to the jupyter repl in BUFFER."
  (save-excursion
      (switch-to-buffer-other-window buffer)
      (goto-char (point-max))
      (insert string)
      (jupyter-repl-ret)))

;; settings

(add-hook 'jupyter-repl-mode-hook #'company-mode)

(provide 'dev-jupyter)
;;; dev-jupyter.el ends here
