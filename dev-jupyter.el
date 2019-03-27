;;; dev-jupyter.el -- jupyter frontend for REPL

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

(defun dev-jupyter-connect-kernel (repl-name)
  "Connect to jupyter kernel found in `dev-jupyter-remote-kernel-locations`.
The repl buffer name is given by REPL-NAME."
  (interactive)
  ;; step 1: concat all file lists
  (let* ((kernel-list '()))
    (dolist (dir dev-jupyter-remote-kernel-locations)
      (setq kernel-list (append kernel-list (f-entries dir))))
    ;; step 2: use ivy to list all selections
    (ivy-read "Choose kernel:" kernel-list
              :caller 'dev-jupyter-select-repl
              :action (lambda (x)
                        (jupyter-connect-repl x "python3-repl")))))

(defun dev-jupyter--get-pos-line-column (l c)
  "Return the buffer position at line L and column C."
  (save-excursion
    (goto-char (point-min))
    (goto-line l)
    (move-to-column c)
    (point)))

(defun dev-jupyter--get-vline ()
  "Return visually selected region, expended to include all of the lines."
  (interactive)
  (let* ((begin-line (line-number-at-pos (region-beginning)))
         (end-line   (line-number-at-pos (region-end))))
    (deactivate-mark)
    (let* ((begin-pos (pos begin-line 0))
           (end-pos (pos end-line (window-body-width))))
      (buffer-substring begin-pos end-pos))))

(defun dev-jupyter-send-code (string buffer back)
  "Send STRING to the jupyter repl in BUFFER.  If BACK is t, go back to code buffer."
  (save-excursion
    (let* ((code-buffer (current-buffer)))
      (switch-to-buffer-other-window buffer)
      (goto-char (point-max))
      (insert string)
      (jupyter-repl-ret)
      (switch-to-buffer-other-window code-buffer))))

;; settings

(add-hook 'jupyter-repl-mode-hook #'company-mode)

(provide 'dev-jupyter)
;;; dev-jupyter.el ends here
