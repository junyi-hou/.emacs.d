;;; ide-stata.el -- create IDE for stata -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dev-autocomplete)
(require 'dev-linter)
(require 'dev-jump)
(require 'dev-repl)


(defvar dev-stata-repl nil
  "Name of the stata REPL buffer, nil means no such buffer.")


(defalias dev-stata--send-string
  (lambda (string)
    (dev-repl--send-string
     dev-stata-repl
     (lambda (x) (insert (concat "do" x "'")))
     string))
  "Send STRING to stata interpreter.")

(defun dev-stata--open-repl-or-switch-to-repl ()
  "Open a REPL using jupyter kernel.  If there is already a python REPL opened, switch to that REPL."
  (interactive)
  (if dev-stata-repl
      (switch-to-buffer-other-window dev-python-repl)
    ;; else if the repl does not exists
    (progn
      (dev-repl-open-repl "stata")
      (setq dev-stata-repl "stata"))))


(defun dev-stata--run-line-or-visual ()
  "Send current line or visually selected region to interpreter."
  (interactive)
  (if (evil-visual-state-p)
      (dev-stata--send-string
       (buffer-substring (region-beginning) (region-end)))
    ;; else: send current line
    (dev-stata--send-string (thing-at-point 'line t))))

(defun dev-stata--get-pos (l c)
  "Return the buffer position at line L and column C."
  (save-excursion
    (goto-char (point-min))
    (goto-line l)
    (move-to-column c)
    (point)))

(defun dev-stata--get-vline ()
  "Return visually selected region, expended to include all of the lines."
  (interactive)
  (let* ((begin-line (line-number-at-pos (region-beginning)))
         (end-line   (line-number-at-pos (region-end))))
    (deactivate-mark)
    (let* ((begin-pos (dev-stata--get-pos begin-line 0))
           (end-pos (dev-stata--get-pos end-line (window-body-width))))
      (buffer-substring begin-pos end-pos))))

(defun dev-stata--run-buffer-or-visual-line ()
  "Send buffer or visually selected region to interpreter."
  (interactive)
  (if (evil-visual-state-p)
      (progn
        (dev-stata--send-string (dev-stata--get-vline))
        (deactivate-mark))
    ;; else: send the whole buffer
    (dev-stata--send-string (buffer-string))))

(general-define-key
 :states '(motion normal visual)
 :keymaps '
 :prefix "SPC"
 "rr" 'dev-stata--run-buffer-or-visual-line
 "rl" 'dev-stata--run-line-or-visual
 "ro" 'dev-stata--open-repl-or-switch-to-repl
 "rO" 'dev-stata--open-remote-repl)

(provide 'ide-stata)
;;; ide-stata.el ends here
