;;; bc-ide-python.el -- provide IDE features for editing python files -*- lexical-binding: t; -*-

;;; Commentary:
;; This module adds on top of the default python.el:
;; linting and autocompletion support using flycheck and company;
;; REPL support using jupyter;
;; jump to definition using dumb-jump;
;; code formatting using py-papf

;;; Code:

;;load pkgs
(require 'bc-autocomplete)
(require 'bc-linter)
(require 'bc-jump)
(require 'bc-jupyter)

(use-package company-jedi
  :after company
  :commands company-jedi)

(use-package yapfify
  :commands yapf-mode)


;; functions

(defalias 'bc-python-local-repl
  (lambda () (interactive)
    (bc-jupyter-start-or-switch-to-repl "python"))
  "Open a jupyter repl for python interpreter.")

(defalias 'bc-python-reconnect
  (lambda () (interactive) (bc-jupyter-reconnect "python"))
  "Reconnect to the current REPL.")

(defalias 'bc-python-remote-repl
  (lambda () (interactive)
    (bc-jupyter-start-or-switch-to-repl "python" t))
  "Open a remote jupyter repl for python interpreter")

(defun bc-python--dedent (string)
  "De-indent multi-line STRING by remove common indentations."
  (let* ((s-list (split-string string "\n" t))
         (has-indent (string-match "^[ \t]+" (car s-list)))
         (end-indent (when has-indent (match-end 0))))
    (let ((out-list (if end-indent
                        (mapcar
                         (lambda (s)
                           (concat
                            (substring-no-properties s end-indent) "\n"))
                         s-list)
                      (mapcar (lambda (s) (concat s "\n")) s-list))))
          (seq-reduce #'concat out-list ""))))

(defalias 'bc-python--send
  (lambda (string) (bc-jupyter--send (bc-python--dedent string)))
  "Send string using `bc-jupyter--send' with `bc-python--dedent' to processing STRING first.")

(defun bc-python-send-string ()
  "If in `evil-visual-state', send the current region to `jupyter-current-client'.  Otherwise send the current line."
  (interactive)
  (if (evil-visual-state-p)
      (bc-python--send
       (buffer-substring-no-properties (region-beginning) (region-end)))
    (bc-python--send (thing-at-point 'line t))))

(defun bc-python--hook ()
  "Initiate venv, autocomplete and linters."
   ;; load venv
   (bc-jupyter--enable-venv)
   ;; setup auto complete
   (make-local-variable 'company-backends)
   (setq company-backends (list (push 'company-jedi (car company-backends))))
   (company-mode 1)
   ;; setup linter
   (flycheck-mode 1)
   ;; set tab-width
   (setq tab-width 4))


;; settings

;; key bindings
(general-define-key
 :states '(motion normal visual)
 :keymaps 'python-mode-map
 :prefix "SPC"
 "rr" 'bc-jupyter-eval-buffer-or-region

 "rl" 'jupyter-eval-line-or-region
 "re" 'bc-python-send-string
 "ro" 'bc-python-local-repl
 "rO" 'bc-python-remote-repl
 "rc" 'jupyter-repl-associate-buffer
 "rh" 'jupyter-inspect-at-point
 "rz" 'bc-python-reconnect)

(add-hook 'python-mode-hook #'bc-python--hook)
(add-hook 'python-mode-hook #'yapf-mode)

(provide 'bc-ide-python)
;;; bc-ide-python ends here
