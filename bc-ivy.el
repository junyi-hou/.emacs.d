;;; bc-ivy.el -- config for ivy -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bc-core)

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-count-format "" ; do not count candidates
        ivy-initial-inputs-alist nil ; do not use ^
        ivy-re-builders-alist '((t   . ivy--regex-ignore-order)) ; allow input not in order
        ivy-format-function #'ivy-format-function-line ; highlight til EOL
        ivy-magic-slash-non-match-action nil ; disable magic slash on nonmatch
        projectile-completion-system 'ivy)  ; use projectile

  (use-package counsel
    :after ivy
    :general
    (:keymaps '(normal visual motion)
              :prefix "SPC"
              "or" 'counsel-recentf
              "js" 'counsel-ag
              "jr" (lambda () (interactive)
                     (counsel-ag (symbol-name (symbol-at-point))))))

  (use-package ivy-xref
    :init
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

  (use-package ivy-posframe
    :after ivy
    :config
    (setq ivy-display-function #'ivy-posframe-display-at-point)
    (ivy-posframe-enable))

  :general
  (:keymaps 'ivy-minibuffer-map
   "M-j" 'ivy-next-line
   "M-k" 'ivy-previous-line
   "M-J" (lambda () (interactive) (ivy-next-line 3))
   "M-K" (lambda () (interactive) (ivy-previous-line 3))))


;; functions

(defun bc-ivy-open-remote-shell (&optional remote)
  "Open a ivy menu with a list of REMOTE location.  Open a eshell at the chosen location."
  (interactive)
  (let* ((remote (or remote bc-default-remote)))
    (ivy-read "Where to?"
              '("home/junyi/Documents/"
                "home/junyi/Documents/Research/"
                "home/junyi/Downloads/data/")
              :action (lambda (x)
                        (let* ((height (/ (window-total-height) 3)))
                          (split-window-vertically (- height))
                          (evil-window-down 1)
                          (bc-eshell--open (concat remote x)))))))

;; (defun bc-ivy-xref--find-xrefs (input kind arg display-action)
;;   (let ((xrefs (funcall (intern (format "xref-backend-%s" kind))
;;                         (xref-find-backend)
;;                         arg)))
;;     (unless xrefs
;;       (user-error "No %s found for: %s" (symbol-name kind) input))
;;     (let ((xref-pos (point))
;;           (xref-buffer (current-buffer))
;;           (success nil))
;;       (ivy-read "Find References: " (ivy-xref-make-collection xrefs)
;;                 :unwind (lambda ()
;;                           (unless success
;;                             (switch-to-buffer xref-buffer)
;;                             (goto-char xref-pos)
;;                             (recenter)))
;;                 :action (lambda (x)
;;                           (let ((location (cdr x)))
;;                             (let* ((marker (xref-location-marker location))
;;                                    (buf (marker-buffer marker)))
;;                               (bc-ivy-xref-posframe-show x)
;;                               (with-ivy-window
;;                                 (goto-char marker)
;;                                 (recenter))
;;                               (unless (eq 'ivy-call this-command)
;;                                 (setq success t)))))))))

;; TODO:
;; uniform ivy backend:
;; 1. projects
;; 2. files
;; 3. buffers
;; 4. recently opened files

;; settings


(provide 'bc-ivy)
;;; bc-ivy.el ends here
