;;; gatsby:minibuffer.el --- enhancement of minibuffer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package prescient
  :init
  (setq prescient-save-file (concat no-littering-var-directory "prescient-save.el"))
  :config
  (prescient-persist-mode 1))

(use-package selectrum
  :straight (selectrum :host github :repo "raxod502/selectrum")
  :defines (selectrum-minibuffer-bindings selectrum-should-sort-p)
  :init
  (selectrum-mode 1)

  (defun gatsby:selectrum--remove-until-slash (bound n)
    "Return the position of the backwards Nth slash until BOUND.
If no slash was found, return BOUND."
    (save-excursion
      (if-let ((found (search-backward "/" bound 'noerror n)))
          (1+ found)
        bound)))

  (defun gatsby:selectrum-better-backspace ()
    "If `point' is at \"/\", delete till the last \"/\"."
    (interactive)
    (cond ((thing-at-point-looking-at "~/")
           (progn
             (delete-region (minibuffer-prompt-end) (point))
             (insert "/home/")))
          ((string= (buffer-substring (minibuffer-prompt-end) (point)) "/")
           (call-interactively #'backward-delete-char))
          ((thing-at-point-looking-at "/")
           (delete-region (gatsby:selectrum--remove-until-slash
                           (minibuffer-prompt-end) 2)
                          (point)))
          (t (call-interactively #'backward-delete-char))))

  (defun gatsby:selectrum-next-candidate-cycle ()
    "Move selection to next candidate, if at the end, go to the top."
    (interactive)
    (when selectrum--current-candidate-index
      (setq selectrum--current-candidate-index
            (if (= selectrum--current-candidate-index
                   (1- (length selectrum--refined-candidates)))
                (if selectrum--match-required-p 0 -1)
              (1+ selectrum--current-candidate-index)))))

  (defun gatsby:selectrum-previous-candidate-cycle ()
    "Move selection to previous candidate, if at the beginning, go to the end."
    (interactive)
    (when selectrum--current-candidate-index
      (setq selectrum--current-candidate-index
            (if (= selectrum--current-candidate-index
                   (if selectrum--match-required-p 0 -1))
                (1- (length selectrum--refined-candidates))
              (1- selectrum--current-candidate-index)))))

  (defun gatsby:selectrum-unified-tab ()
    "<tab> does the following things
1. if there is a common part among candidates, complete the common part;
2. if there is only one candidate, select the candidate
3. if the last command is `gatsby:selectrum-unified-tab', `gatsby:selectrum-previous-candidate-cycle' or `gatsby:selecturm-next-candidate-cycle', then select the current candidate"
    (interactive)
    (when selectrum--current-candidate-index
      (let* ((input (buffer-substring-no-properties
                     (minibuffer-prompt-end)
                     (point-max)))
             (common (try-completion "" selectrum--refined-candidates)))
        (cond
         ;; case 3
         ((memq last-command '(gatsby:selectrum-unified-tab
                               gatsby:selectrum-next-candidate-cycle
                               gatsby:selectrum-previous-candidate-cycle))
          (selectrum-select-current-candidate))
         ;; case 2
         ((= 1 (length selectrum--refined-candidates))
          ;; do not use dired to open folder
          (if (directory-name-p (car selectrum--refined-candidates))
              (progn
                (delete-region (gatsby:selectrum--remove-until-slash
                                (minibuffer-prompt-end) 1)
                               (point-max))
                (insert (car selectrum--refined-candidates)))
            (selectrum-select-current-candidate)))
         ;; case 1
         ((not (string= common ""))
          (progn
            (delete-region (gatsby:selectrum--remove-until-slash
                            (minibuffer-prompt-end) 1)
                           (point-max))
            (insert common)))))))

  (defun gatsby:open-recent-file ()
    "Do not sort my recent file list."
    (interactive)
    (find-file (completing-read "Recent file: "
                                (mapcar #'abbreviate-file-name recentf-list)
                                nil t)))

  :custom
  (selectrum-fix-minibuffer-height t)

  :general
  (:keymaps 'selectrum-minibuffer-map
   "M-j" 'gatsby:selectrum-next-candidate-cycle
   "M-k" 'gatsby:selectrum-previous-candidate-cycle
   "<backspace>" 'gatsby:selectrum-better-backspace
   "<tab>" 'gatsby:selectrum-unified-tab)

  (:keymaps '(motion normal visual emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "oo" 'find-file
   "or" 'gatsby:open-recent-file
   "ob" 'switch-to-buffer
   "om" (lambda () (interactive)
          (switch-to-buffer-other-window (get-buffer-create "*Messages*")))))

(use-package selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode 1))

(use-package eldoc-box
  :hook ((text-mode prog-mode) . eldoc-box-hover-mode)
  :config
  (defun gatsby:eldoc-box--get-frame (buffer)
    "Overriding `eldoc-box--get-frame'.

The original function creates a visible frame at the bottom right corner of the screen, which is super annoying. This function fix that."
    (if eldoc-box--inhibit-childframe
        ;; if inhibit display, do nothing
        eldoc-box--frame
      (let* ((after-make-frame-functions nil)
             (before-make-frame-hook nil)
             (parameter (append eldoc-box-frame-parameters
                                `((default-minibuffer-frame . ,(selected-frame))
                                  (minibuffer . ,(minibuffer-window))
                                  (left-fringe . ,(frame-char-width)))))
             window frame
             (main-frame (selected-frame)))
        (if (and eldoc-box--frame (frame-live-p eldoc-box--frame))
            (progn (setq frame eldoc-box--frame)
                   (setq window (frame-selected-window frame))
                   ;; in case the main frame changed
                   (set-frame-parameter frame 'parent-frame main-frame))
          (setq window (display-buffer-in-child-frame
                        buffer
                        `((child-frame-parameters . ,parameter))))
          (setq frame (make-frame parameter))
          (with-selected-frame frame
            (delete-other-windows)
            (switch-to-buffer buffer)
            (setq window (get-buffer-window))))

        (set-face-attribute 'fringe frame :background nil :inherit 'eldoc-box-body)
        (set-window-dedicated-p window t)
        (redirect-frame-focus frame (frame-parent frame))
        (set-face-attribute 'internal-border frame :inherit 'eldoc-box-border)
        ;; set size
        (eldoc-box--update-childframe-geometry frame window)
        (setq eldoc-box--frame frame)
        (run-hook-with-args 'eldoc-box-frame-hook main-frame)
        (make-frame-visible frame))))
  (advice-add #'eldoc-box--get-frame :override #'gatsby:eldoc-box--get-frame)

  (defun gatsby:eldoc-box--position (width height)
    "Display `eldoc-box' in the bottom right corner of the `selected-window'."
    (let* ((edge (window-inside-pixel-edges))
           (y (- (nth 3 edge) 5 height))
           (x (- (nth 2 edge) 2 width)))
      (cons x y)))

  (defun gatsby:eldoc-box--update-parent-frame (frame)
    "Update frame parameters of `eldoc-box--frame' when it is called.  Run with `eldoc-box-frame-hook'."
    (set-frame-parameter eldoc-box--frame 'parent-frame frame))
  (add-hook 'eldoc-box-frame-hook #'gatsby:eldoc-box--update-parent-frame)

  (setq eldoc-box-position-function #'gatsby:eldoc-box--position
        eldoc-box-cleanup-interval 0.5)
  (setf (alist-get 'internal-border-width eldoc-box-frame-parameters) 2))

(use-package embark
  :config
  (defun embark-selectrum-candidates+ ()
    (when selectrum-active-p
      (selectrum-get-current-candidates
       ;; Pass relative file names for dired.
       minibuffer-completing-file-name)))

  (defun embark-selectrum-input-getter+ ()
    (when selectrum-active-p
      (let ((input (selectrum-get-current-input)))
        (if minibuffer-completing-file-name
            ;; Only get the input used for matching.
            (file-name-nondirectory input)
          input))))

  (add-hook 'embark-target-finders 'selectrum-get-current-candidate)
  (add-hook 'embark-candidate-collectors 'embark-selectrum-candidates+)

  ;; No unnecessary computation delay after injection.
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)
  (add-hook 'embark-input-getters 'embark-selectrum-input-getter+)

  :general
  (:keymaps '(insert emacs visual normal motion)
   "<C-return>" 'embark-act-noexit)

  (:keymaps 'minibuffer-local-map
   "<C-return>" 'embark-act))

(provide 'gatsby:minibuffer)
;;; gatsby:minibuffer.el ends here
