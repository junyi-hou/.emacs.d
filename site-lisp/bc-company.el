;;; bc-company.el --- init company-mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package yasnippet
  :commands (yas-minor-mode yas-reload-all)
  :hook
  (company-mode . yas-reload-all)
  (company-mode . yas-minor-mode)
  :general
  (:keymaps 'yas-keymap
   "<tab>" nil
   "TAB" nil
   "M-n" 'yas-next-field
   "M-p" 'yas-prev-field)

  ;; kill M-j/M-k so they won't show up when snippets are active
  (:keymaps 'global-map
   "M-j" nil
   "M-k" nil))

(use-package company
  :hook
  (prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  (jupyter-repl-mode . company-mode)
  (eshell-mode . company-mode)
  (org-mode . company-mode)

  :config
  (setq company-idle-delay nil
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-show-numbers t
        company-tooltip-limit 5
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-backends '((company-capf
                            company-yasnippet
                            company-files)
                           (company-dabbrev-code
                            company-dabbrev
                            company-abbrev)))

  ;; function
  (defun bc-company-unified-tab ()
    "Use tab for both company and indentation.

In insert mode, first try `company-manual-begin'.  If there is no completion available at point, indent the current line by `tab-width' length."
    (interactive)
    (if (looking-back "^[ \t]*" (line-beginning-position))
        (dotimes (n tab-width) (insert " "))
      (yas-expand)
      (company-manual-begin)
      ;; manually call `company-manual-begin' will set
      ;; `company-minimum-prefix-length' to 0, which means that the snippets
      ;; will always get included. To fix this add a condition that if
      ;; all candidates are snippets, cancel auto completion and indent region.
      (if (null (seq-every-p
                 (lambda (candidate)
                   (member candidate (yas-active-keys)))
                 company-candidates))
          (if (or company-selection-changed
                  (memq last-command '(company-complete-common
                                       bc-lsp-unified-tab)))
              (call-interactively 'company-complete-selection)
            (call-interactively 'company-complete-common)
            (when company-candidates
              (setq this-command 'company-complete-common)))
        (company-cancel)
        (save-excursion
          (indent-region (line-beginning-position) (line-end-position))))))

  :general
  (:keymaps 'company-active-map
   "<tab>" 'bc-company-unified-tab
   "M-j" 'company-select-next
   "M-k" 'company-select-previous-or-abort
   "C-g" 'company-abort))

(use-package company-prescient
  :hook
  (company-mode . company-prescient-mode))

(use-package company-posframe
  :after company
  :hook (company-mode . company-posframe-mode)
  :init
  (setq company-posframe-show-metadata nil
        company-posframe-show-indicator nil
        company-posframe-quickhelp-delay nil)

  (defconst bc-company-posframe-quickhelp-height 10
    "Height of the company-posframe-quickhelp frame.")

  ;; place quickhelp frame side-by-side with the candidate frame
  (defun bc-company-posframe-quickhelp-show ()
    (company-posframe-quickhelp-cancel-timer)
    (while-no-input
      (let* ((selected (nth company-selection company-candidates))
             (doc (let ((inhibit-message t))
                    (company-posframe-quickhelp-doc selected)))
             (height bc-company-posframe-quickhelp-height)
             (header-line
              (substitute-command-keys
               (concat
                "## "
                "\\<company-posframe-active-map>\\[company-posframe-quickhelp-toggle]:Show/Hide  "
                "\\<company-posframe-active-map>\\[company-posframe-quickhelp-scroll-up]:Scroll-Up  "
                "\\<company-posframe-active-map>\\[company-posframe-quickhelp-scroll-down]:Scroll-Down "
                "##"))))
        (when doc
          (with-current-buffer (get-buffer-create company-posframe-quickhelp-buffer)
            (setq-local header-line-format header-line))
          (apply #'posframe-show
                 company-posframe-quickhelp-buffer
                 :string (propertize doc 'face 'company-posframe-quickhelp)
                 :width (let ((n (apply #'max (mapcar #'string-width
                                                      (split-string doc "\n+")))))
                          (max (length header-line) (min fill-column n)))
                 :min-width (length header-line)
                 :min-height height
                 :height height
                 :respect-header-line t
                 ;; When first show quickhelp's posframe, it seem using wrong height,
                 ;; maybe header-line's reason, just refresh again, ugly but useful :-).
                 :refresh 0.5
                 :background-color (face-attribute 'company-posframe-quickhelp :background nil t)
                 :foreground-color (face-attribute 'company-posframe-quickhelp :foreground nil t)
                 :position
                 (with-current-buffer company-posframe-buffer
                   (let ((pos posframe--last-posframe-pixel-position))
                     (cons (+ (car pos) (frame-pixel-width posframe--frame))
                           (cdr pos))))
                 company-posframe-quickhelp-show-params)))))

  (advice-add #'company-posframe-quickhelp-show
              :override
              #'bc-company-posframe-quickhelp-show)

  :general
  (:keymaps 'company-active-map
   "M-n" 'company-posframe-quickhelp-toggle
   "J" 'company-posframe-quickhelp-scroll-up
   "K" 'company-posframe-quickhelp-scroll-down))

(provide 'bc-company)
;;; bc-company.el ends here
