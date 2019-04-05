;;; dev-autocomplete.el -- autocomplete settings using company and yasnippet -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package yasnippet
  :commands
  (yas-minor-mode yas-reload-all yas-expand yas-next-field yas-prev-field)
  :general
  (:keymaps 'yas-minor-mode-map
            "<tab>" nil
            "TAB" nil
            "M-f" 'yas-expand
            "M-j" 'yas-next-field
            "M-k" 'yas-prev-field))

(use-package company
  :commands
  (company-mode company-complete company-complete-common)
  :general
  (:keymaps 'company-active-map
            "<tab>" 'company-complete-common
            "TAB" 'company-complete-common
            "M-j" 'company-select-next
            "M-k" 'company-select-previous-or-abort)

  :config
  (setq company-idle-delay 0
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-backends  ; set default backends
        '(company-files company-capf company-yasnippet)))

(use-package company-posframe
  :after company
  :commands company-posframe-mode)

(use-package company-statistics :after company)

(defun dev-autocomplete--load-yas ()
  "Turn on `yas-minor-mode` and reload all snippets."
  (interactive)
  (yas-minor-mode 1))

(add-hook 'company-mode-hook #'company-posframe-mode)
(add-hook 'company-mode-hook #'dev-autocomplete--load-yas)

(provide 'dev-autocomplete)
;;; dev-autocomplete.el ends here
