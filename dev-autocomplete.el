;;; dev-autocomplete.el -- autocomplete settings using company and yasnippets


(use-package yasnippet
  :commands
  (yas-minor-mode yas-reload-all yas-expand yas-next-field yas-prev-field)
  :general
  (:keymaps 'yas-minor-mode-map
            "<tab>" nil
            "TAB" nil
            "M-f" 'yas-expand
            "M-j" 'yas-next-field
            "M-k" 'yas-prev-field)
  :config
  (defun jy/load-yas ()
    (interactive)
    (yas-minor-mode)
    (yas-reload-all))
  (add-hook 'company-mode-hook #'jy/load-yas))

(use-package company
  :commands
  (company-mode company-complete company-complete-common company-manual-begin company-grab-line)
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
        '((company-files company-keywords company-capf company-yasnippets)
          (company-abbrev company-dabbrev))
        company-frontends ; set frontends
        '(company-pseudo-tooltip-frontend)))

(use-package company-quickhelp
  :after company
  :config
  (setq company-quickhelp-delay 1)
  (add-hook 'company-mode-hook #'company-quickhelp-mode)
  )

(provide 'dev-autocomplete)
;;; dev-autocomplete.el ends here
