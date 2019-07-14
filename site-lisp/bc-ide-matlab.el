;;; bc-ide-matlab.el --- ide for matlab .m file -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package octave
  :defer t
  :commands octave-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.m" . octave-mode)))


(provide 'bc-ide-matlab)
;;; bc-ide-matlab.el ends here
