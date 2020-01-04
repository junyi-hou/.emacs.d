;;; bc-pyim.el --- py input method support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package pyim
  :demand t
  :init
  ;; enable pinyin search in ivy-read
  (with-eval-after-load 'ivy
    (progn
      (defun bc-pyim--ivy-cregexp (str)
        "taken from https://emacs-china.org/t/ivy-read/2432/5"
        (concat
         (ivy--regex-plus str) "\\|" (pyim-cregexp-build str)))

      (setq ivy-re-builders-alist '((t . bc-pyim--ivy-cregexp)))))

  (setq default-input-method "pyim"
        pyim-title "拼音"
        default-scheme 'quanpin
        pyim-page-tooltip 'posframe
        pyim-page-length 5)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-i 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  :general
  (:keymaps '(insert emacs)
   "M-i" 'pyim-convert-string-at-point
   "C-\\" 'toggle-input-method)

  (:keymaps 'pyim-mode-map
   :states '(insert emacs)
   "M-k" 'pyim-page-previous-page
   "M-j" 'pyim-page-next-page)

  (:keymaps '(normal visual motion)
   "C-\\" nil))

(use-package pyim-basedict
  :after pyim
  :config
  (pyim-basedict-enable))

(provide 'bc-pyim)
;;; bc-pyim.el ends here
