;;; bc-theme.el --- setting themes -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; theme
(use-package gruvbox-theme
  :init
  (setq custom-safe-themes t)
  (setq-default left-fringe-width 8)

  :config
  (load-theme 'gruvbox-dark-hard t)

  (set-face-attribute
   'default
   nil
   :family "Monospace"
   :height 150
   :weight 'Light
   :width 'normal)

  (set-face-attribute
   'minibuffer-prompt
   nil
   :weight 'normal)

  (set-face-attribute
   'line-number
   nil
   :background (face-background 'default)))

;; mode line
(use-package telephone-line
  :init

  (set-face-attribute
   'mode-line-buffer-id
   nil
   :weight 'normal)

  (telephone-line-defsegment* bc-theme-pctg-buffer-position ()
    (concat "LN " (format "%d/%d"
                          (1+ (count-lines 1 (point)))
                          (1+ (count-lines (point-min) (point-max))))))
  (setq telephone-line-lhs
        '((evil     . (telephone-line-evil-tag-segment))
          (accent   . (telephone-line-vc-segment
                       telephone-line-erc-modified-channels-segment))
          (nil      . (telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil      . (telephone-line-misc-info-segment))
          (accent   . (telephone-line-major-mode-segment))
          (evil     . (bc-theme-pctg-buffer-position))))
  :config
  (telephone-line-mode 1))

;; indentation guide
(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (LaTeX-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\┆
        highlight-indent-guides-responsive 'stack))

;; line numbers
(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  (org-mode . display-line-numbers-mode)
  (LaTeX-mode . display-line-numbers-mode)
  :config
  (setq-default
   display-line-numbers-type 'visual
   display-line-numbers-current-absolute t
   display-line-numbers-width 3
   display-line-numbers-widen nil))

;; display time
(use-package time
  :config
  (setq display-time-load-average-threshold 2.0
        display-time-24hr-format t)
  (display-time-mode))

;; display battery level
(use-package battery
  :config
  (setq battery-mode-line-limit 30)
  (display-battery-mode))

;; highlight keywords
(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . "#FB4934")
          ("FIXME"  . "#FB4934")
          ("WONT FIX" . "#FB4934")
          ("REVIEW"   . "#FABD2F")
          ("NOTE"   . "#FABD2F")
          ("HACK"   . "#FABD2F")
          ("\\?\\?\\?+" . "#cc9393")))
  (global-hl-todo-mode))

;; setup font
;; FIXME: alignment for ligatures is off
;; taken from https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;; (defconst fira-code-font-lock-keywords-alist
;;   (mapcar (lambda (regex-char-pair)
;;             `(,(car regex-char-pair)
;;               (0 (prog1 ()
;;                    (compose-region
;;                     (match-beginning 1)
;;                     (match-end 1)
;;                     ;; The first argument to concat is a string containing a literal tab
;;                     ,(concat "   " (list (decode-char 'ucs (cadr regex-char-pair)))))))))
;;           '(("\\(www\\)"                   #Xe100)
;;             ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
;;             ("\\(\\*\\*\\*\\)"             #Xe102)
;;             ("\\(\\*\\*/\\)"               #Xe103)
;;             ("\\(\\*>\\)"                  #Xe104)
;;             ("[^*]\\(\\*/\\)"              #Xe105)
;;             ("\\(\\\\\\\\\\)"              #Xe106)
;;             ("\\(\\\\\\\\\\\\\\)"          #Xe107)
;;             ("\\({-\\)"                    #Xe108)
;;             ("\\(\\[\\]\\)"                #Xe109)
;;             ("\\(::\\)"                    #Xe10a)
;;             ("\\(:::\\)"                   #Xe10b)
;;             ("[^=]\\(:=\\)"                #Xe10c)
;;             ("\\(!!\\)"                    #Xe10d)
;;             ("\\(!=\\)"                    #Xe10e)
;;             ("\\(!==\\)"                   #Xe10f)
;;             ("\\(-}\\)"                    #Xe110)
;;             ("\\(--\\)"                    #Xe111)
;;             ("\\(---\\)"                   #Xe112)
;;             ("\\(-->\\)"                   #Xe113)
;;             ("[^-]\\(->\\)"                #Xe114)
;;             ("\\(->>\\)"                   #Xe115)
;;             ("\\(-<\\)"                    #Xe116)
;;             ("\\(-<<\\)"                   #Xe117)
;;             ("\\(-~\\)"                    #Xe118)
;;             ("\\(#{\\)"                    #Xe119)
;;             ("\\(#\\[\\)"                  #Xe11a)
;;             ("\\(##\\)"                    #Xe11b)
;;             ("\\(###\\)"                   #Xe11c)
;;             ("\\(####\\)"                  #Xe11d)
;;             ("\\(#(\\)"                    #Xe11e)
;;             ("\\(#\\?\\)"                  #Xe11f)
;;             ("\\(#_\\)"                    #Xe120)
;;             ("\\(#_(\\)"                   #Xe121)
;;             ("\\(\\.-\\)"                  #Xe122)
;;             ("\\(\\.=\\)"                  #Xe123)
;;             ("\\(\\.\\.\\)"                #Xe124)
;;             ("\\(\\.\\.<\\)"               #Xe125)
;;             ("\\(\\.\\.\\.\\)"             #Xe126)
;;             ("\\(\\?=\\)"                  #Xe127)
;;             ("\\(\\?\\?\\)"                #Xe128)
;;             ("\\(;;\\)"                    #Xe129)
;;             ("\\(/\\*\\)"                  #Xe12a)
;;             ("\\(/\\*\\*\\)"               #Xe12b)
;;             ("\\(/=\\)"                    #Xe12c)
;;             ("\\(/==\\)"                   #Xe12d)
;;             ("\\(/>\\)"                    #Xe12e)
;;             ("\\(//\\)"                    #Xe12f)
;;             ("\\(///\\)"                   #Xe130)
;;             ("\\(&&\\)"                    #Xe131)
;;             ("\\(||\\)"                    #Xe132)
;;             ("\\(||=\\)"                   #Xe133)
;;             ("[^|]\\(|=\\)"                #Xe134)
;;             ("\\(|>\\)"                    #Xe135)
;;             ("\\(\\^=\\)"                  #Xe136)
;;             ("\\(\\$>\\)"                  #Xe137)
;;             ("\\(\\+\\+\\)"                #Xe138)
;;             ("\\(\\+\\+\\+\\)"             #Xe139)
;;             ("\\(\\+>\\)"                  #Xe13a)
;;             ("\\(=:=\\)"                   #Xe13b)
;;             ("[^!/]\\(==\\)[^>]"           #Xe13c)
;;             ("\\(===\\)"                   #Xe13d)
;;             ("\\(==>\\)"                   #Xe13e)
;;             ("[^=]\\(=>\\)"                #Xe13f)
;;             ("\\(=>>\\)"                   #Xe140)
;;             ("\\(<=\\)"                    #Xe141)
;;             ("\\(=<<\\)"                   #Xe142)
;;             ("\\(=/=\\)"                   #Xe143)
;;             ("\\(>-\\)"                    #Xe144)
;;             ("\\(>=\\)"                    #Xe145)
;;             ("\\(>=>\\)"                   #Xe146)
;;             ("[^-=]\\(>>\\)"               #Xe147)
;;             ("\\(>>-\\)"                   #Xe148)
;;             ("\\(>>=\\)"                   #Xe149)
;;             ("\\(>>>\\)"                   #Xe14a)
;;             ("\\(<\\*\\)"                  #Xe14b)
;;             ("\\(<\\*>\\)"                 #Xe14c)
;;             ("\\(<|\\)"                    #Xe14d)
;;             ("\\(<|>\\)"                   #Xe14e)
;;             ("\\(<\\$\\)"                  #Xe14f)
;;             ("\\(<\\$>\\)"                 #Xe150)
;;             ("\\(<!--\\)"                  #Xe151)
;;             ("\\(<-\\)"                    #Xe152)
;;             ("\\(<--\\)"                   #Xe153)
;;             ("\\(<->\\)"                   #Xe154)
;;             ("\\(<\\+\\)"                  #Xe155)
;;             ("\\(<\\+>\\)"                 #Xe156)
;;             ("\\(<=\\)"                    #Xe157)
;;             ("\\(<==\\)"                   #Xe158)
;;             ("\\(<=>\\)"                   #Xe159)
;;             ("\\(<=<\\)"                   #Xe15a)
;;             ("\\(<>\\)"                    #Xe15b)
;;             ("[^-=]\\(<<\\)"               #Xe15c)
;;             ("\\(<<-\\)"                   #Xe15d)
;;             ("\\(<<=\\)"                   #Xe15e)
;;             ("\\(<<<\\)"                   #Xe15f)
;;             ("\\(<~\\)"                    #Xe160)
;;             ("\\(<~~\\)"                   #Xe161)
;;             ("\\(</\\)"                    #Xe162)
;;             ("\\(</>\\)"                   #Xe163)
;;             ("\\(~@\\)"                    #Xe164)
;;             ("\\(~-\\)"                    #Xe165)
;;             ("\\(~=\\)"                    #Xe166)
;;             ("\\(~>\\)"                    #Xe167)
;;             ("[^<]\\(~~\\)"                #Xe168)
;;             ("\\(~~>\\)"                   #Xe169)
;;             ("\\(%%\\)"                    #Xe16a)
;;             ("[0\[]\\(x\\)"                #Xe16b)
;;             ("[^:=]\\(:\\)[^:=]"           #Xe16c)
;;             ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
;;             ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

;; (defun bc-allow-ligatures ()
;;   (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

;; (add-hook 'prog-mode-hook #'bc-allow-ligatures)

(defun bc-fontsize-up (&optional size)
  "Increase the font size in the current frame by SIZE.  If SIZE is nil, default to 5."
  (interactive)
  (let* ((current-size (plist-get (custom-face-attributes-get 'default nil) :height))
         (new-size (+ (or size 5) current-size)))
    (set-face-attribute
     'default (selected-frame)
     :height new-size)))

(defun bc-fontsize-down (&optional size)
  "Decrease the font size in the current frame by SIZE.  If SIZE is nil, default to 5."
  (interactive)
  (let* ((current-size (plist-get (custom-face-attributes-get 'default nil) :height))
         (new-size (- current-size (or size 5))))
    (set-face-attribute
     'default (selected-frame)
     :height new-size)))

(defun bc-fontsize-reset ()
  "Reset the current frame font size."
  (interactive)
  (set-face-attribute
   'default (selected-frame) :height 150))

(provide 'bc-theme)
;;; bc-theme.el ends here
