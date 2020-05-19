;;; gatsby:dired.el --- settings for dired mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby:core)

(use-package dired
  :straight (:type built-in)
  :defer t
  :config
  (evil-set-initial-state 'dired-mode 'motion)
  (setq dired-listing-switches "-alh")

  ;; functions
  (defun gatsby:dired-toggle-hide ()
    "Toggle whether to show hidden files."
    (interactive)
    (let* ((switches dired-actual-switches)
           (new-switches (if (string-match "a" dired-actual-switches)
                             (replace-regexp-in-string "a" "" dired-actual-switches)
                           (concat dired-actual-switches "a"))))
      (setq-local dired-actual-switches new-switches)
      (revert-buffer)))

  :general
  (:keymaps '(motion normal visual emacs insert)
   :prefix "SPC"
   :non-normal-prefix "s-SPC"
   "od" (lambda () (interactive)
          (dired default-directory)))

  (:keymaps 'dired-mode-map
   :states '(motion visual normal)

   ;; sort
   "s" 'dired-sort-toggle-or-edit

   ;; movement
   "j" 'dired-next-line
   "k" 'dired-previous-line
   "J" (lambda () (interactive) (dired-next-line 3))
   "K" (lambda () (interactive) (dired-previous-line 3))
   ">" 'dired-next-subdir
   "<" 'dired-prev-subdir
   "<backspace>" 'dired-up-directory

   ;; files
   "<return>" 'dired-find-file
   "<C-return>" 'dired-find-file-other-window
   "f" 'find-file
   "F" 'dired-create-directory
   "t" 'dired-show-file-type
   "y" 'dired-copy-filename-as-kill

   ;; marks
   "m" 'dired-mark
   "u" 'dired-unmark
   "U" 'dired-unmark-all-marks
   "C" 'dired-do-copy
   "D" 'dired-do-delete
   "R" 'dired-do-rename
   "M" 'dired-do-chmod
   "O" 'dired-do-chown
   "Z" 'dired-compress
   "M-j" 'dired-next-marked-file
   "M-k" 'dired-prev-marked-file)

  (:keymaps 'dired-mode-map
   :states 'motion
   :prefix "SPC"
   "r" 'revert-buffer
   "h" 'gatsby:dired-toggle-hide))

(use-package dired-rainbow
  :after dired
  :config
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*"))

(use-package dired-collapse :hook (dired-mode . dired-collapse-mode))

(provide 'gatsby:dired)
;;; gatsby:dired.el ends here
