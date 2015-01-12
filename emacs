;;;; emacs config file
;;;; Elliot Penson

;;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'base16-default t)

;;; Backup Files
(setq make-backup-files nil)

;;; Line Numbers
(global-linum-mode t)
(setq linum-format "%d ")

;(setq find-file-visit-truename t)

