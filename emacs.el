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

;;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;; SLIME
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;(setq find-file-visit-truename t)
