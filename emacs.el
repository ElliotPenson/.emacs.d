;;; emacs.el --- Emacs configuration file
;;
;; Author: Elliot Penson
;;
;;; Code:

;; Interface ---------------------------------------------------------

;; Themes
(cond ((>= emacs-major-version 24)
       (add-to-list 'custom-theme-load-path
                    "~/.emacs.d/themes")
       (load-theme (if (display-graphic-p)
                       'base16-railscasts
                     'base16-default)
                   t))
      (t ;; (< emacs-major-version 24)
       (load-file "~/.emacs.d/themes/base16-railscasts-theme.el")))

;; Display the current column
(setq column-number-mode t)

(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Fixedsys")
  (set-frame-font "Fixedsys" nil t)
  (setq default-directory "C:/Users/epenson/Documents/"))

(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 88))

(setq visible-bell nil)

;; Keys/Navigation  --------------------------------------------------

(cond ((eq system-type 'darwin)
       (setq mac-command-modifier 'meta)
       (setq mac-option-modifier 'super))
      (t (setq w32-lwindow-modifier 'meta)))

;; Editing -----------------------------------------------------------

(setq-default fill-column 80)

(define-coding-system-alias 'UTF-8 'utf-8)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (if (looking-at "[0123456789]+")
      (let ((number-at-point (string-to-number (match-string 0))))
        (replace-match (number-to-string (1+ number-at-point))))
    (error "No number at point")))

(push (cons "\\.cl$" 'lisp-mode)
      auto-mode-alist)

;; Misc Settings -----------------------------------------------------

(setq user-full-name "Elliot Penson"
      user-mail-address "elliotpenson@gmail.com")

(global-set-key (kbd "C-x g") 'webjump)

;; Other Configuration Files -----------------------------------------

(load "~/.emacs.d/packages.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs.el ends here
