;;; emacs.el --- Emacs configuration file
;;
;; Author: Elliot Penson
;;
;;; Code:

(package-initialize)

;;; Interface --------------------------------------------------------

(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Fixedsys")
  (set-frame-font "Fixedsys" nil t)
  (setq default-directory "C:/Users/epenson/Documents/"))

(add-to-list 'default-frame-alist '(height . 75))
(add-to-list 'default-frame-alist '(width . 100))

(setq column-number-mode t
      confirm-kill-emacs 'y-or-n-p
      inhibit-startup-message t
      visible-bell nil)

(fset 'yes-or-no-p 'y-or-n-p)

(when (display-graphic-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

;;; Keys/Navigation  -------------------------------------------------

(cond ((eq system-type 'darwin)
       (setq mac-command-modifier 'meta)
       (setq mac-option-modifier 'super))
      (t (setq w32-lwindow-modifier 'meta)))

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Highlight matching parentheses.
(show-paren-mode 1)

;;; Editing ----------------------------------------------------------

(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups")))
      load-prefer-newer t
      mouse-yank-at-point t
      require-final-newline t)

(define-coding-system-alias 'UTF-8 'utf-8)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(autoload 'zap-up-to-char "misc"
  "Kill up to the given CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "M-/") 'hippie-expand)

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (if (looking-at "[0123456789]+")
      (let ((number-at-point (string-to-number (match-string 0))))
        (replace-match (number-to-string (1+ number-at-point))))
    (error "No number at point")))

(defun rename-current-buffer-file ()
  "Rename the current buffer and the file it is visiting. Taken from
   whattheemacsd.com."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(push (cons "\\.cl$" 'lisp-mode)
      auto-mode-alist)

;;; Misc Settings ----------------------------------------------------

(setq user-full-name "Elliot Penson"
      user-mail-address "elliotpenson@gmail.com")

(global-set-key (kbd "C-x g") 'webjump)

;;; Other Configuration Files ----------------------------------------

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/spell-checking.el")
(load "~/.emacs.d/org-mode.el")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs.el ends here
