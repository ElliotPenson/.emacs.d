;;; emacs.el --- Emacs configuration file
;;
;; Author: Elliot Penson
;;
;;; Code:

(package-initialize)

;; Interface ---------------------------------------------------------

;; Display the current column
(setq column-number-mode t)

(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Fixedsys")
  (set-frame-font "Fixedsys" nil t)
  (setq default-directory "C:/Users/epenson/Documents/"))

(add-to-list 'default-frame-alist '(height . 75))
(add-to-list 'default-frame-alist '(width . 100))

(setq confirm-kill-emacs 'y-or-n-p)

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

;; Misc Settings -----------------------------------------------------

(setq user-full-name "Elliot Penson"
      user-mail-address "elliotpenson@gmail.com")

(global-set-key (kbd "C-x g") 'webjump)

;; Other Configuration Files -----------------------------------------

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/spell-checking.el")
(load "~/.emacs.d/org-mode.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs.el ends here
