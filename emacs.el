;;;; Emacs configuration file 
;;;; by Elliot Penson

;;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme (if (display-graphic-p)
                'base16-railscasts
              'base16-default)
            t)

;;; Line Numbers
(global-linum-mode t)
(setq linum-format "%d ")

(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Fixedsys")
  (set-frame-font "Fixedsys" nil t)
  (setq default-directory "C:/Users/epenson/Documents/"))

(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta)
  (setq w32-lwindow-modifier 'meta))

(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 83))

(setq fill-column 80)

(push (cons "\\.cl$" 'lisp-mode)
      auto-mode-alist)

;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(defvar my-packages '(better-defaults
                      paredit
                      ace-jump-mode
                      projectile
                      clojure-mode
                      swift-mode
                      slime
                      cider))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;;; SLIME
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;; Org-Mode
(require 'org)
(require 'ob-clojure)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-src-fontify-natively t) ; turn on syntax highlighting
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;; Paredit
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural-editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hood #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)

;;; Paredit in SLIME
(add-hook 'slime-repl-mode-hood (lambda () (paredit-mode +1)))
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook #'override-slime-repl-bindings-with-paredit)

;;; Ace Jump Mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;; Misc Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Elliot Penson"
      user-mail-address "elliotpenson@gmail.com")
