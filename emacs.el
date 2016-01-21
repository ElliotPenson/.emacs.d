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

;; Line Numbers
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

(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 88))

(setq fill-column 80)

(push (cons "\\.cl$" 'lisp-mode)
      auto-mode-alist)

;; Packages ----------------------------------------------------------

;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(defvar my-packages
  '(better-defaults
    projectile
    exec-path-from-shell
    ;; editor modes
    markdown-mode
    clojure-mode
    swift-mode
    python-mode
    ;; environments
    slime
    cider
    paredit
    ;; movement
    avy))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Org-Mode
(require 'org)
(require 'ob-clojure)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-src-fontify-natively t) ; turn on syntax highlighting
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Spell checking
(setq ispell-program-name
      "/usr/local/Cellar/ispell/3.4.00/bin/ispell")
(dolist (hook '(text-mode-hook
                org-mode-hook))
  ;; Spell check entire file
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                lisp-mode-hook
                python-mode-hook))
  ;; Spell check comments and strings
  (add-hook hook (lambda () (flyspell-prog-mode))))
(global-set-key (kbd "<f8>") 'ispell-word)

;; Python
(require 'python-mode)
(exec-path-from-shell-initialize)
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-force-py-shell-name-p t)
; don't split windows
(setq py-keep-windows-configuration t)

;; SLIME
(setq inferior-lisp-program
      (cond ((file-exists-p "/usr/bin/sbcl")
             "/usr/bin/sbcl")
            ((file-exists-p "/usr/local/bin/sbcl")
             "/usr/bin/local/sbcl")
            (t (error "Cannot find SBCL!"))))
(setq slime-contribs '(slime-fancy))

;; Paredit
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural-editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hood #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)

;; Paredit in SLIME
(add-hook 'slime-repl-mode-hood (lambda () (paredit-mode +1)))
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook #'override-slime-repl-bindings-with-paredit)

;; Avy
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)

;; Misc Settings -----------------------------------------------------

(setq user-full-name "Elliot Penson"
      user-mail-address "elliotpenson@gmail.com")

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(setq visible-bell nil)

(define-coding-system-alias 'UTF-8 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs.el ends here
