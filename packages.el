;;; packages.el --- Emacs package configuration
;; Author: Elliot Penson
;;
;;; Commentary:
;; Emacs package configuration
;;
;;
;;; Code:

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package avy
  :ensure t
  :bind (("C-'" . 'avy-goto-char)
         ("C-:" . 'avy-goto-char-2))
  :config
  ;; Unset conflicting binding in Org mode
  (add-hook 'org-mode-hook
            (lambda ()
              (local-unset-key (kbd "C-'")))))

(use-package better-defaults
  :ensure t)

(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package elpy
  :ensure python-mode
  :init
  (exec-path-from-shell-initialize)
  :config
  ;; Be sure to install required packages first!
  ;;     > pip install jedi flake8 autopep8 yap
  (elpy-enable))

(use-package exec-path-from-shell
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (let*
      ((go-path "~/go/")
       (goflymake-package "github.com/dougm/goflymake")
       (goflymake-path (concat go-path "src/" goflymake-package)))
    (if (file-directory-p goflymake-path)
        (add-to-list 'load-path goflymake-path)
      (error (format "goflymake is not installed. Please run `go get -u %s`."
                     goflymake-package))))
  (require 'go-flymake)
  (require 'go-flycheck))

(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js$"
  :commands js2-mode)

(use-package json-mode
  :ensure t
  :defer t
  :commands json-mode)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package org
  :ensure org-plus-contrib
  :defer 1
  :bind (("\C-cl" . 'org-store-link)
         ("\C-ca" . 'org-agenda))
  :config
  (require 'org-drill)
  (require 'ob-clojure)
  (setq org-log-done t)
  ;; Syntax highlighting.
  (setq org-src-fontify-natively t)
  ;; Word wrap.
  (add-hook 'org-mode-hook 'turn-on-auto-fill))

(use-package ox-twbs
  :ensure t
  :config
  (require 'ox-publish))

(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit" t)
  (dolist (hook '(emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  scheme-mode-hook))
    (add-hook hook #'enable-paredit-mode))

  ;; Paredit in SLIME
  (add-hook 'slime-repl-mode-hook
            (lambda () (paredit-mode +1)))
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook
            #'override-slime-repl-bindings-with-paredit))

(use-package projectile
  :ensure t)

(use-package python-mode
  :ensure t
  :defer t)

(use-package restclient
  :ensure t
  :defer t)

(use-package rjsx-mode
  :ensure t
  :defer t)

(use-package scss-mode
  :ensure t
  :defer t)

(use-package slime
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program
        (cond ((file-exists-p "/usr/bin/sbcl")
               "/usr/bin/sbcl")
              ((file-exists-p "/usr/local/bin/sbcl")
               "/usr/local/bin/sbcl")
              (t (error "Cannot find SBCL!"))))
  (setq slime-contribs '(slime-fancy))
  (setq slime-net-coding-system 'utf-8-unix))

(use-package swift-mode
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer t
  :mode "\\.html?\\'")

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packages.el ends here
