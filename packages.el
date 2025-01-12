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

(use-package ascii-art-to-unicode
  :ensure t)

(use-package avy
  :ensure t
  :bind (("C-'" . 'avy-goto-char)
         ("C-:" . 'avy-goto-char-2))
  :config
  (setq avy-all-windows nil)
  ;; Unset conflicting binding in Org mode
  (add-hook 'org-mode-hook
            (lambda ()
              (local-unset-key (kbd "C-'")))))

(use-package cider
  :ensure t
  :commands (cider cider-connect cider-jack-in))

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package diminish
  :ensure t)

(use-package docker-compose-mode
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode)
      :config
      (setq doom-modeline-python-executable "python3")
      (setq doom-modeline-icon nil))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package dumb-jump
  :ensure t
  :config
  ;; Be sure to install ag first!
  ;;     > brew install the_silver_searcher
  (dumb-jump-mode))

(use-package elpy
  :ensure t
  :defer 2
  :config
  ;; Be sure to install required packages first!
  ;;     > pip install jedi flake8 autopep8 yap
  (elpy-enable)
  (elpy-set-test-runner 'elpy-test-pytest-runner))

(use-package esup
  :ensure t
  :defer t
  :config
  (setq esup-user-init-file "~/.emacs.d/emacs.el"))

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind ("C-=" . 'er/expand-region))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
  :config
  (require 'helm-config)
  (helm-mode 1))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package indium
  ;; Be sure to also install the Indium server.
  ;;     > npm install -g indium
  :ensure t
  :defer t
  :diminish indium-interaction-mode
  :bind (("C-c C-z" . 'indium-launch)
         ("C-c C-c" . 'indium-eval-region))
  :config
  (add-hook 'js2-mode-hook #'indium-interaction-mode))

(use-package js2-mode
  :ensure t
  :defer t
  :commands js2-mode)

(use-package json-mode
  :ensure t
  :defer t
  :commands json-mode)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package magit-todos
  :ensure t
  :hook (magit-mode . magit-todos-mode))

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package org
  :ensure org-plus-contrib
  :commands org-publish
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
  :defer t
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

(use-package parrot
  :ensure t)

(use-package pip-requirements
  :ensure t
  :defer t
  :config
  (add-hook 'pip-requirements-mode-hook
            #'pip-requirements-auto-complete-setup))

(use-package prettier-js
  ;; Be sure to install prettier!
  ;;   > npm install --global prettier
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package python-mode
  :ensure t
  :defer t)

(use-package restclient
  :ensure t
  :defer t)

(use-package rjsx-mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" "\\.jsx\\'")
  :config
  (setq js-indent-level 2)
  (yas-activate-extra-mode 'js2-mode))

(use-package saveplace
  :ensure nil
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package scss-mode
  :ensure t
  :defer t)

(use-package slime
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  (setq slime-net-coding-system 'utf-8-unix))

(use-package swift-mode
  :ensure t
  :defer t)

(use-package terraform-mode
  :ensure t
  :defer t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (tide-hl-identifier-mode +1))

(use-package tide
  :ensure t
  :config
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)))))

(use-package typescript-mode
  :ensure t
  :defer t
  :config
  (setq typescript-indent-level 2))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html?\\'" "\\.css\\'" "\\.tsx\\'")
  :config
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packages.el ends here
