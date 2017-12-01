;;; packages.el --- Emacs package configuration
;;
;; Author: Elliot Penson
;;
;;; Code:

;; MELPA  ----------------------------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(defvar my-packages
  '(better-defaults
    projectile
    exec-path-from-shell
    ox-twbs
    yasnippet
    ;; editor modes
    clojure-mode
    go-mode
    markdown-mode
    python-mode
    scss-mode
    swift-mode
    web-mode
    ;; environments
    slime
    cider
    paredit
    flycheck
    ;; movement
    avy))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; org-mode  ---------------------------------------------------------

(require 'org)

(require 'ob-clojure)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)

(setq org-src-fontify-natively t) ; turn on syntax highlighting

(add-hook 'org-mode-hook 'turn-on-auto-fill) ; word wrap

(setq org-publish-project-alist
      '(("org"
         :base-directory "~/org/"
         :publishing-directory "~/org/public_html/"
         :publishing-function org-twbs-publish-to-html
         :with-headline-numbers nil
         :with-sub-superscript nil)))

(defun my-org-publish-buffer ()
  "Export the current buffer's file and open in browser. Function taken from
   the ox-twbs manual (https://github.com/mars mining/ox-twbs)."
  (interactive)
  (save-buffer)
  (save-excursion (org-publish-current-file))
  (let* ((proj (org-publish-get-project-from-filename buffer-file-name))
         (proj-plist (cdr proj))
         (rel (file-relative-name buffer-file-name
                                  (plist-get proj-plist :base-directory)))
         (dest (plist-get proj-plist :publishing-directory)))
    (browse-url (concat "file://"
                        (file-name-as-directory (expand-file-name dest))
                        (file-name-sans-extension rel)
                        ".html"))))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "s-\\")
                           'my-org-publish-buffer)))

;; YASnippet  --------------------------------------------------------

(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; Spell Checking  ---------------------------------------------------

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

;; Python ------------------------------------------------------------

(require 'python-mode)

(exec-path-from-shell-initialize)

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-force-py-shell-name-p t)

(setq py-keep-windows-configuration t) ; don't split windows

;; SLIME -------------------------------------------------------------

(setq inferior-lisp-program
      (cond ((file-exists-p "/usr/bin/sbcl")
             "/usr/bin/sbcl")
            ((file-exists-p "/usr/local/bin/sbcl")
             "/usr/bin/local/sbcl")
            (t (error "Cannot find SBCL!"))))

(setq slime-contribs '(slime-fancy))

;; Paredit -----------------------------------------------------------

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural-editing of Lisp code."
  t)

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
          #'override-slime-repl-bindings-with-paredit)

;; Flycheck ----------------------------------------------------------

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Avy ---------------------------------------------------------------

(global-set-key (kbd "C-'") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-char-2)

;; Go ----------------------------------------------------------------

;; PRECONDITION: goflymake is installed (go get -u github.com/dougm/goflymake)

(let ((go-path "~/go/"))
  (add-to-list 'load-path (concat go-path "src/github.com/dougm/goflymake")))

(require 'go-flymake)
(require 'go-flycheck)

;; web-mode ----------------------------------------------------------

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packages.el ends here
