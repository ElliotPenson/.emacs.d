;;; go-mode.el --- Go configuration
;;
;;; Commentary:
;;
;; - This file checks that required Go packages are installed.
;; - Flycheck (see packages.el) lints Go by default.
;; - For now I'm only using Go Guru for identifier highlighting.
;;
;;; Code:

(require 'package)

(defvar penson/go-path "~/go/")

(defvar penson/go-packages
  '("github.com/dougm/goflymake"
    "github.com/rogpeppe/godef"
    "golang.org/x/lint/golint"
    "golang.org/x/tools/cmd/godoc"
    "golang.org/x/tools/cmd/goimports"
    "golang.org/x/tools/cmd/guru"))

(use-package go-mode
  :ensure t
  :defer t
  :bind (("C-c C-h" . 'godoc-at-point)
         ("C-c C-j" . 'godef-jump)
         ("C-c C-b" . 'pop-tag-mark))
  :config
  (mapc #'require-go penson/go-packages)
  (add-hook 'go-mode-hook #'go-hook))

(use-package go-guru
  :ensure t
  :defer t)

(defun go-hook ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq indent-tabs-mode 1)
  (setq tab-width 4)
  (go-guru-hl-identifier-mode))

(defun require-go (module)
  (let ((path (concat penson/go-path "src/" module)))
    (unless (file-directory-p path)
      (error (format "Go package not installed. Run 'go get -u %s'." module)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; go-mode.el ends here
