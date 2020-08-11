;;; spell-checking.el --- Spell checker configuration
;; Author: Elliot Penson
;;
;;; Commentary:
;; Spell checker configuration
;;
;;
;;; Code:

(cond ((eq system-type 'darwin)
       (setq ispell-program-name
             "/usr/local/Cellar/ispell/3.4.00/bin/ispell"))
      ((eq system-type 'gnu/linux)
       (setq ispell-program-name
             "/usr/bin/ispell"))
      (t (error "Unknown system type '%s'" system-type)))

(dolist (hook '(text-mode-hook
                org-mode-hook))
  ;; Spell check entire file.
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                lisp-mode-hook
                python-mode-hook))
  ;; Spell check comments and strings.
  (add-hook hook (lambda () (flyspell-prog-mode))))

;; Prevent flyspell from using C-, and C-.
(eval-after-load "flyspell"
  '(progn (define-key flyspell-mode-map (kbd "C-,") nil)
          (define-key flyspell-mode-map (kbd "C-.") nil)))

(global-set-key (kbd "<f8>") 'ispell-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; spell-checking.el ends here
