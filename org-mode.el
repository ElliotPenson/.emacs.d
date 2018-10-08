;;; org-mode.el --- Org mode configuration
;; Author: Elliot Penson
;;
;;; Commentary:
;; Org mode configuration
;;
;;
;;; Code:

(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/org/"
         :base-extension "org"
         :publishing-directory "~/org/public_html/"
         :publishing-function org-twbs-publish-to-html
         :recursive t
         :with-headline-numbers nil
         :with-sub-superscript nil)
        ("org-static"
         :base-directory "~/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|pdf"
         :publishing-directory "~/org/public_html/"
         :publishing-function org-publish-attachment
         :recursive t)
        ("org" :components ("org-notes" "org-static"))))

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

(setq org-drill-learn-fraction 0.25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode.el ends here
