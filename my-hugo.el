
(require 'my-orgmode)

(with-eval-after-load 'ox
  (require 'ox-hugo))

(defun my-org-hugo-export-file (f)
  (interactive)
  (save-excursion 
    (find-file f)
    (org-hugo-export-wim-to-md :all-subtrees)
    (kill-buffer (current-buffer))))

(defun my-org-hugo-export-files-org-personal () 
  (interactive)
  (mapc 'my-org-hugo-export-file
        (directory-files "~/org/personal" t "^blog-.*\\.org$")))


;; Hyphenation

(defun my-hyphenize-russian (input hyphen)
  (interactive)
  (with-temp-buffer
    (progn
      (insert input)
      (call-process-region
       (point-min)
       (point-max)
       "~/bin/hyphen/russian/russian-hyphen.pl"
       t
       t
       nil
       "--hyphenize-stdin"
       (concat "--hyphen-char=" hyphen))
      (buffer-string))))

(org-export-define-derived-backend 'my-hugo 'hugo
  :translate-alist '((plain-text . my-hugo-improvements)
                     (bold . my-hugo-improvements)
                     (italic . my-hugo-improvements)
                     (paragraph . my-hugo-improvements)))

(defun my-hugo-improvements (contents info)
  (my-blog-macro-expand
   (my-hyphenize-russian contents "&#173;")))



(provide 'my-hugo)
