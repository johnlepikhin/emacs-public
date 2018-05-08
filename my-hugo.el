
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
  (save-excursion
    (mapc 'my-org-hugo-export-file
          (directory-files "~/org/personal" t "^blog-.*\\.org$"))))

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

(defun my-hugo-improvements (text backend info)
  (when (org-export-derived-backend-p backend 'hugo)
    (my-hyphenize-russian text "&#173;")))

(add-to-list 'org-export-filter-plain-text-functions
             'my-hugo-improvements)



(provide 'my-hugo)
