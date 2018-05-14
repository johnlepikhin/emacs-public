
(require 'my-orgmode)

(require 'common)

(with-eval-after-load 'ox
  (require 'ox-hugo))

(defun my-org-hugo-twits-prepare ()
  (setq-local org-twit-counter 1)
  (org-map-entries
   (lambda ()
     (when
         (and
          (not (string= (string-trim (org-entry-get nil "ITEM")) ""))
          (not (string= (org-entry-get nil "EXPORT_FILE_NAME") "")))
       (progn
         (org-todo 'done)
         (org-set-property
          "EXPORT_FILE_NAME"
          (format "twit-%s-%i" (format-time-string "%F-%T") org-twit-counter))
         (incf org-twit-counter))))
   "twit"
   'file))

(defun my-org-hugo-export-file (f)
  (interactive)
  (save-excursion
    (find-file f)
    (my-org-hugo-twits-prepare)
    (save-buffer)
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
