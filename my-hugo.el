
(require 'my-orgmode)

(require 'common)
(require 'cl)
(require 'seq)
(require 'files)

(with-eval-after-load 'ox
  (require 'ox-hugo))

(setq org-hugo-external-file-extensions-allowed-for-copying '("jpg" "jpeg" "tiff" "png" "svg" "gif" "pdf" "odt" "doc" "ppt" "xls" "docx" "pptx" "xlsx" "sorg"))

(defun my-org-hugo-add-printable-version (backend)
  (if (eq backend 'hugo)
      (let ((generate-printable (org-entry-get nil "HUGO_GENERATE_PRINTABLE"))
            (file-org-name (buffer-file-name))
            (file-pdf-name (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
        (if (and generate-printable (string= generate-printable "t"))
            (if (file-newer-than-file-p file-org-name file-pdf-name)
                (progn
                  (org-latex-export-to-pdf)
                  (find-file file-org-name)
                  (if (not (org-entry-get nil "HUGO_GENERATE_PRINTABLE_ADDED"))
                      (progn
                        (save-excursion
                          (goto-char (point-max))
                          (insert (format "\n** Версия для печати\n\nДля удобства просмотра и печати можно воспользоваться [[file:%s][PDF]]-версией этой статьи." (file-name-nondirectory file-pdf-name))))
                        (org-set-property "HUGO_GENERATE_PRINTABLE_ADDED" "t")
                        (save-buffer)))))))))

(defun my-org-hugo-add-source-of-article (backend)
  (if (eq backend 'hugo)
      (let* ((generate-printable (org-entry-get nil "HUGO_ADD_ARTICLE_SOURCE"))
            (file-org-name (buffer-file-name))
            (file-org-shortname (file-name-nondirectory file-org-name)))
        (if (and generate-printable (string= generate-printable "t"))
            (progn
              (find-file file-org-name)
              (if (not (org-entry-get nil "HUGO_ADD_ARTICLE_SOURCE_ADDED"))
                  (progn
                    (save-excursion
                      (goto-char (point-max))
                      (insert (format "\n** Исходник статьи\n\nСсылка для скачивания: [[file:%s][%s]]."
                                      file-org-shortname
                                      file-org-shortname)))
                        (org-set-property "HUGO_ADD_ARTICLE_SOURCE_ADDED" "t")
                        (save-buffer))))))))

(add-hook 'org-export-before-processing-hook 'my-org-hugo-add-printable-version)
(add-hook 'org-export-before-processing-hook 'my-org-hugo-add-source-of-article)
(remove-hook 'org-export-before-parsing-hook 'my-org-hugo-add-printable-version)

(defun my-org-hugo-twits-prepare (file)
  (interactive)
  (setq-local org-twit-counter 1)
  (message "Preparing twits...")
  (org-map-entries
   (lambda ()
     (when
         (and
          (not (string= (string-trim (org-entry-get nil "ITEM")) ""))
          (not (org-entry-get nil "EXPORT_FILE_NAME")))
       (progn
         (message (format "Preparing twit '%s'" (org-entry-get nil "ITEM")))
         (org-todo 'done)
         (org-set-property
          "EXPORT_FILE_NAME"
          (format "twit-%s-%i" (format-time-string "%F-%T") org-twit-counter))
         (incf org-twit-counter))))
   "twit"
   (list file)))

(defun my-org-hugo-export-file (f)
  (interactive)
  (save-excursion
    (message (concat "Processing file " f))
    (find-file f)
    (my-org-hugo-twits-prepare f)
    (save-buffer)
    (org-hugo-export-wim-to-md :all-subtrees nil nil t)
    (kill-buffer (current-buffer))))

(defun my-org-hugo-export-files-org-personal (&key newer-than)
  (interactive)
  (save-excursion
    (let ((newer-than (seconds-to-time (if (null newer-than) 0 newer-than))))
      (mapc 'my-org-hugo-export-file
            (seq-filter
             (lambda (file)
               (and
                (not (string-match "/[.]#" file))
                (time-less-p newer-than (nth 5 (file-attributes file)))))
             (directory-files-recursively "~/org/personal" "\\.s?org$"))))))

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
