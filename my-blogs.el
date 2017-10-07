
(add-to-list 'load-path "~/.emacs.d/public/org-page-patched")

(require 'org-page)
(require 'blog-admin)

;; common settings

(setq httpd-port 9999
      org-plantuml-jar-path "~/bin/plantuml.jar"
      op/highlight-render 'htmlize
      blog-admin-backend-type 'org-page
      blog-admin-backend-new-post-in-drafts t
      blog-admin-backend-new-post-with-same-name-dir t
      blog-admin-backend-org-page-drafts "_drafts"
      org-export-default-language "ru"
      op/export-backend 'my-html
      op/personal-avatar "https://avatars0.githubusercontent.com/u/381986?v=4&s=80"
      op/theme-root-directory "~/.emacs.d/public/blog/themes"
      op/personal-github-link "https://github.com/johnlepikhin")

(defun configure-blog (directory domain master-branch html-branch main-title subtitle theme google-analytics)
  (interactive)
  (progn
    (setq op/repository-directory directory
          blog-admin-backend-path directory
          op/site-domain domain
          op/repository-org-branch master-branch
          op/repository-html-branch html-branch
          op/site-main-title main-title
          op/site-sub-title subtitle
          op/theme theme
          op/personal-google-analytics-id google-analytics)))

;; YouTube support

(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<div class=\"yt-container\"><iframe src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))

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

(org-export-define-derived-backend 'my-html 'html
  :translate-alist '((plain-text . my-html-improvements)
                     (template . my-html-improvements)))

(defun my-html-improvements (contents info)
  (my-hyphenize-russian contents "&shy;"))

;;

(defalias 'blog-post 'op/new-post)

(defun blog-publish-changes ()
  (interactive)
  (op/do-publication nil "HEAD~1" nil t t))

(defun blog-publish-all ()
  (interactive)
  (op/do-publication t "HEAD~1" nil t t))

(defun blog-insert-image-by-url (url filename)
  (interactive "MURL: \nMFile: ")
  (if (not (string= major-mode "org-mode"))
      (error "Error major mode is not org-mode"))
  (setq filename (concat "./images/" filename))
  (mkdir "images" t)
  (url-copy-file url filename)
  (insert (concat "file:" filename)))

(provide 'my-blogs)
