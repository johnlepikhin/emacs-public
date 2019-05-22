
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(require 'org)
(require 'org-agenda)
(require 'org-install)
(require 'find-lisp)
(require 'helm-org-rifle)
(require 'bbdb-anniv)
(require 'org-password-manager)
(require 'seq)
(require 'org-element)
(require 'my-sport)
(require 'ox-latex)
(require 'ox-confluence)
(require 'ispell)
(require 'filenotify)

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("\\[id:" . "\\]"))


(setq org-bbdb-anniversary-field 'birthday)

(add-to-list 'auto-mode-alist '("\\.s?org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)

(setq org-log-into-drawer 't
      org-catch-invisible-edits 'error
      org-cycle-separator-lines 0
      org-agenda-skip-scheduled-if-done 't)

(defvar my-org-inotify-handlers '() "List of file handlers to watch")

(defun my-org-agenda-delay-task ()
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
		       (org-agenda-error)))
	 (buffer (marker-buffer hdmarker))
	 (pos (marker-position hdmarker))
	 (inhibit-read-only t)
	 newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
        (let ((delay (org-read-date 't 'nil 'nil "Отложить до" 'nil
                                    (format-time-string "%H:%M" (time-add (current-time) 3600)))))
          (org-set-property "DELAYED_TILL" delay))))))

(defun my-org-agenda-skip-delayed ()
  (let ((now (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time) 120)))
        (delayed-till (org-read-date t nil (or (org-entry-get nil "DELAYED_TILL") "") nil))
        (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string> delayed-till now) subtree-end nil)))

(defun my-org-reload-from-disk (&optional event)
  (interactive)
  (with-current-buffer "*Org Agenda*"
    (org-agenda-maybe-redo)))

(defun my-org-fill-inotify-handlers ()
  (dolist (elt my-org-inotify-handlers)
    (file-notify-rm-watch elt))
  (setq my-org-inotify-handlers
        (list (mapcar
               (lambda (file)
                 (file-notify-add-watch
                  file
                  '(change attribute-change)
                  'my-org-reload-from-disk))
               org-agenda-files))))

(defun my-org-fill-files-list (&optional EXHAUSTIVE)
  (setq org-agenda-files
        (seq-remove
         (lambda (file) (string-match "[.]#" file))
         (directory-files-recursively "~/org" "[.]org$")))
  (my-org-fill-inotify-handlers))

(my-org-fill-files-list)

(run-with-timer 0 600 'my-org-fill-files-list)

(add-to-list 'org-modules 'org-id)
;; (add-to-list 'org-modules 'org-checklist)
(add-to-list 'org-modules 'org-gnus)
(add-to-list 'org-modules 'org-mouse)
(add-to-list 'org-modules 'org-attach-screenshot)

(setq org-src-tab-acts-natively t
      org-refile-targets '((org-agenda-files :maxlevel . 2))
      org-refile-use-outline-path 'file)

(add-to-list 'org-src-lang-modes '("conf" . conf))
(add-to-list 'org-src-lang-modes '("ini" . conf))
(add-to-list 'org-src-lang-modes '("vim" . vimrc))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w!)" "VERIFY(v!)" "|" "DONE(d!)" "DELEGATED(D!)" "CANCELED(c!)")))

(setq org-export-allow-bind-keywords t)

(setq org-todo-keyword-faces
      '(("WAIT" . (:foreground "#ff8040" :weight bold))
        ("VERIFY" . (:foreground "#afaf00" :weight bold))
        ("CANCELED" . (:foreground "#006000" :weight bold))))

(setq org-agenda-custom-commands
      '(("d" . "Сегодня")
        ("dd" agenda "Сегодня, все записи"
         ((org-agenda-span 'day)
          (org-agenda-overriding-header "Сегодня, все записи")))
        ("da" agenda "Сегодня, без отложенных"
         ((org-agenda-span 'day)
          (org-agenda-skip-function 'my-org-agenda-skip-delayed)
          (org-agenda-overriding-header "Сегодня, только активные")))))

(setq org-latex-default-packages-alist
      '(("utf8" "inputenc" t ("pdflatex"))
        ("T2A" "fontenc" t ("pdflatex"))
        ("russian" "babel" t)
        ("" "cmap" t)
        ("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "tabularx" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "hyperref" nil)))

(setq org-latex-logfiles-extensions '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx" "log" "nav" "out" "ptc" "run.xml" "snm" "toc" "vrb" "xdv" "tex"))

(require 'ob-ruby)
(require 'ob-perl)
(require 'ob-shell)
;; (require 'ob-sh)
(require 'ob-sql)
(require 'ob-plantuml)
(require 'ob-gnuplot)
(require 'ob-coq)
(require 'ob-python)
(require 'ob-ocaml)
(require 'ob-http)
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((perl . t)
   ;; (sh . t)
   (shell . t)
   (latex . t)
   (org . t)
   (dot . t)
   (http . t)
   (plantuml . t)
   (gnuplot 't)
   (emacs-lisp . t)))

(org-babel-lob-ingest "~/.dotfiles.conf.org")

(defun org-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

(defun org-global-prop-value (key)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (org-global-props key))))

(defun my-before-org-mode-save ()
  (interactive)
  (org-table-iterate-buffer-tables))

(defun my/org-confirm-babel-evaluate (lang body)
  (not (or
        (string= lang "latex")
        (string= lang "dot")
        (string= lang "graphviz")
        (string= lang "gnuplot")
        (string= lang "plantuml"))))

(setq org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)

;; common

(defun org-image-update-overlay (file link &optional data-p refresh)
  "Create image overlay for FILE associtated with org-element LINK.
        If DATA-P is non-nil FILE is not a file name but a string with the image data.
        See also `create-image'.
        This function is almost a duplicate of a part of `org-display-inline-images'."
  (when (or data-p (file-exists-p file))
    (let ((width
           ;; Apply `org-image-actual-width' specifications.
           (cond
            ((not (image-type-available-p 'imagemagick)) nil)
            ((eq org-image-actual-width t) nil)
            ((listp org-image-actual-width)
             (or
              ;; First try to find a width among
              ;; attributes associated to the paragraph
              ;; containing link.
              (let ((paragraph
                     (let ((e link))
                       (while (and (setq e (org-element-property
                                            :parent e))
                                   (not (eq (org-element-type e)
                                            'paragraph))))
                       e)))
                (when paragraph
                  (save-excursion
                    (goto-char (org-element-property :begin paragraph))
                    (when
                        (re-search-forward
                         "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
                         (org-element-property
                          :post-affiliated paragraph)
                         t)
                      (string-to-number (match-string 1))))))
              ;; Otherwise, fall-back to provided number.
              (car org-image-actual-width)))
            ((numberp org-image-actual-width)
             org-image-actual-width)))
          (old (get-char-property-and-overlay
                (org-element-property :begin link)
                'org-image-overlay)))
      (if (and (car-safe old) refresh)
          (image-refresh (overlay-get (cdr old) 'display))
        (let ((image (create-image file
                                   (and width 'imagemagick)
                                   data-p
                                   :width width)))
          (when image
            (let* ((link
                    ;; If inline image is the description
                    ;; of another link, be sure to
                    ;; consider the latter as the one to
                    ;; apply the overlay on.
                    (let ((parent
                           (org-element-property :parent link)))
                      (if (eq (org-element-type parent) 'link)
                          parent
                        link)))
                   (ov (make-overlay
                        (org-element-property :begin link)
                        (progn
                          (goto-char
                           (org-element-property :end link))
                          (skip-chars-backward " \t")
                          (point)))))
              (overlay-put ov 'display image)
              (overlay-put ov 'face 'default)
              (overlay-put ov 'org-image-overlay t)
              (overlay-put
               ov 'modification-hooks
               (list 'org-display-inline-remove-overlay))
              (push ov org-inline-image-overlays))))))))

;; youtube

(defvar yt-iframe-format
  (concat "<div class=\"yt-container\"><iframe src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe></div>"))

(defvar yt-hugo-format "{{< youtube id=\"%s\" >}}")

(org-link-set-parameters
 "yt"
 :follow (lambda (handle)
           (browse-url
            (concat "https://www.youtube.com/embed/"
                    handle)))
 :export (lambda (path desc backend)
           (cl-case backend
             (md (format yt-hugo-format path))
             (html (format yt-iframe-format path (or desc "")))
             (latex (format "\href{%s}{%s}"
                            path (or desc "video"))))))

(defun org-yt-get-image (url)
  "Retrieve image from url."
  (let ((image-buf (url-retrieve-synchronously url)))
    (when image-buf
      (with-current-buffer image-buf
        (goto-char (point-min))
        (when (looking-at "HTTP/")
          (delete-region (point-min)
                         (progn (re-search-forward "\n[\n]+")
                                (point))))
        (setq image-data (buffer-substring-no-properties (point-min) (point-max)))))))

(defconst org-yt-video-id-regexp "[-_[:alnum:]]\\{10\\}[AEIMQUYcgkosw048]"
  "Regexp matching youtube video id's taken from `https://webapps.stackexchange.com/questions/54443/format-for-id-of-youtube-video'.")

(defun org-yt-display-inline-images (&optional include-linked refresh beg end)
  "Like `org-display-inline-images' but for yt-links."
  (when (display-graphic-p)
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (let ((re (format "\\[\\[%s:\\(%s\\)\\]\\]" "yt" org-yt-video-id-regexp)))
       (while (re-search-forward re end t)
         (let ((video-id (match-string 1))
               (el (save-excursion (goto-char (match-beginning 1)) (org-element-context)))
               image-data)
           (when el
             (setq image-data 
                   (or (let ((old (get-char-property-and-overlay
                                   (org-element-property :begin el)
                                   'org-image-overlay)))
                         (and old
                              (car-safe old)
                              (overlay-get (cdr old) 'display)))
                       (org-yt-get-image (format "http://img.youtube.com/vi/%s/0.jpg" video-id))))
             (when image-data
               (org-image-update-overlay image-data el t t)))))))))

(advice-add #'org-display-inline-images :after #'org-yt-display-inline-images)

;;

(add-hook 'org-mode-hook 'org-password-manager-key-bindings)

;;

(defun my-org-refile (file headline &optional arg)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))

(defun my-org-add-todo/non-interacitve (file title)
  "Add TODO record to file non-interactively"
  (with-temp-buffer
    (insert (concat "* TODO " title))
    (org-schedule t)
    (my-org-refile file "")))

;;

(advice-add 'org-revert-all-org-buffers :before 'my-org-fill-files-list)
(advice-add 'org-agenda-redo-all :before 'my-org-fill-files-list)

;; autosave

(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

(defun my-org-set-flag-git-need-sync ()
  (write-region "" "" "~/org/need_git_sync"))

(defun my-org-mode-hook-cb ()
  (interactive)
  (local-unset-key [C-return])
  (local-unset-key [M-return])
  (local-unset-key [M-left])
  (local-unset-key [M-right])
  (local-unset-key (kbd "C-c RET"))
  (setq-local fill-column 90)
  (auto-fill-mode)

  (add-hook 'before-save-hook 'my-before-org-mode-save nil 'make-it-local)

  (if (and (not (null (buffer-file-name))) (string-match ".*/org/.*" (buffer-file-name)))
      (progn
        (setq-local buffer-save-without-query 't)
        (add-hook 'after-save-hook 'my-org-set-flag-git-need-sync nil t))))

(add-hook 'org-mode-hook 'my-org-mode-hook-cb)

;;

(defun my-org-archive-done-tasks ()
  "Archive all DONE tasks in current buffer"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(defun my-org-clone-to-date ()
  "Clone current subtree into specified file with all dates shifted to the same period."
  (interactive)
  (let* ((title (message (nth 4 (org-heading-components))))
         (orig-date (org-time-string-to-absolute (org-entry-get nil "SCHEDULED")))
         (dest-date (org-time-string-to-absolute
                     (org-read-date nil nil nil (format "Дата для '%s'" title))))
         (offset (format "+%id" (- dest-date orig-date))))
    (org-copy-subtree)
    (with-temp-buffer
      (org-mode)
      (org-paste-subtree)
      (org-clone-subtree-with-time-shift 1 offset)
      (org-forward-element)
      (org-refile))))

(defun my-agenda-mode-setup ()
  (hl-line-mode))

(add-hook 'org-agenda-mode-hook 'my-agenda-mode-setup)

;; templates

(setq org-capture-templates '())

(add-to-list
 'org-capture-templates
 '("e" "Экспедиция/мероприятие" plain
   (file (lambda () (read-file-name "Как назвать файл экспы: " "~/org/personal/sport/")))
   (function sport/expedition-template)))

(add-to-list
 'org-capture-templates
 '("g" "Общий TODO" entry (file "~/org/personal/general-TODO.org")
   "* TODO %?
SCHEDULED: %t"))

(defun my-org-inherit-input-method ()
  "Set the input method of this buffer to that of original's buffer."
  (let* ((note-buffer (marker-buffer org-log-note-marker))
         (im (with-current-buffer note-buffer
               current-input-method)))
    (set-input-method im)))

(add-hook 'org-log-buffer-setup-hook 'my-org-inherit-input-method)

(provide 'my-orgmode)
