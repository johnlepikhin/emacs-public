
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(require 'org)
(require 'org-agenda)
;; (require 'org-checklist)
(require 'org-install)
(require 'find-lisp)
(require 'helm-org-rifle)
(require 'bbdb-anniv)
(require 'org-password-manager)
(require 'seq)
(require 'org-element)
(require 'my-sport)
;; (require 'git-auto-commit-mode)
(require 'ox-latex)

(setq org-bbdb-anniversary-field 'birthday)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)

(setq org-log-into-drawer 't)

(defun my-org-fill-files-list (&optional EXHAUSTIVE)
  (setq org-agenda-files
        (seq-remove
         (lambda (file) (string-match "[.]#" file))
         (directory-files-recursively "~/org" "[.]org$"))))

(run-with-timer 0 600 'my-org-fill-files-list)

(add-to-list 'org-modules 'org-id)
;; (add-to-list 'org-modules 'org-checklist)
(add-to-list 'org-modules 'org-gnus)
(add-to-list 'org-modules 'org-mouse)
(add-to-list 'org-modules 'org-attach-screenshot)

(setq org-src-tab-acts-natively t
      org-refile-targets '((org-agenda-files :maxlevel . 2))
      org-refile-use-outline-path 'file)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w!)" "VERIFY(v!)" "|" "DONE(d!)" "DELEGATED(D!)" "CANCELED(c!)")))

(setq org-todo-keyword-faces
      '(("WAIT" . (:foreground "#ff8040" :weight bold))
        ("VERIFY" . (:foreground "#afaf00" :weight bold))
        ("CANCELED" . (:foreground "#006000" :weight bold))))

(add-to-list 'org-agenda-custom-commands
             '("d" agenda "Agenda for current day"
               ((org-agenda-span 'day)
                (org-agenda-overriding-header "Today's Deadlines "))))

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
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((perl . t)
   ;; (sh . t)
   (shell . t)
   (latex . t)
   (dot . t)
   (plantuml . t)
   (gnuplot 't)
   (emacs-lisp . t)))

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

;; alerts

; (require 'org-alert)
; (setq alert-default-style 'libnotify)

;;

(add-hook 'org-mode-hook 'org-password-manager-key-bindings)

;;

(defvar my-org-default-file "~/org/personal/general-TODO.org")

(defun my-org-add-todo ()
  (interactive)
  (find-file my-org-default-file)
  (goto-char (point-max))
  (insert "* TODO ")
  (org-schedule t))

(global-set-key (kbd "C-c RET") 'my-org-add-todo)

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

;; templates

(custom-set-variables
  '(org-capture-templates
    (quote
     (("e" "Экспедиция/мероприятие" plain
       (file (lambda () (read-file-name "Как назвать файл экспы: " "~/org/personal/sport/")))
       (function sport/expedition-template))))))

;; setup hydra for agenda mode

(require 'hydra)

(defun org-agenda-cts ()
  (and (eq major-mode 'org-agenda-mode)
       (let ((args (get-text-property
                    (min (1- (point-max)) (point))
                    'org-last-args)))
         (nth 2 args))))

(defhydra hydra-org-agenda-view (:hint none)
  "
_d_: ?d? day        _g_: time grid=?g?  _a_: arch-trees
_w_: ?w? week       _[_: inactive       _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?     _r_: clock report=?r?
_m_: ?m? month      _e_: entry text=?e? _D_: include diary=?D?
_y_: ?y? year       _q_: quit           _L__l__c_: log = ?l?
_C_: calendar
"
  ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]"))
  ("w" org-agenda-week-view (if (eq 'week (org-agenda-cts)) "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]"))
  ("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("C" my-open-calendar)
  ("f" org-agenda-follow-mode (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("[" (let ((org-agenda-include-inactive-timestamps t))
         (org-agenda-check-type t 'timeline 'agenda)
         (org-agenda-redo)
         (message "Display now includes inactive timestamps as well")))
  ("q" (message "Abort") :exit t)
  ("v" nil))

(define-key org-agenda-mode-map "v" 'hydra-org-agenda-view/body)

(provide 'my-orgmode)
