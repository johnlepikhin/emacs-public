
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(require 'org)
(require 'org-agenda)
(require 'org-checklist)
(require 'org-install)
(require 'find-lisp)
(require 'git-auto-commit-mode)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(defun update-agenda-files-list ()
  (interactive)
  (setq org-agenda-files
        (find-lisp-find-files "~/org/" "-TODO\.org$")))

(run-with-timer 0 600 'update-agenda-files-list)

(add-to-list 'org-modules 'org-id)
(add-to-list 'org-modules 'org-checklist)
(add-to-list 'org-modules 'org-gnus)
(add-to-list 'org-modules 'org-mouse)
(add-to-list 'org-modules 'org-attach-screenshot)

(setq org-todo-keywords
      '((sequence "TODO(t!)" "WAIT(w!)" "VERIFY(v!)" "|" "DONE(d!)" "DELEGATED(D!)" "CANCELED(c!)")))

(setq org-todo-keyword-faces
      '(("WAIT" . (:foreground "#ff8040" :weight bold))
        ("VERIFY" . (:foreground "#afaf00" :weight bold))
        ("CANCELED" . (:foreground "#006000" :weight bold))))

(require 'ox-latex)

(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t ("pdflatex"))
       ("T2A" "fontenc" t ("pdflatex"))
       ("" "graphicx" t)
       ("" "grffile" t)
       ("" "longtable" nil)
       ("" "wrapfig" nil)
       ("" "rotating" nil)
       ("normalem" "ulem" t)
       ("" "amsmath" t)
       ("" "textcomp" t)
       ("" "amssymb" t)
       ("" "capt-of" nil)
       ("" "hyperref" nil)))

(setq org-latex-logfiles-extensions '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx" "log" "nav" "out" "ptc" "run.xml" "snm" "toc" "vrb" "xdv" "tex"))

(require 'ob-ruby)
(require 'ob-perl)
(require 'ob-sh)
(require 'ob-sql)
(require 'ob-plantuml)
(require 'ob-gnuplot)
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((perl . t)
   (sh . t)
   (latex . t)
   (dot . t)
   (plantuml . t)
   (gnuplot 't)
   (emacs-lisp . t)))

(defun my-before-org-mode-save ()
  (interactive)
  (org-table-iterate-buffer-tables))

(add-hook
 'org-mode-hook
 (lambda ()
   (progn
     (setq-local buffer-save-without-query 't)
     (add-hook 'before-save-hook 'my-before-org-mode-save nil 'make-it-local)

     (git-auto-commit-mode +1)
     (setq-local gac-automatically-push-p 't)

     (local-unset-key [C-return])
     (local-unset-key [M-return])
     (local-unset-key [M-left])
     (local-unset-key [M-right]))))

(defun my/org-confirm-babel-evaluate (lang body)
  (not (or
        (string= lang "latex")
        (string= lang "dot")
        (string= lang "graphviz")
        (string= lang "gnuplot")
        (string= lang "plantuml"))))
(setq org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)


;; autosave

(defun my-orgmode-autosave-setup-common ()
  (interactive)
  (setq-local auto-save-interval 20)
  (setq-local auto-save-timeout 30)
  (auto-save-mode)
  (setq-local gac-automatically-push-p 't)
  (git-auto-commit-mode +1))

(defun my-orgmode-autosave-disable-common ()
  (interactive)
  (setq-local auto-save-interval 0)
  (setq-local auto-save-timeout nil)
  (auto-save-mode -1)
  (git-auto-commit-mode -1))

(defun my-org-agenda-autosave-setup ()
  (interactive)
  (my-orgmode-autosave-setup-common)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t))

(add-hook 'org-agenda-mode-hook 'my-org-agenda-autosave-setup)

(defun my-orgmode-autosave-setup ()
  (interactive)
  (if (string-match ".*/org/.*" (buffer-file-name))
      (progn
        (my-orgmode-autosave-setup-common)
        (add-hook 'auto-save-hook 'save-buffer nil t))))

(add-hook 'org-mode-hook 'my-orgmode-autosave-setup)

;;

(defun my-org-archive-done-tasks ()
  "Archive all DONE tasks in current buffer"
  (interactive)
  ;; (my-orgmode-autosave-disable-common)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
  ;; (my-orgmode-autosave-setup-common))

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
_y_: ?y? year       _q_: quit           _L__l__c_: log = ?l?"
  ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]"))
  ("w" org-agenda-week-view (if (eq 'week (org-agenda-cts)) "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]"))
  ("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
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
