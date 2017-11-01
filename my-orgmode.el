
(require 'package)
(add-to-list 'package-archives '("org-plus-contrib" . "http://orgmode.org/elpa/") t)

(require 'org)
(require 'org-checklist)
(require 'org-install)
(require 'find-lisp)

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
     (add-hook 'before-save-hook 'my-before-org-mode-save nil 'make-it-local)
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

(provide 'my-orgmode)
