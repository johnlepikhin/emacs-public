
(require 'org)

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


(require 'find-lisp)

(defun update-agenda-files-list ()
  (interactive)
  (setq org-agenda-files
        (find-lisp-find-files "~/org/" "\.org$")))

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((perl . t)
   (sh . t)
   (latex . t)
   (emacs-lisp . t)))

(add-hook
 'org-mode-hook
 (lambda ()
   (progn
     (local-unset-key [C-return])
     (local-unset-key [M-return])
     (local-unset-key [M-left])
     (local-unset-key [M-right]))))
