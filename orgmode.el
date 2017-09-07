
(require 'org)

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


(setq org-agenda-files (list "~/.emacs.d/org/"))

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

;(require 'org-attach-screenshot)
