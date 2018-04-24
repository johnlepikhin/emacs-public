
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
  (mapc 'my-org-hugo-export-file
        (directory-files "~/org/personal" t "^blog-.*\\.org$")))


(provide 'my-hugo)
