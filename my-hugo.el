
(require 'my-orgmode)

(with-eval-after-load 'ox
  (require 'ox-hugo))

(defun my-org-hugo-export-file (f)
  (save-excursion 
    (find-file f)
    (org-hugo-export-wim-to-md :all-subtrees)
    (kill-buffer (current-buffer))))

(defun my-org-hugo-export-files-org-personal () 
  (mapc 'my-org-hugo-export-file
        (directory-files dir t ".txt$")))


(provide 'my-hugo)
