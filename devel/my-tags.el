
(setq path-to-ctags "~/bin/ctags")

(defun my-create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (let ((dir (directory-file-name dir-name)))
    (shell-command
     (format "%s --exclude='*_flymake*' -f %s/TAGS -e -R %s" path-to-ctags dir dir))
    (visit-tags-table dir)))

(provide 'my-tags)
