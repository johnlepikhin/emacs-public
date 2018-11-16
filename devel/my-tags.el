
(setq path-to-ctags "~/bin/ctags")

(defun my-create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s --exclude='*_flycheck*' -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name))))

(provide 'my-tags)
