
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(global-set-key (kbd "C-c d s") 'mark-defun)

(setq tags-revert-without-query t)

(provide 'my-devel-common)
