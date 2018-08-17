
(defun shell-quote-selection ()
  (interactive)
  (if (use-region-p)
      (let ((regionp (buffer-substring (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert (shell-quote-argument regionp)))))

(defun shell-quoted-yank ()
  (interactive)
  (insert (shell-quote-argument (car kill-ring))))

(provide 'my-shell)
