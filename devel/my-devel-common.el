
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun my-select-defun ()
  "Select current line."
  (interactive)
  (goto-char (beginning-of-defun))
  (setq mark-active t))

(provide 'my-devel-common)
