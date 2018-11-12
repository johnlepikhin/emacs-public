
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun my-select-defun ()
  "Select current line."
  (interactive)
  (end-of-defun)
  (let ((end-of-defun-pos (point)))
    (beginning-of-defun)
    (push-mark end-of-defun-pos 'nil 't)
    (setq mark-active t)))

(global-set-key (kbd "C-c d s") 'my-select-defun)

(provide 'my-devel-common)
