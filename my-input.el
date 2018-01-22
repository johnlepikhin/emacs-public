
(defun im-cursor-color-set nil 
  (set-cursor-color
   (if current-input-method 
       "red"
     "black")))

(add-hook 'post-command-hook 'im-cursor-color-set)

(global-set-key (kbd "s-SPC") (lambda () (interactive) (deactivate-input-method)))
(global-set-key (kbd "H-SPC") (lambda () (interactive) (set-input-method 'russian-computer)))
