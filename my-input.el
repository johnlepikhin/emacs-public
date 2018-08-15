
(defun im-cursor-color-set nil
  (set-cursor-color
   (if current-input-method 
       "red"
     "black")))

(add-hook 'post-command-hook 'im-cursor-color-set)

(global-set-key (kbd "s-\\") (lambda () (interactive) (deactivate-input-method)))
(global-set-key (kbd "C-\\") (lambda () (interactive) (set-input-method 'russian-computer)))

(global-set-key (kbd "M-<tab>") (lambda () (interactive) (other-window 1)))

(global-set-key (kbd "s-'") (lambda () (interactive) (insert "«»") (left-char)))

(setq focus-follows-mouse t
      mouse-autoselect-window nil)


(global-unset-key (kbd "C-z"))

;; region expanding

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;



(provide 'my-input)
