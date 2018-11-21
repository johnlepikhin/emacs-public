
(defun im-cursor-color-set nil
  (set-cursor-color
   (if current-input-method 
       "red"
     "black")))

(add-hook 'post-command-hook 'im-cursor-color-set)

(defun my-update-input-method (is-ru)
  (if is-ru
      (progn
        (set-input-method 'russian-computer)
        (set-cursor-color "red"))
    (progn
      (deactivate-input-method)
      (set-cursor-color "black"))))

(defun my-select-input-eng ()
  (interactive)
  (my-update-input-method nil))

(defun my-select-input-rus ()
  (interactive)
  (my-update-input-method t))

(global-set-key (kbd "s-\\") 'my-select-input-eng)
(global-set-key (kbd "C-\\") 'my-select-input-rus)

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
