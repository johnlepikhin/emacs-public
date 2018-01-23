
(defun im-cursor-color-set nil 
  (set-cursor-color
   (if current-input-method 
       "red"
     "black")))

(add-hook 'post-command-hook 'im-cursor-color-set)

(global-set-key (kbd "s-\\") (lambda () (interactive) (deactivate-input-method)))
(global-set-key (kbd "C-\\") (lambda () (interactive) (set-input-method 'russian-computer)))


(defun my-reset-xkb-layout ()
  (message "reset XKB...")
  (shell-command "~/bin/xkblayout-state set 0"))
  

(add-hook 'focus-in-hook 'my-reset-xkb-layout)
