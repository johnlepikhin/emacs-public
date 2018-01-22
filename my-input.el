
(defun im-cursor-color-set nil 
  (set-cursor-color
   (if current-input-method 
       "red"
     "black")))

(add-hook 'post-command-hook 'im-cursor-color-set)

(global-set-key [?\s-/] 'toggle-input-method)
(define-key isearch-mode-map [?\s-/] 'isearch-toggle-input-method)
