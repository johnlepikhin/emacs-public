
(defun my-update-cursor ()
  (set-cursor-color
   (if buffer-read-only "grey"
     (if (string= current-input-method "russian-computer")
         "red"
       "black"))))

(add-hook 'buffer-list-update-hook 'my-update-cursor)

(defun my-update-isearch-input-method ()
  (if isearch-mode
      (progn
        (setq isearch-input-method-function input-method-function
              isearch-input-method-local-p t)
        (isearch-update))))

(defun my-update-input-method (is-ru)
  (start-process "" nil "xkblayout-state" "set" "0")
  (if is-ru
      (set-input-method 'russian-computer)
    (inactivate-input-method))
  (my-update-isearch-input-method)
  (my-update-cursor))

(defun my-select-input-eng ()
  (interactive)
  (my-update-input-method nil))

(defun my-select-input-rus ()
  (interactive)
  (my-update-input-method t))

(global-set-key (kbd "s-\\") 'my-select-input-eng)
(global-set-key (kbd "C-\\") 'my-select-input-rus)
(define-key isearch-mode-map (kbd "C-\\") 'my-select-input-rus)
(define-key isearch-mode-map (kbd "s-\\") 'my-select-input-eng)

(global-set-key (kbd "M-<tab>") (lambda () (interactive) (other-window 1)))

(setq focus-follows-mouse t
      mouse-autoselect-window nil)


(global-unset-key (kbd "C-z"))

;; region expanding

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;

(put 'narrow-to-region 'disabled nil)

(provide 'my-input)
