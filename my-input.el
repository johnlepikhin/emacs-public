
(set-input-method 'arabic)

(defun my-update-cursor
    (if buffer-read-only "grey"

      
input-method-alist

(defun my-update-isearch-input-method ()
  (setq isearch-input-method-function input-method-function
        isearch-input-method-local-p t)
  ; (setq input-method-function nil)
  (isearch-update))

(defun my-update-input-method (is-ru)
  (if is-ru
      (progn
        (start-process "" nil "xkblayout-state" "set" "0")
        (set-input-method 'russian-computer)
        ;; (set-face-attribute 'mode-line nil :background "red")
        (set-cursor-color "red")
        (my-update-isearch-input-method))
    (progn
      ;; (deactivate-input-method)
      (inactivate-input-method)
      (start-process "" nil "xkblayout-state" "set" "0")
      ;; (set-face-attribute 'mode-line nil :background "light gray")
      (set-cursor-color "black")
      (my-update-isearch-input-method))))

(deactivate-input-method)

(defun my-select-input-eng ()
  (interactive)
  (my-update-input-method nil))

(defun my-select-input-rus ()
  (interactive)
  (my-update-input-method t))

(global-set-key (kbd "s-\\") 'my-select-input-eng)
(global-set-key (kbd "C-\\") 'my-select-input-rus)
; (define-key isearch-mode-map (kbd "C-\\") 'my-select-input-rus)
; (define-key isearch-mode-map (kbd "s-\\") 'my-select-input-eng)

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
