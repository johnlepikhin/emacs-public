
(defun my-update-cursor ()
  (set-cursor-color
   (if (string= current-input-method "russian-computer") "red" "black")))

(add-hook 'buffer-list-update-hook 'my-update-cursor)

(defun my-update-isearch-input-method ()
  (if isearch-mode
      (progn
        (setq isearch-input-method-function input-method-function
              isearch-input-method-local-p t)
        (isearch-update))))

(defun my-update-input-method (is-ru)
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

(global-set-key (kbd "<M-f11>") 'my-select-input-eng)
(global-set-key (kbd "<M-f12>") 'my-select-input-rus)
(define-key isearch-mode-map (kbd "<M-f11>") 'my-select-input-eng)
(define-key isearch-mode-map (kbd "<M-f12>") 'my-select-input-rus)

(global-set-key (kbd "M-<tab>") (lambda () (interactive) (other-window 1)))

(setq focus-follows-mouse t
      mouse-autoselect-window nil)


(global-unset-key (kbd "C-z"))

(setq sentence-end-double-space nil)

(defun delete-all-spaces ()
  "Delete all spaces including newlines at a point"
  (interactive)
  (save-excursion
    (skip-chars-backward "\r\n\t ")
    (let ((start (point)))
      (skip-chars-forward "\r\n\t ")
      (delete-region start (point)))))

(defun fill-sentences-in-paragraph ()
  "Fill the current paragraph with a newline after each sentence."
  (interactive)
  (save-excursion
    (save-restriction
      (mark-paragraph)
      (narrow-to-region (point) (mark))
      (while (not (eobp))
        (fill-region-as-paragraph
         (point)
         (progn (forward-sentence) (point)))
        (delete-all-spaces)
        (newline)))))

;; region expanding

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;

(put 'narrow-to-region 'disabled nil)

(provide 'my-input)
