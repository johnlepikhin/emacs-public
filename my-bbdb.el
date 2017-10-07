
;(require 'bbdb-autoloads)
(require 'bbdb)
(load "bbdb-com" t)
(bbdb-initialize 'gnus 'message 'reportmail 'w3)
(bbdb-mua-auto-update-init 'gnus 'message)

(setq bbdb-pop-up-window-size 0.15)
(setq bbdb-mua-pop-up-window-size 0.15)

(setq bbdb-mua-update-interactive-p '(query . create))

(setq bbdb-message-all-addresses t)

(add-hook
 'gnus-summary-mode-hook
 (lambda ()
   (define-key gnus-summary-mode-map (kbd ";") 'bbdb-mua-edit-field)))


(add-to-list 'load-path "~/.emacs.d/lisp/")
(autoload 'gmail2bbdb-import-file "gmail2bbdb" "" t)
(setq gmail2bbdb-bbdb-file "~/.emacs.d/bbdb")


(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(add-hook 'bbdb-list-hook 'my-bbdb-display-xface)
(defun my-bbdb-display-xface ()
      "Search for face properties and display the faces."
      (when (or (gnus-image-type-available-p 'xface)
                (gnus-image-type-available-p 'pbm))
        (save-excursion
          (goto-char (point-min))
          (let ((inhibit-read-only t); edit the BBDB buffer
                (default-enable-multibyte-characters nil); prevents corruption
                pbm faces)
          (while (re-search-forward "^           face: \\(.*\\)" nil t)
            (setq faces (match-string 1))
            (replace-match "" t t nil 1)
            (dolist (data (split-string faces ", "))
              (condition-case nil
                  (insert-image (create-image (gnus-convert-face-to-png data) nil t))
                (error
                 (insert-image (gnus-create-image (uncompface data) nil t :face 'tooltip))))
              (insert " ")))))))


(provide 'my-bbdb)
