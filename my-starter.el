

(defun start-blog ()
  (interactive)
  (if (not (boundp 'start-blog-done))
      (progn
        (setq start-blog-done t)
        (require 'load-dir)
        (add-to-list 'load-path "~/.emacs.d/public/blog/elisp")
        (load-dir-one "~/.emacs.d/public/blog/elisp"))))

(defun start-devel ()
  (interactive)
  (if (not (boundp 'start-devel-done))
      (progn
        (setq start-devel-done t)
        (require 'load-dir)
        (add-to-list 'load-path "~/.emacs.d/public/devel")
        (load-dir-one "~/.emacs.d/public/devel"))))

(defun start-gnus-n-server ()
  (interactive)
  (if (not (boundp 'start-gnus-n-server-done))
      (progn
        (setq start-gnus-n-server-done t)
        (server-start)
        (start-devel)
        (require 'load-dir)
        (add-to-list 'load-path "~/.emacs.d/mygnus")
        (load-dir-one "~/.emacs.d/mygnus")
        (set-background-color "ivory1")
        (gnus))))

(defun start-desktop ()
  (interactive)
  (if (not (boundp 'start-desktop-done))
      (progn
        (message "Starting desktop environment")
        (setq start-desktop-done t)
        (start-blog)
        (start-devel))))

(defun start-mobile ()
  (interactive)
  (if (not (boundp 'start-mobile-done)))
      (progn
        (setq start-mobile-done t)))

(defun my-recompile-emacs-configs ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d/" 0))

(defun my-start ()
  (interactive)
  (if (file-exists-p "~/.emacs.d/flags/is_desktop")
      (start-desktop))
  (if (file-exists-p "~/.emacs.d/flags/is_mobile")
      (start-mobile)))

(my-start)

(provide 'my-starter)
