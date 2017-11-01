
(require 'load-dir)

(defun start-general ()
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/public/blog/elisp")
  (load-dir "~/.emacs.d/public/blog/elisp"))

(defun start-gnus-n-server ()
  (interactive)
  (start-general)
  (server-start)
  (add-to-list 'load-path "~/.emacs.d/mygnus")
  (load-dir-one "~/.emacs.d/mygnus")
  (set-background-color "ivory1")
  (gnus))

(defun start-devel ()
  (add-to-list 'load-path "~/.emacs.d/public/devel")
  (load-dir "~/.emacs.d/public/devel"))

(defun start-desktop ()
  (interactive)
  (start-general)
  (start-devel))

(defun start-mobile ()
  (interactive)
  (start-general))

(provide 'my-starter)
