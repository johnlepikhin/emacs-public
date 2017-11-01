
(require 'load-dir)

(defun start-gnus-n-server ()
  (interactive)
  (server-start)
  (add-to-list 'load-path "~/.emacs.d/mygnus")
  (load-dir-one "~/.emacs.d/mygnus")
  (gnus))

(provide 'my-starter)
