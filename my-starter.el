
(require 'load-dir)

(defun start-frame-title-with-prefix (prefix)
  (interactive "Mname: ")
  (setq-default
   frame-title-format
   '("adsasd"
     (:eval
     (format "%s@%s: %s %s"
             (or (file-remote-p default-directory 'user)
                 user-real-login-name)
             (or (file-remote-p default-directory 'host)
                 system-name)
             (buffer-name)
             (cond 
              (buffer-file-truename
               (concat "(" buffer-file-truename ")"))
              (dired-directory
               (concat "{" dired-directory "}"))
              (t
               "[no file]"))) ))))

(start-frame-title-with-prefix "zzzz")
(setq-default frame-title-format "--- %b")

(defun start-general ()
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/public/blog/elisp")
  (load-dir-one "~/.emacs.d/public/blog/elisp"))

(defun start-gnus-n-server ()
  (interactive)
  (start-general)
  (server-start)
  (add-to-list 'load-path "~/.emacs.d/mygnus")
  (load-dir-one "~/.emacs.d/mygnus")
  (set-background-color "ivory1")
  (start-frame-title-with-prefix "GNUS")
  (gnus))

(defun start-devel ()
  (add-to-list 'load-path "~/.emacs.d/public/devel")
  (load-dir-one "~/.emacs.d/public/devel"))

(defun start-desktop ()
  (interactive)
  (start-general)
  (start-devel))

(defun start-mobile ()
  (interactive))

(provide 'my-starter)
