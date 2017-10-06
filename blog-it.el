
(require 'my-blogs)

(defun blog-IT-select ()
  (interactive)
  (configure-blog
   "~/blogs/content/it"
   "https://johnlepikhin.github.io"
   "source"
   "master"
   "The great nothingness"
   "/it"
   "."
   'anatta
   "UA-107728320-1"))

(provide 'blog-it)
