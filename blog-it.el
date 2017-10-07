
(require 'my-blogs)

(defun blog-select-IT ()
  (interactive)
  (configure-blog
   "~/blogs/johnlepikhin.github.io"
   "https://johnlepikhin.github.io"
   "source"
   "master"
   "The great nothingness"
   "."
   'anatta
   "UA-107728320-1"))

(provide 'blog-it)
