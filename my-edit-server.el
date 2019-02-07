
(when (and (daemonp) (locate-library "edit-server"))
  (require 'edit-server)
  (edit-server-start))

(provide 'my-edit-server)
