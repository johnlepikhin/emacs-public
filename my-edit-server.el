
(when (and (daemonp) (locate-library "edit-server"))
  (require 'edit-server)
  (edit-server-start t))

(provide 'my-edit-server)
