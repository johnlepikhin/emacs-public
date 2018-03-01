
(require 'calfw-cal)
(require 'calfw-ical)
(require 'calfw-org)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer :contents-sources
    (list
     (cfw:org-create-source "Green"))))

