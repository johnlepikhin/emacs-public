
(require 'calfw-cal)
(require 'calfw-ical)
(require 'calfw-org)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer :contents-sources
    (list
     (cfw:org-create-source "Green")  ; orgmode source
     (cfw:cal-create-source "Orange") ; diary source)
     (cfw:ical-create-source "google" "http://www.google.com/calendar/ical/blah/basic.ics" "Red")))
