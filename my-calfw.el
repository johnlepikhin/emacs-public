
(require 'calfw-cal)
(require 'calfw-ical)
(require 'calfw-org)

(setq calendar-week-start-day 1)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer :view 'two-weeks :contents-sources
    (list
     (cfw:org-create-source "Green"))))

(provide 'my-calfw)
