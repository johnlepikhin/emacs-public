
;;; Code:

(setq debug-on-error 't)

(defun number-or-v (n v)
  "Return V if N is not a number, or n otherwise."
  (if (not (numberp n)) v n))

(defun lst-number-or-v (lst pos v)
  "Return v if list element at pos is not a number, or (nth pos lst) otherwise"
  (number-or-v (nth pos lst) v))
  
(provide 'my-utils)
