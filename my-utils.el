
;;; Code:

(defun number-or-v (n v)
  "Return V if N is not a number, or N otherwise."
  (if (not (numberp n)) v n))

(defun lst-number-or-v (lst pos v)
  "Return V if list element at POS is not a number, or (nth POS LST) otherwise."
  (number-or-v (nth pos lst) v))
  
(provide 'my-utils)
