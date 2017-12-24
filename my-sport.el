
(require 'my-utils)

(defun sport/equipment-report-weights (src)
  "Return summary weights grouped by buggage type"
  (cons
   (nthcdr 2 (first src))
   (cons
    'hline
    (list (let ((cols (- (length (first src)) 2)))
    (seq-reduce '(lambda (sums row)
        (mapcar* '(lambda (v s)
            (+ (* (number-or-v v 0) (lst-number-or-v row 1 0))
            s))
         (nthcdr 2 row) sums))
   (cdr src)
   (make-list cols 0)))))))

(defun sport/equipment-report-baggage (src pos)
  "Return list for specified buggage type (column position in src table)"
  (cons
   '("" "Кол-во" "Вес")
   (cons
    'hline
    (remove-if-not
     '(lambda (row) (> (nth 1 row) 0))
     (map
      #'list
      '(lambda (row)
         (let ((name (first row))
               (cnt (lst-number-or-v row pos 0)))
               (list name cnt (* (lst-number-or-v row 1 0) cnt))))
      (cdr src))))))

(provide 'my-sport)
