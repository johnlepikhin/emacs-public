
(require 'my-utils)

(defun sport/equipment-report-weights (src)
  "Return summary weights grouped by buggage type"
  (let
      ((vals
        (let ((cols (- (length (first src)) 2)))
                (seq-reduce '(lambda (sums row)
                               (mapcar*
                                '(lambda (v s)
                                   (+ (* (number-or-v v 0) (lst-number-or-v row 1 0)) s))
                                (nthcdr 2 row)
                                sums))
                            (cdr src)
                            (make-list cols 0)))))
    (list
     (cons "ИТОГО" (nthcdr 2 (first src)))
     'hline
     (cons (seq-reduce #'+ vals 0) vals))))

(defun sport/equipment-report-baggage (src pos)
  "Return list for specified buggage type (column position in src table)"
  (cons
   '("" "Вес" "Кол-во")
   (cons
    'hline
    (remove-if-not
     '(lambda (row) (> (nth 1 row) 0))
     (map
      #'list
      '(lambda (row)
         (let ((name (first row))
               (cnt (lst-number-or-v row pos 0)))
               (list name (* (lst-number-or-v row 1 0) cnt) cnt)))
      (cdr src))))))

(defun sport/expedition-template ()
  (interactive)
  (let ((tpl (with-temp-buffer
      (insert-file-contents "~/org/personal/sport/организация-экспедиции.org.tpl")
      (buffer-string))))
    (message tpl)))

(provide 'my-sport)
