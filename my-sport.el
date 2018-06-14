
(require 'my-utils)
(require 's)

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
  (let* ((event (org-completing-read-no-i "Название экспедиции: " nil))
         (category (org-completing-read-no-i "Категория в org: " nil))
         (date-input (org-read-date nil t nil "Начало экспедиции: "))
         (date-fmt (car org-time-stamp-formats))
         (date-5-months (format-time-string date-fmt (time-subtract date-input (seconds-to-time (* 86400 30 5)))))
         (date-1-month (format-time-string date-fmt (time-subtract date-input (seconds-to-time (* 86400 30)))))
         (date-2-weeks (format-time-string date-fmt (time-subtract date-input (seconds-to-time (* 86400 7 2)))))
         (date-1-week (format-time-string date-fmt (time-subtract date-input (seconds-to-time (* 86400 7)))))
         (date-start (format-time-string date-fmt date-input))
         (src-tpl (with-temp-buffer
                    (insert-file-contents "~/org/personal/sport/организация-экспедиции.org.tpl")
                    (buffer-string)))
         (tpl
          (s-replace
           "%event%" event
           (s-replace
            "%category%" category
            (s-replace
             "%date_5_months%" date-5-months
             (s-replace
              "%date_1_month%" date-1-month
              (s-replace
               "%date_2_weeks%" date-2-weeks
               (s-replace
                "%date_1_week%" date-1-week
                (s-replace "%date_start%" date-start src-tpl)))))))))
    tpl))

(provide 'my-sport)
