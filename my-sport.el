
(require 'my-utils)
(require 's)
(require 'seq)

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
    (sort 
     (remove-if-not
      '(lambda (row) (> (nth 1 row) 0))
      (map
       #'list
       '(lambda (row)
          (let ((name (first row))
                (cnt (lst-number-or-v row pos 0)))
            (list name (* (lst-number-or-v row 1 0) cnt) cnt)))
       (cdr src)))
     (lambda (a b) (> (nth 1 a) (nth 1 b)))))))

(defun sport/expedition-template ()
  (interactive)
  (let* ((event (read-string "Название экспедиции: " nil))
         (category (org-completing-read-no-i "Категория в org: " nil))
         (date-input (org-read-date nil t nil "Начало экспедиции: "))
         (date-fmt (car org-time-stamp-formats))
         (date-5-months (format-time-string date-fmt (time-subtract date-input (seconds-to-time (* 86400 30 5)))))
         (date-1-month (format-time-string date-fmt (time-subtract date-input (seconds-to-time (* 86400 30)))))
         (date-2-weeks (format-time-string date-fmt (time-subtract date-input (seconds-to-time (* 86400 7 2)))))
         (date-1-week (format-time-string date-fmt (time-subtract date-input (seconds-to-time (* 86400 7)))))
         (date-1-day (format-time-string date-fmt (time-subtract date-input (seconds-to-time (* 86400 1)))))
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

(defun my-sport-journal-add (type value notes)
  (interactive
   (list
    (completing-read
     "Тип: "
     (with-current-buffer (find-file-noselect "~/org/personal/sport/sports-periodic-TODO.org")
       (goto-char (point-min))
       (setq case-fold-search nil)
       (re-search-forward "^#\\+NAME: sports-journal")
       (next-line)
       (mapcar
        (lambda (row) (car (cdr row)))
        (seq-filter (lambda (row) (not (eq row 'hline))) (org-table-to-lisp)))))
    (read-string "Значение: ")
    (read-string "Заметки: ")))
  ;; TODO: improve
  (with-current-buffer (find-file-noselect "~/org/personal/sport/sports-periodic-TODO.org")
    (goto-char (point-min))
    (setq case-fold-search nil)
    (re-search-forward "^#\\+NAME: sports-journal")
    (next-line)
    (goto-char (org-table-end))
    (insert
     (format "| [%s] | %s | | %s | %s |\n" (format-time-string "%F %R") type value notes))))

(provide 'my-sport)
