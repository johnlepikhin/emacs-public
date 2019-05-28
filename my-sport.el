
(require 'my-utils)
(require 's)
(require 'seq)
(require 'org-id)

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

(defun sport/equipment-report-shared (src)
  "Return list for specified buggage type (column position in src table)"
  (cons
   '("" "Вес" "Кол-во")
   (cons
    'hline
    (sort 
      (map
       #'list
       '(lambda (row)
          (list
           (first row)
           (lst-number-or-v row 1 0)
           (lst-number-or-v row 6 0)))
       (remove-if
        '(lambda (row) (= (lst-number-or-v row 6 0) 0))
        (cdr src)))
      (lambda (a b) (string< (nth 0 a) (nth 0 b)))))))

(defun my-sport-journal-add (type value notes)
  (interactive
   (list
    (completing-read
     "Тип: "
     (with-current-buffer (find-file-noselect "~/org/personal/sport/sports-periodic-TODO.org")
       (outline-show-all)
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

(defun my-sport-journal-add-cookie (count)
  "Добавить печенек в журнал"
  (interactive "P")
  (my-sport-journal-add "печенье" (prefix-numeric-value count) ""))

(provide 'my-sport)
