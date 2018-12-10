; List 6 : リストのマージ

(defun merge-list (f l1 l2)
  (cond
    ((atom l1) l2)
    ((atom l2) l1)
    ((funcall f (car l1) (car l2))
      (cons (car l1) (merge-list f (cdr l1) l2)))
    (t (cons (car l2) (merge-list f l1 (cdr l2))))))


(setf ope '(merge-list #'< '(2 4 6 8) '(1 3 5 7 9)))
(format t "~A~%=>~A~%" ope (eval ope))

(setf ope '(merge-list #'< '(10 20 30) '(1 2 3 4)))
(format t "~A~%=>~A~%" ope (eval ope))


(defun merge-sort(f l n)
  (cond
    ((= n 1) (list (car l)))
    ((= n 2)
      (let ((x (first l)) (y (second l)))
        (if (funcall f x y) (list x y) (list y x))))
    (t (let ((m (truncate n 2)))
          (merge-list f
            (merge-sort f l m)
            (merge-sort f (nthcdr m l) (- n m)))))))


(setf ope '(merge-sort #'< '(9 5 3 7 6 4 2 8) 8))
(format t "~A~%=>~A~%" ope (eval ope))

(setf ope '(merge-sort #'> '(9 5 3 7 6 4 2 8) 8))
(format t "~A~%=>~A~%" ope (eval ope))


