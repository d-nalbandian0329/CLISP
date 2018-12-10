; Merge Sort

(defun _merge(l1 l2 func)
	(setf ret nil)
	(loop
		(cond
			((atom l1) (return (append ret l2)))
			((atom l2) (return (append ret l1)))
			(t
				(setf ret
					(append ret
						(if (funcall func (car l1) (car l2))
							(prog1 (list (car l1)) (setf l1 (cdr l1)))
							(prog1 (list (car l2)) (setf l2 (cdr l2))))))))))

;(setf hoge '(1 3 5)
;		fuga '(2 4 6 8))
;(format t "~A + ~A => ~A~%" hoge fuga (_merge hoge fuga #'<))

;(setf ope '(setf foo (mapcar #'list '(1 2 3 4 5 6))))
;(format t "~A => ~A~A~%" ope (eval ope) #\LineFeed)

(defun merge_sort(src_tree func)
  (setf tree (mapcar #'list src_tree))

  (loop
    (setf buff nil)
    (setf npair (floor (/ (length tree) 2.0)))
    (dotimes (i npair)
      (setf buff (append buff (list (_merge (car tree) (cadr tree) func))))
      (setf tree (cddr tree)))

    (if (not (atom tree)) (setf buff (append buff (list (car tree)))))

    (setf tree buff)
    (if (<= (length tree) 1) (return)))

  (car tree)
)

(setf ope '(merge_sort '(3 2 4 1 5 1 9 6) #'<))
(format t "~A => ~A~A" ope (eval ope) #\LineFeed)

