
(setf src_target '(2 1 5 4 3 6 9 10))
(setf target (mapcar #'list src_target))

(format t "~A~%" target)

(loop
  (setf buff nil)
  (setf npair (floor (/ (length target) 2.0)))

  (dotimes (i npair)
    (setf buff (append buff (list (append (car target) (cadr target)))))
    (setf target (cddr target))
    (format t "BUFF : ~A~%" buff))

  (setf target buff)
  (format t "TGT : ~A~%BUFF : ~A~%" target buff)

  (if (<= (length target) 1) (return))
)

(format t "~A~%" (car target))

