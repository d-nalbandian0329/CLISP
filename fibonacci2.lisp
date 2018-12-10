; 再帰制御版

(setf *fibo-table-limit* 100
		*fibo-table* (make-array (1+ *fibo-table-limit*)))

(defun fibo_(n)
	(if (<= 0 n 1)
		1
		(if (aref *fibo-table* n)
			(aref *fibo-table* n)
			(setf (aref *fibo-table* n) (+ (fibo_ (1- n)) (fibo_ (- n 2)))))))

(defun fibo(n)
	(if (<= 0 n *fibo-table-limit*)
		(fibo_ n)))

(format t "~%Fibonacci :")
(dotimes (i *fibo-table-limit*)
	(format t " ~A" (fibo i)))
(format t "~%")
