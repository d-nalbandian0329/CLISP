; 制限あり

; Special Variable
; 配列は添字と数列番号合わせるので0から100
(setf *fibo-table-limit* 100
		*fibo-table* (make-array (1+ *fibo-table-limit*)))

(defun init-table()
	(setf (aref *fibo-table* 0) 1
			(aref *fibo-table* 1) 1)

	(dotimes (i (1- *fibo-table-limit*))
		(setf (aref *fibo-table* (+ i 2))
			(+ (aref *fibo-table* (1+ i)) (aref *fibo-table* i)))))

(defun fibo(n)
	(if (<= 0 n *fibo-table-limit*)
		(aref *fibo-table* n)))

(init-table)
(format t "Fibonacci :")
(dotimes (i *fibo-table-limit*)
	(format t " ~A" (fibo i)))
(format t "~A" #\NewLine)
