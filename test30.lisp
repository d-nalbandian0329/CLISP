;Pascal's Triangle

(defun _pascal(n)
	(let ((table) (buff))
		(setf table (make-array (1+ (* n 2)) :initial-element 0))
		(setf (aref table n) 1)
		(_show table)

		(dotimes (i (1- n))
			(setf buff (copy-seq table))
			(dotimes (j (- (length table) 2))
				(setf (elt buff (1+ j))
					(+ (elt table j) (elt table (+ j 2)))))
			(setf table (copy-seq buff))
			(_show table))))

(defun _show(seq)
	(dotimes (i (length seq))
		(format t "~3A" (if (zerop (elt seq i)) " " (elt seq i))))
	(format t "~%"))

(defun pascal(n)
	(if (<= 1 n 10) (_pascal n)))

(pascal 10)
