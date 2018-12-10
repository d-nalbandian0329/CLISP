; Quick Sort

(defun quicksort(tree)
	(unless (atom tree)
		(let ((p (car tree)) listL listR)
			(dolist (num (cdr tree))
				(if (< p num)
					(push num listR)
					(push num listL)
				)
			)

			(append (quicksort listL) (cons p (quicksort listR)))
		)
	)
)

(setf ope '(quicksort '(3 2 4 1 6 9)))

(format t "~A => ~A~A" ope (eval ope) #\LineFeed)

