; 

(defun my-subst(old new tree)
	(cond
		((equal old tree) new)
		((atom tree) tree)
		(t (cons (my-subst old new (car tree))
					(my-subst old new (cdr tree))))
	)
)

(setf tree '(a b (a c) (d . a)))
(format t "Before : ~A~A" tree #\LineFeed)

(setf ret (my-subst 'a '1 tree))
(format t "After  : ~A~A" ret #\LineFeed)

