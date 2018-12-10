(defun isort(list_)
	(if (atom list_) list_)

	(insert (car list_) (cdr list_))
)


(defun insert(e list_)
	(cond
		((atom list_) (list e))
		(t (if (> e (car list_))
				(progn
					(format t "~A  ~A~A" list_ e #\LineFeed)
					(append (cons (car list_) e) (cdr list_))
				)
				(append (list e) list_)
		   )
		)
	)
)

(setf hoge '(2 1 4 3))
(format t "~A : ~A~A" hoge (isort hoge) #\LineFeed)

