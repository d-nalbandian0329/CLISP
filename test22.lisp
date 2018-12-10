; 挿入対象の数nとソート済みのリストlist_を
; 受け取る
(defun test(n list_ cmp)
		(format t "N : ~A  List : ~A~A" n list_ #\LineFeed)

		(if (atom list_)
			n

			(if (funcall cmp n (car list_))
				(append (list n (car list_)) (cdr list_))
				(cons (car list_) (test n (cdr list_) cmp))
			)
		)
)

(format t "(test 4 '(2 3 5 6)) (function <=) : ~A~A"
				(test 4 '(2 3 5 6) (function <=)) #\LineFeed)

(format t "(test 7 '(2 3 5 6)) (function <=) : ~A~A"
				(test 7 '(2 3 5 6) (function <=)) #\LineFeed)

(defun hoge(list_)
	(if (atom list_)
		
		(cons (car ))
	)
)


