; recursive search

;(defun getelement(list_) (if (consp list_) (progn (format t "~A~A" (car list_) #\LineFeed) (getelement (cdr list_)))))
;(setf list_ (quote (1 2 (2 1) 5)))
;(getelement list_)

(defun dive(list_)
	(block b
		(setf i 1 end (length list_))
		(format t "List :")
		(tagbody
			next
			(if (> i end) (return-from b))
			(format t " ~A" (car list_))
			(setf list_ (cdr list_))
			(incf i)
			(go next)
		)
	)
)

(dive '(1 2 3 4 5))

