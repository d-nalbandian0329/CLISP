; initList

(defun initList(lst start end)
	(if (<= start end)
		(progn (set 'stn start) (set 'edn end))
		(progn (set 'stn end)   (set 'edn start))
	)

	(setf i edn)

	(block nb
		(tagbody
			next
			(if (< i stn) (return-from nb lst))
			(setf lst (cons i lst))
			(setf i (- i 1))
			(go next)
		)
	)
)

(setf lst1 nil lst2 nil)

(format t "lst1 : ~A~A" (setf lst1 (initList lst1 11 20)) #\LineFeed)

(format t "lst2 : ~A~A" (setf lst2 (initList lst2 31 40)) #\LineFeed)

(defun jointList(lst1 lst2)
	(if (<= (length lst1) 1)
		(cons (car lst1) lst2)
		(cons (car lst1) (jointList (cdr lst1) lst2))
	)
)

(format t "=> : ~A~A" (jointList lst1 lst2) #\LineFeed)


