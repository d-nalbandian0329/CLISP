; リスト内要素入れ替えの復習

(defun subst_(old new list_)
	(cond
		((equal old list_) new)
		((atom list_) list_)
		(t (cons (subst_ old new (car list_)) (subst_ old new (cdr list_))))
	)
)

(setf list_ '(a b (a b c) (d a e) z))

(format t "~A => ~A~A" list_ (subst_ 'a 'z list_) #\LineFeed)

