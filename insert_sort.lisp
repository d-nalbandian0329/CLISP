; 挿入ソート


; 比較関数
(setf cmp (function <))

; 自分をバラしながら、引数要素と比較
; 対象リストはソート済み

(defun insert(e tree)
	(cond
		((or (atom tree) (funcall cmp e (car tree)))
			(append (list e) tree)
		)

		(t 
			(append (list (car tree)) (insert e (cdr tree)))
		)
	)
)

;(setf hoge '(1 2 4 5))

;(format t "~A => ~A : ~A~A" hoge 3 (insert 3 hoge) #\LineFeed)
;(format t "~A => ~A : ~A~A" hoge 6 (insert 6 hoge) #\LineFeed)

(defun isort(tree)
	(if (atom tree)
		tree
		(insert (car tree) (isort (cdr tree)))
	)
)

(setf fuga '(10 2 3 1 4 2 3 5))

(format t "~A => ~A~A" fuga (isort fuga) #\LineFeed)

(setf ope '(isort '(9 5 3 7 6 4 8)))
(format t "~A~A=>~A~A" ope #\LineFeed (eval ope) #\LineFeed)

