; 高階関数(higher order function), 汎関数(functional)
; => 関数を引数として受け取る関数

; マップ関数(mapping function)

; リストの要素を足す
(defun add-element(x y)
	(if (and (consp x) (consp y))
		(cons
			(+ (car x) (car y))
			(add-element (cdr x) (cdr y))
		)
	)
)

(setf list1 '(1 2 3 4 5) list2 '(10 20 30 40 50))
(format t "list1 : ~A~A" list1 #\LineFeed)
(format t "list2 : ~A~A" list2 #\LineFeed)
(format t "=> ~A~A" (add-element list1 list2) #\LineFeed)


; 上記の処理はmapcar関数を使えば同じことが可能
(format t "(mapcar #'+ '~A '~A)~A=>~A~A" list1 list2
			#\LineFeed (mapcar #'+ list1 list2) #\LineFeed)

(format t "(mapcar (function +) '~A '~A)~A=>~A~A" list1 list2
			#\LineFeed (mapcar (function +) list1 list2) #\LineFeed)

(format t "(mapcar #'* '~A '~A)~A=>~A~A" list1 list2
			#\LineFeed (mapcar #'* list1 list2) #\LineFeed)

(format t "(mapcar (function *) '~A '~A)~A=>~A~A" list1 list2
			#\LineFeed (mapcar (function *) list1 list2) #\LineFeed)


(defun square(x) (* x x))
(format t "(mapcar #'square '~A) =>~A~A"
				list1 (mapcar #'square list1) #\LineFeed)

(format t "(mapcar (function square) '~A) =>~A~A"
				list1 (mapcar (function square) list1) #\LineFeed)

; マップ関数を使うことでデータを変換するだけでなく、リストからデータ
; を取り出すことも簡単になる

(format t "(mapcar #'car '((a . 1) (b . 2) (c . 3)))~A=>~A~A"
	#\LineFeed (mapcar #'car '((a . 1) (b . 2) (c . 3))) #\LineFeed
)

(format t "(mapcar #'cdr '((a . 1) (b . 2) (c . 3)))~A=>~A~A"
	#\LineFeed (mapcar #'cdr '((a . 1) (b . 2) (c . 3))) #\LineFeed
)

(setf l1 '(A B C D) l2 '(1 2 3 4))
(format t "(mapcar (function list) '~A '~A)~A=>~A~A" l1 l2
	#\LineFeed
	(mapcar (function list) l1 l2)
	#\LineFeed
)

; apply : 高階関数の一つ。最初の引数funcを第2引数に適用してその結果を返す。

; (apply function args-list)

(format t "(apply #'+ '(1 2 3)) : ~A~A"
				(apply #'+ '(1 2 3)) #\LineFeed)

(format t "(apply #'car '((1 2 3))) : ~A~A"
				(apply #'car '((1 2 3))) #\LineFeed)

(format t "(apply #'+ 4 5 6 '(1 2 3)) => ~A~A"
				(apply #'+ 4 5 6 '(1 2 3)) #\LineFeed)

(setf ope '(mapcar #'length '("abc" "defg" "hijkl" "mnopqr")))
(format t "~A : ~A~A" ope (eval ope) #\LineFeed)

(setf ope '(apply (function +) (mapcar #'length '("abc" "defg" "hijkl" "mnopqr"))))
(format t "~A : ~A~A" ope (eval ope) #\LineFeed)

; dolistでリスト内文字列要素の総文字数をカウントする
(defun counter(list_)
	(let ((result 0))
		(dolist (var list_ result)
			(setf result (+ result (length var)))
		)
	)
)

(setf ope '(counter '("abc" "defg" "hijkl" "mnopqr")))
(format t "~A : ~A~A" ope (eval ope) #\LineFeed)


; funcall : 最初の引数funcを残りの引数argsに適用し、その結果を返す
; (funcall func args ...)

; ＊funcallを使うことで高階関数(関数を引数として扱う関数)を定義できる！

; 高階関数の定義(誤例)
; (defun execfunc (func arg1 arg2)
;   (func arg1 arg2))

; (execfunc #'+ 1 2) => エラー
; シンボルは変数の値と関数を別々に格納している。この場合、シンボルfunc
; に定義されている関数を実行しようとする。しかし、シンボルfuncには関数が
; 定義されていないのでエラーとなる。実行しようとする関数はfuncに値として
; 格納されているので、リストの先頭にfuncを書いてはいけない。
; ＊変数funcに格納された関数を実行する場合はfuncallを使う！

(defun execfunc(func arg1 arg2) (funcall func arg1 arg2))
(setf ope '(execfunc (function *) 10 20))
(format t "~A : ~A~A" ope (eval ope) #\LineFeed)

; ＠Lispはリストの先頭要素がラムダ式の場合、それを関数として実行する。
;   残りの要素はラムダ式に与える実引数となる。

; ラムダ式(lambda expression) : 無名関数の定義
; (lambda
;   (<仮引数名>...)
;     処理1
;     処理2
;     ...
;     処理M
; )

; 同じ処理が重複する場合が多ければ、修正労力の点から
; 関数にする。
(setf ope '(mapcar #'(lambda (x) (* x x)) '(1 2 3 4 5)))
(format t "~A : ~A~A" ope (eval ope) #\LineFeed)

(setf n1 10 n2 20)
(setf cmp #'<)
(format t "~A ~A ~A : ~A~A" n1 cmp n2 (funcall cmp n1 n2) #\LineFeed)


; 挿入ソート
; ソート済みリストへの挿入処理
(defun insert-element(f n l)
	(cond ((atom l) (cons n nil))
		((funcall f n (car l)) (cons n l))
		(t (cons (car l) (insert-element f n (cdr l))))
	)
)

; 挿入ソート 
(defun insert-sort(f l)
	(if (atom l)
		nil
		(insert-element f (car l) (insert-sort f (cdr l)))
	)
)

(setf ope '(insert-sort (function <) (quote (9 5 3 7 6 4 8))))
(format t "~A : ~A~A" ope (eval ope) #\LineFeed)

(setf ope '(insert-sort (function >) (quote (9 5 3 7 6 4 8))))
(format t "~A : ~A~A" ope (eval ope) #\LineFeed)

; 要素がリストの場合でもラムダ式を用いると簡単に処理可能

(setf ope '(insert-sort #'(lambda (x y) (< (first x) (first y)))
			'((9 1) (5 5) (3 7) (6 4) (2 8))))

(format t "~A~A=>~A~A" ope #\LineFeed (eval ope) #\LineFeed)


