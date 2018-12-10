; reduce 関数  sequenceの各要素に対して関数Fを次のように適用する
; (A1 A2 A3 ... An-1 An) = reduce F => (F (F ... (F (F A1 A2) A3) ...) An-1) An)

; :from-end t の場合
; (A1 A2 A3 ... An-1 An) = reduce F => (F A1 (F A2 (F A3 (F An-1 An) ...)))

; ex.)  F(x,y) = x + y ならば  reduce => A1 + A2 + A3 + ... + An-1 + An

(setf ope '(reduce #'+ '(1 2 3 4 5 6)))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(reduce #'list '(1 2 3 4 5 6)))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(reduce #'list '(1 2 3 4 5 6) :from-end t))
(format t "~A => ~A~%" ope (eval ope))


; ;initial-value キーワードを指定した場合

; :initial-value G の場合
; (G A1 A2 A3 ... An-1 An) = reduce F => (F (F ...(F (F G A1) A2) ...) An-1) An)

(setf ope '(reduce #'list '(1 2 3 4 5 6) :initial-value 0))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(reduce #'list '(1 2 3 4 5 6) :initial-value 7 :from-end t))
(format t "~A => ~A~%" ope (eval ope))



; ＠ ・:initial-valueが無くて列の要素が一つしかない場合
;    　reduceはその値を返す。
;
;    ・列が空で:initial-valueが指定されている場合
;    　reduceは:initial-valueの値を返す。
;
;    ・列が空で:initial-valueも指定されていない場合、関数functionを
;      引数なしで呼び出してその結果を返す。


(setf ope '(reduce #'+ '(1)))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(reduce #'+ nil :initial-value 2))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(reduce #'+ nil))
(format t "~A => ~A~%" ope (eval ope))


; reduce関数を利用したlength関数とappend関数の実装
; ＊ :initial-value 0 がポイント!!

(setf ope '(reduce #'(lambda (x y) (+ x 1)) '(a b c) :initial-value 0))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(reduce #'cons '(a b c d) :initial-value '(1 2 3 4 5) :from-end t))
(format t "~A => ~A~%" ope (eval ope))


; sort 関数   データを昇順または降順に整列させる操作
; sort sequence predicate

; merge 関数  整列済みの二つのデータ列を一つの整列済みデータ列にまとめること
; merge result-type seq1 seq2

(setf ope '(setf ary #(9 1 8 2 7 3 4 6 5)))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)

(setf ope '(sort ary (function <)))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)

(setf ope '(setf ary #((9 a) (1 b) (8 c) (2 d) (7 e) (3 f) (4 g) (6 h) (5 i))))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)

(setf ope '(sort ary #'< :key #'car))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)

(setf ope '(merge 'list '(1 3 5 7 9) '(2 4 6 8) #'<))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)


; Pascal's triangle (list)
(defun pascal-sub(num-list)
	(if (second num-list)
		(cons (+ (first num-list) (second num-list))
			(pascal-sub (rest num-list)))
		'(1)))

(defun pascal-list(n)
	(let ((buf))
		(dotimes (i n)
			(setf buf (pascal-sub (cons 0 buf)))
			(print buf))))

(pascal-list 4)


(defun pascal(n)
	(let ((buff (make-array (1+ n) :initial-element 0)))
		(setf (aref buff 1) 1)
		(dotimes (i n)
			(do ((j (1+ i) (1- j)))
				((zerop j))
				(format t " ~3D" (setf (aref buff j)
					(+ (aref buff j) (aref buff (1- j))))))
			(terpri))))

(format t "~%~%")
(pascal 6)

