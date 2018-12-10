; Stack composed in vector.

(setf *stack-size* 10
		*stack* (make-array (1+ *stack-size*)))
(setf (aref *stack* 0) 0)


(defun isEmpty(stack) (if (zerop (aref stack 0)) t nil))
(defun isFull(stack) (if (>= (1+ (aref stack 0)) *stack-size*) t nil))

(defun pop_(stack)
	(if (isEmpty stack) 
		nil 
		(prog1 (aref stack (aref stack 0) )
				(setf (aref stack 0) (1- (aref stack 0))))))

(defun push_(stack e)
	(if (isFull stack)
		nil
		(progn (setf (aref stack 0) (1+ (aref stack 0)))
				(setf (aref stack (aref stack 0)) e))))

(defun getTop(stack)
	(if (isEmpty stack)
		nil
		(aref stack (aref stack 0))))

(format t "Top  : ~:A~%" (getTop *stack*))
(format t "Push :")
(dotimes (i 12)
	(format t " ~A"(push_ *stack* i)))
(format t "~A" #\NewLine)

(format t "Top  : ~:A~%" (getTop *stack*))

(format t "Pop  :")
(dotimes (i 12)
	(format t " ~A" (pop_ *stack*)))
(format t "~A" #\LineFeed)



; ベクタによるスタックの実装
; 初期化
(defun init-stack(n)
	(setf *stack-size* n
			*top* 0
			*stack* (make-array n)))

; データをスタックに積む
(defun push-down(data)
	(when (< *top* *stack-size*)
			(setf (aref *stack* *top*) data)
			(incf *top*)))

; スタックからデータを取り出す
(defun pop-up()
	(when (plusp *top*)
		(decf *top*)
		(aref *stack* *top*)))


(setf *size* 10)

(init-stack *size*)

(dotimes (i *size*)
	(push-down i))

(format t "~%Pop :")
(dotimes (i *top*)
	(format t " ~:A" (pop-up)))
(format t "~%")



; Common Lispにはベクタをスタックとして扱うための
; 関数がある

; vector-push関数 : 指定したベクタのフィルポインタの位置にデータを書き込む
; (vector-push item vector)
; スタック(ベクタ)が満杯であるときはnilを返す

; フィルポインタは最終データの一つ後を指す

; vector-pop関数 : フィルポインタの値を一つ減らしてその位置に
;                  格納されているデータを取り出す
; (vector-pop vector)
; データをpopできない場合はエラーを起こす

; ＠上記の関数は通常のベクタに適用することはできないので注意!!
; => make-arrayでベクタを生成するときに :fill-pointer キーワードに
;    nil以外の値を指定する必要がある。
;    ・指定可能な値は[0, vector_size]とt
;    ・tを指定するとベクタのサイズと同じ値になる

; :fill-pointer キーワードで指定した値はfill pointer(stack pointer)
; の初期値とる。
; fill pointer == 0            スタックが空
; fill pointer == vector size  スタックが満杯

; fill-pointer関数 : 指定スタックのフィルポインタを取得する
; (fill-pointer vector)


(setf ope '(setf stack (make-array 10 :fill-pointer 0)))
(format t "~A : ~A~%" ope (eval ope))

(setf ope '(fill-pointer stack))
(format t "~A : ~A~%" ope (eval ope))

(setf ope '(vector-push 123 stack))
(format t "~A : ~A~%" ope (eval ope))

(setf ope '(vector-push "hoge" stack))
(format t "~A : ~A~%" ope (eval ope))

(setf ope '(fill-pointer stack))
(format t "~A : ~A~%" ope (eval ope))

(format t "Pop : ~A ~A~%" (vector-pop stack) (vector-pop stack))



; スタックはリストと異なりサイズに限りがある
; 最初に設定したベクタのサイズを超えて要素を追加する場合
; => ・そのベクタをmake-array関数で生成する際に
;      :adjustableキーワードにnil以外の値を指定する
;    ・要素を追加する際にvector-push-extend関数を使う

; vector-push-extend関数 : :adjustableキーワードでnil以外の値を
;                          指定したベクタの最大要素数を超えて要素を追加

; (vector-push-extend item vector &optional extension)

; item : 追加要素
; extension : ベクタに追加する要素の個数
;             オプション引数なので省略可能

; :fill-pointerにtを指定 => fill pointerの値がスタックサイズと同じになる
; => スタックが満杯状態
; :adjustableにtを指定 => nil以外の指定 => スタック拡張関数が使用可能
(setf ope '(setf stack (make-array 3 :fill-pointer t :adjustable t)))
(format t "~A : ~A~A" ope (eval ope) #\LineFeed)

(setf ope '(fill-pointer stack))
(format t "fill pointer : ~A~%" (eval ope))

(setf ope '(vector-push 1 stack))
(format t "~A : ~A~A" ope (eval ope) #\NewLine)

(setf ope '(vector-push-extend 1 stack 1))
(format t "~A : ~A~%" ope (eval ope))

(format t "stack : ~A~%" stack)


; 以前作成した素数を求めるプログラムのベクタ版

; 素数のチェック
; 奇数のみ判定対象、素数リスト内要素で割り切れなければ
; 対象は素数
(defun prime-p(n k prime-list cnt)
	(dotimes (m cnt)
		(cond ((zerop (mod n (aref prime-list m))) (return))
				((<= k (aref prime-list m)) (return t)))))

; n個の素数のベクタを返す
; 素数テーブルの先頭に2を入れておく
(defun prime-vector(n)
	(let ((prime-list (make-array n))
			(cnt 0))
		(setf (aref prime-list cnt) 2)
		(incf cnt)
		(do ((m 3 (+ m 2)))
			((<= n cnt) prime-list)
			(when (prime-p m (sqrt m) prime-list cnt)
				(setf (aref prime-list cnt) m)
				(incf cnt)))))


(format t "Prime Number : ~%~A~%" (prime-vector 10))


