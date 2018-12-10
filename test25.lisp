; 配列 : Common Lisp の配列はS式であればどんなデータ
;        でも格納することができる

; make-array関数 : dimensionsには非負の整数を要素とする
;                  リストを与える
; ＊キーワードで初期値を指定しなければ、初期値はnil
; make-srray dimensions

; dimensions : リストの長さが配列の次元
;              各要素がその次元の要素数を示す


(setf ope '(setf table (make-array 5)))
(format t "~A => ~A~A" ope (eval ope) #\LineFeed)

; 1次元目が0-1, 2次元目が0-2
(setf ope (quote (setf table (make-array (quote (2 3))))))
(format t "~A => ~A~A" ope (eval ope) #\LineFeed)

(setf ope '(setf table (make-array '(2 3 4))))
(format t "~A => ~A~A" ope (eval ope) #\LineFeed)


; 配列型データは数値や文字列と同じく自己評価フォーム
; # の次の数字が次元を表し、Aが配列であることを表す
; 特に一次元の配列をベクタ(vector)という

; 第一次元がリストのトップレベルに相当することに注意！！


; Common Lispでは : で始まる引数をキーワード(keyword)という

(defmacro show_com(ope)
	(format t "~A => ~A~A" (eval ope) (eval (eval ope)) #\LineFeed))
(defun show_com2(ope)
	(format t "~A => ~A~A" ope (eval ope) #\LineFeed))

(setf ope '(setf table (make-array 5 :initial-element 0)))
(show_com ope)
(show_com2 ope)

(setf ope '(setf table (make-array 5 :initial-element '(1 2 3 4 5))))
(show_com ope)
(show_com2 ope)

; 各要素に異なる値を初期値として渡す場合は
; initial-contents で初期値をリストに与える


; aref関数 : 配列要素の取り出し
; (aref array subscripts ...)

; subscripts : 添字。arrayの次元の範囲内に収まっていなければならない。


(setf ope '(setf table (make-array 3 :initial-contents '(10 20 30))))
(show_com ope)
(setf ope '(aref table 0))
(show_com ope)

(setf ope '(setf table (make-array '(2 3) :initial-contents
											'((10 20 30) (40 50 60)))))
(show_com ope)
(setf ope '(aref table 0 0))
(show_com ope)
(setf ope '(aref table 1 2))
(show_com ope)

; 添字指定は0基底であることに注意！！
; 第二引数以降は一次元目から順に添字指定

; Common Lispでは配列に値を代入する際にsetfを使用する
; ＊処理系によっては配列専用の関数が用意されている。

; !! Common Lisp ではデータ代入はsetfで統一することを推薦 !!

; setf : アクセス関数が示す位置にデータを代入する

; (setf アクセス関数 データ)

; アクセス関数 : 評価した時にデータを取り出す関数
; => 配列の場合はaref関数
; => 変数は評価するとその値を返すのでアクセス関数とみなせる

; setfによる指定配列要素への値代入は破壊的な処理

(setf ope '(setf ary (make-array 3)))
(show_com ope)

(setf ope '(setf (aref ary 1) 10))
(show_com ope)

(show_com (quote ary))

(setf ope '(setf ary (make-array '(2 3))))
(show_com ope)

(setf ope '(setf (aref ary 1 2) 10))
(show_com ope)

(show_com (quote ary))



; Fibonacci
; f(n) = f(n-1) + f(n-2)
; f(0) = f(1) = 1

(defun fibo(n) (if (<= n 1) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))
(do ((i 2 (1+ i)))
	((> i 10))
	(format t "fibo(~A) = ~A~A" i (fibo i) #\LineFeed)
)

(defun fibo(x)
	(if (or (= x 0) (= x 1))
		1	
		(+ (fibo (- x 1)) (fibo (- x 2)))))
(do ((i 0 (1+ i)))
	((> i 10))
	(format t "fib(~A) = ~A~A" i (fibo i) #\LineFeed))

; 二重再帰 : 自分自身を再帰処理の中で二回呼び出すこと

; フィボナッチ数列は添字を引数xに対応させることで
; 高速化できる
; (<= 0 i 1)  これでiが0以上1以下であることを条件にできる、、
; 引数xを添字として見てフィボナッチ数列をベクタに格納
; テーブルの作成
(defun fibo(x)
	(if (<= x 1)
		(make-array 2 :initial-element '(1 1))

		(let ((ret (make-array (1+ x) :initial-element 0)))
			(setf (aref ret 0) 1)
			(setf (aref ret 1) 1)

			(do ((i 2 (1+ i)))
				((>= i (length ret)) ret)

				(setf (aref ret i) (+ (aref ret (- i 1)) (aref ret (- i 2))))
			)
		)
	)
)

(setf table (fibo 10))

(format t "~AFibo :" #\LineFeed)

(do ((i 0 (1+ i)))
	((>= i (length table)))
	(format t " ~A" (aref table i))
)
(format t "~A" #\LineFeed)


; フィボナッチ関数(ベクタ版その1)
; グローバル変数(スペシャル変数)の定義
(setf *fibo-table-limit* 100
	  *fibo-table*		 (make-array (1+ *fibo-table-limit*)))

; フィボナッチ関数本体
; 初期値指定してないので初期値nil
; よって、配列要素にアクセスした時にテーブル内要素に
; 値が格納されていればその値を、そうでなければ計算した
; 値を返す。

(defun fibo1_ (n)
	(if (<= 0 n 1)
		1
		(if (aref *fibo-table* n)
			(aref *fibo-table* n)
			(setf (aref *fibo-table* n)
					(+ (fibo1_ (- n 1)) (fibo1_ (- n 2)))))))

; フィボナッチ関数
(defun fibo1 (n)
	(if (<= 0 n *fibo-table-limit*) (fibo1_ n)))

(format t "Fibo1 :")
(do ((i 0 (1+ i)))
	((> i 20))
	(format t " ~A" (fibo1 i))
)
(format t "~A" #\NewLine)



; グローバル変数の定義
(setf *fibo-table-limit* 100
	  *fibo-table*		 (make-array (1+ *fibo-table-limit*)))

; ベクタの初期化
(defun init-fibo-table()
	; 先頭2つ, 添字0と1の中身は1を入れる
	(setf (aref *fibo-table* 0) 1
		  (aref *fibo-table* 1) 1)

	; 0から100までで101個, 0と1は設定済みだから101-2=99個
	(dotimes (i (1- *fibo-table-limit*))
		(setf (aref *fibo-table* (+ i 2))
			(+ (aref *fibo-table* (+ i 1)) (aref *fibo-table* i)))))

(defun fibo2(n)
	(if (<= 0 n *fibo-table-limit*)
		(aref *fibo-table* n)))

(init-fibo-table)
(format t "(init-fibo-table)")

(format t "~AFibo2 :" #\NewLine)
(dotimes (i (1+ *fibo-table-limit*))
	(format t " ~A" (fibo2 i)))
(format t "~A" #\LineFeed)

