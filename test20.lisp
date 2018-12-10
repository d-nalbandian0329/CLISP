; 繰り返し

; dotimesマクロ : 指定回数与えられたS式を繰り返し評価する

; (dotimes (var limit result) S式 ...)

; 1. limitを評価。limitの評価結果は0以上でなければならない。

; 2. 評価結果をnとすると、0からn-1までの整数が順番に変数varに
;    代入され、S式を評価する。
; ＊varはレキシカル変数として扱われ、dotimesが評価されている間
;   だけ有効。

; 3. 最後にresultが評価され、その値がdotimesの戻り値になる。
; ＊resultが省略された場合はnilを返す。

(format t "(dotimes (x 5) (print x))")
(dotimes (x 5) (print x))
(princ #\LineFeed)

; 階乗計算をdotimesマクロで行う
(defun fact (num) (setf ret 1) (dotimes (x num ret) (setf ret (* (1+ x) ret))))

(setf num 6)
(format t "Result ~A! = ~A~A" num (fact num) #\LineFeed)

(defun fact(num)
	(let ((ret 1))
		(dotimes (x num ret)
		 	(setf ret (* (1+ x) ret))
		)
	)
)

(setf num 6)
(format t "Result ~A! = ~A~A" num (fact num) #\LineFeed)

; dolistマクロ
; (dolist (var init-form result) S式...)

; 1. 最初にinit-formを評価する
;  ＊評価結果はリストでなければならない！ => リスト以外ならエラー

; 2. doistはリストの要素を順番に変数varに代入してS式を評価する
; ＊リストの要素が無くなったらresultを評価して、その値がdolist
;   の返り値になる。
;   dotimes同様、result指定しなければnilを返す。

(format t "(dolist (x '(1 2 3 4)) (printx))")
(dolist (x '(1 2 3 4)) (print x))

(setf list_ '(2 1 3 1))
(defun count_list(list_)
	(let ((ret 0))
		(dolist (x list_ ret) (incf ret))
	)
)

(princ #\LineFeed)
(format t "~A : ~A~A" list_ (count_list list_) #\LineFeed)

; xyzzy lispではwhileはマクロ定義されているがCommon Lispでは定義されていない。
; つまりCommon Lisp処理系であるCLISPでも定義されていないので自分で実装しよう。
; 可変長引数の指定方法がわからないので(そもそも指定可能なのか?)後回し。
; whileマクロ
; (while 条件式 S式 ...)

; loopマクロ : 最も単純な繰り返し用のマクロ
; 条件式が真であるwhileマクロを回すのと同じ

; また階乗計算
(defun fact(num)
	(let ((ret 1) (i 1))
		(loop
			(if (> i num) (return ret))
			(setf ret (* ret i))
			(incf i)
		)
	)
)


(format t "Result => ~A~A" (fact 4) #\LineFeed)

; doマクロ

; (do ((var [init-form [step-form]]) ...) (end-test [result]) S式 ...)

; 1. 変数varをinit-formの評価結果に初期化する。init-formがない場合は
;    nilに初期化される。

; 2. end-testを評価して結果が真であれば繰り返しを終了する。ここでresultを
;    評価し、その結果がdoの返り値となる。resultがない場合はnilを返す。

; 3. 本体のS式を順番に評価する。

; 4. 変数varの値をstep-formの評価結果に更新する。step-formがない場合は
;    何もしない。
;    ここでわざわざ変数の更新用にsetqやsetfを指定しなくともおk

; 5. 2から4までを繰り返す。

; ＊C言語などのfor文に似ているが、end-testが真になった場合に
;   ループを脱けることに注意！！

(defun fact(num)
	(do								 ; (do
		((i 1 (incf i)) (result 1))  ; 		((var1 init-form1 step-form1) (var2 init-form2 step-form2))
		((> i num) result)			 ; 		(end-test [result])
									 ; 		S式 ...
		(setf result (* result i))   ; )
	)
)

(setf num 7)
(format t "いつもの ~A! = ~A~A" num (fact num) #\LineFeed)

; 簡易版素数算出関数

(defun findNN(num)
	(do
		((i 3 (+ i 2)) (ret '(2)))
		((> i num) ret)

		(if (equal nil (dolist (e ret nil) (if (= (mod i e) 0) (return i))))
			(setf ret (cons i ret))
		)
	)
)

(format t "~A~A" (findNN 20) #\LineFeed)


; 素数チェック
(defun prime-p (n prime-list)
	(dolist (m prime-list t) (if (zerop (mod n m)) (return)))
)

; 素数のリストを返す
(defun prime(n)
	(do
		((prime-list '(2)) (m 3 (+ m 2)))
		((> m n) prime-list)

		(if (prime-p m prime-list)
			(setf prime-list (append prime-list (list m)))
		)
	)
)

(format t "~A~A" (prime 100) #\LineFeed)


; progn : 与えられたS式を順番に実行し、一番最後に評価した値を返す。
; ex.)  (progn (setq a 1) (setq b 2) (setq c 3))

; ifのthen節やelse節は複数のS式を受け付けない。

; (if <条件節>
;   (progn S式A S式B ...)
;	(progn S式a S式b ...)
; )


(princ #\LineFeed)
; prog1 : 最初に評価したS式の値が返り値となる
; (prog1 (setq a 1) (setq b 2) (setq c 3))

(format t "(prog1 (setf a 10) (setf b 20) (setf c 30)) : ~A~A"
	(prog1 (setf a 10) (setf b 20) (setf c 30)) #\LineFeed)

; prog2 : 二番目に評価したS式の値が返り値となる
; (prog2 (setq a 1) (setq b 2) (setq c 3))

(format t "(prog2 (setf a 10) (setf b 20) (setf c 30)) : ~A~A"
	(prog2 (setf a 10) (setf b 20) (setf c 30)) #\LineFeed)


; Stack : 後入れ先出し(LIFO Last In First Out)
; Push : スタックへの要素の追加  Pop : スタックから要素の排出
; ＊consで先頭へ追加、car,cdrで排出を再現


(setf *stack* nil)
; Push new element to stack.
(defun push-stack(data) (setf *stack* (cons data *stack*)))
; Pop first element from stack.
; 最後に無理くりぶち込まなくても、prog1を使えば
; 最初に実行した処理を全体の評価値にしてくれる。
(defun pop-stack() (setf ret (car *stack*)) (setf *stack* (cdr *stack*)) (+ 0 ret))
(defun show-stack() (format t "Stack : ~A~A" *stack* #\LineFeed))

(do
	((i 0 (1+ i)))
	((> i 10) nil)
	(push-stack i)
	(show-stack)
)

(loop
	(if (equal *stack* nil) (return))
	(format t "e : ~A~A" (pop-stack) #\LineFeed)
	(show-stack)
)


(setf *stack* nil)
; プッシュ関数
(defun push-stack(data) (setq *stack* (cons data *stack*)))
; ポップ関数
(defun pop-stack() (prog1 (car *stack*) (setf *stack* (cdr *stack*))))


(do
	((i 1 (+ i 2)))
	((> i 20))
	(push-stack i)
	(show-stack)
)

(loop
	(if (equal nil *stack*) (return))
	(format t "Pop : ~A~A" (pop-stack) #\LineFeed)
	(show-stack)
)

; Common Lispにはスタック操作用のマクロ、push, popが用意されている。
; push item place
; pop place

; ＊push, popは破壊的な操作であることに注意！！

; 以下の二文は等価である
; (push item place) == (setq place (cons item place))

(defun test(list_ nc)
	(do
		((i 1 (+ i 1)) (num 2 (* num 2)))
		((> i nc) nil)
		(push num list_)
		(format t "Stack : ~A~A" list_ #\LineFeed)
	)
	
	(loop
		(if (equal list_ nil) (return))
		(format t "Pop(~A)  Stack : ~A~A" (pop list_) list_ #\LineFeed)
	)
)

(test nil 10)


; 組み合わせの数を求めるプログラム
; 階乗から求めてもおkだけども、簡素な形にしたものから求めた方がよろし

(defun fact(n) (if (<= n 1) 1 (* n (fact (- n 1)))))

(defun comb(n r)
	(if (< n r)
		0
		(/ (/ (fact n) (fact (- n r))) (fact r))
	)
)

(format t "(5,5) : ~A~A" (comb 5 5) #\LineFeed)
(format t "(5,4) : ~A~A" (comb 5 4) #\LineFeed)
(format t "(5,3) : ~A~A" (comb 5 3) #\LineFeed)
(format t "(5,2) : ~A~A" (comb 5 2) #\LineFeed)
(format t "(5,1) : ~A~A" (comb 5 1) #\LineFeed)
(format t "(5,0) : ~A~A" (comb 5 0) #\LineFeed)


; よく考えてみよう:次のように再起定義できるようだ。
; (n,r) = (n,r-1) * (n-r+1) / r

(defun comb(n r)
	(if (or (= r 0) (= r n))
		1
		(* (comb n (- r 1)) (/ (+ (- n r) 1) r))
	)
)

(defun ncr (n r)
	(if (or (= n r) (zerop r))
		1
		(/ (* (ncr n (1- r)) (1+ (- n r))) r)
	)
)

(format t "(5,5) : ~A~A" (comb 5 5) #\LineFeed)
(format t "(5,4) : ~A~A" (comb 5 4) #\LineFeed)
(format t "(5,3) : ~A~A" (comb 5 3) #\LineFeed)
(format t "(5,2) : ~A~A" (comb 5 2) #\LineFeed)
(format t "(5,1) : ~A~A" (comb 5 1) #\LineFeed)
(format t "(5,0) : ~A~A" (comb 5 0) #\LineFeed)

(format t "(5,5) : ~A~A" (ncr 5 5) #\LineFeed)
(format t "(5,4) : ~A~A" (ncr 5 4) #\LineFeed)
(format t "(5,3) : ~A~A" (ncr 5 3) #\LineFeed)
(format t "(5,2) : ~A~A" (ncr 5 2) #\LineFeed)
(format t "(5,1) : ~A~A" (ncr 5 1) #\LineFeed)
(format t "(5,0) : ~A~A" (ncr 5 0) #\LineFeed)


; "~3D"     整数値を3桁で出力する指定子
; (terpri)  標準出力へ改行を出力

; パスカルの三角形
(defun pascal(n)
	(dotimes (i (1+ n))
		(dotimes (j (1+ i)) (format t " ~3D" (ncr i j)))
		(terpri)
	)
)

(pascal 10)
