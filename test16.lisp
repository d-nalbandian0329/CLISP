; 全ての引数の最大公約数を求める
; Greatest Common Divisor
; (gcd para1 ...)

(format t "(91 49)     : ~A~A" (gcd 91 49) #\LineFeed)

(format t "(63 42 35)  : ~A~A" (gcd 63 42 35) #\LineFeed)

; 全ての引数の最小公倍数を求める
; Least Common Multiple
; (lcm para1 ...)

(format t "(14 35)     : ~A~A" (lcm 14 35) #\LineFeed)

(format t "(1 2 3 4 5) : ~A~A" (lcm 1 2 3 4 5) #\LineFeed)

; Euclidean algorithm
(defun gcd_(num1 num2)
	(set 'big num1)
	(set 'small num2)

	(if (< num1 num2)
		(progn (set 'small num1) (set 'big num2))
	)

	(loop
		(setf ret (mod big small))

		(if (= ret 0) (return small))

		(set 'big small)
		(set 'small ret)
	)
)
; Least Common Multiple
(defun lcm_(num1 num2) (setf e (gcd_ num1 num2)) (* e (/ num1 e) (/ num2 e)))


(setf num1 36 num2 12)
(format t "gcd(~A ~A) = ~A~A" num1 num2 (gcd_ num1 num2) #\LineFeed)

(setf num1 15 num2 31)
(format t "gcd(~A ~A) = ~A~A" num1 num2 (gcd_ num1 num2) #\LineFeed)

(setf num1 256 num2 36)
(format t "gcd(~A ~A) = ~A~A" num1 num2 (gcd_ num1 num2) #\LineFeed)

(setf num1 36 num2 12)
(format t "lcm(~A ~A) = ~A~A" num1 num2 (lcm_ num1 num2) #\LineFeed)

(setf num1 15 num2 31)
(format t "lcm(~A ~A) = ~A~A" num1 num2 (lcm_ num1 num2) #\LineFeed)

(setf num1 256 num2 36)
(format t "lcm(~A ~A) = ~A~A" num1 num2 (lcm_ num1 num2) #\LineFeed)


; 関数定義
; (defun function-nam (parameter-list)
; 	処理1
;	処理2
;	...
;	処理M
; )

; ＊関数名はシンボルで指定する。文字列や数値では不可能。
; ＊lambda-list : (parameter-list)
; 関数内のS式はリストの順に評価されて、関数の評価結果は
; 最後に評価されたS式の結果となる



(defun square(x) (* x x))
(setf num 17)
(format t "~A^2 = ~A~A" num (square num) #\LineFeed)

; 関数の仮引数はその関数が実行されている間のみ有効である。
; つまり、関数宣言部外で同名の変数を使用していても問題無し。
; このような変数をレキシカル変数(Lexical variable)
; または、局所変数(Local variable)という。

; ずっと値が残る変数 : スペシャル変数(Special variable)
; 					   グローバル変数(Global variable)

; スペシャル変数を定義するときは、変数名を
; *変数名* というようにアスタリスクで囲む慣習がある。

; 同名のレキシカル変数とスペシャル変数が存在するとき
; 外側のスコープの変数は隠蔽される

(setf x 200 y 100)
(defun foo(x) (print x) (print y))

(foo 100)

; let : レキシカル変数を定義する
; (let ((変数1 初期値1)
;	    (変数2 初期値2)
;	    ...
;		(変数M 初期値M))
;	   S式1
;	   ...
;	   S式M
; )

; 初期値が省略されると変数はnilに初期化される

; 関数の仮引数のように与えられた名前をレキシカル変数として扱い
; 後に続くS式を順番に評価する。変数は初期値を評価した順に
; 初期化される。

; ＊定義されたレキシカル変数はletの実行が終了するまで有効である。
;   letは最後に評価したS式の値を評価結果として返す。

(princ #\LineFeed)

(setq x 10)
(let ((x 100)) (format t "X :~A~A" x #\LineFeed))
(format t "X :~A~A" x #\LineFeed)



