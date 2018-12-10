; 条件による分岐処理

; if特別式
; (if test then [else])

; test : 条件式
; then : 条件式がnil以外の任意の値である場合に実行される式
; else : 条件式がnilの場合に実行される式(省略可能)

; ＊if特別式が評価する式は任意の数ではなく、一つの式だけであることに注意！
;   つまり、thenやelseは一つの式でなければならないということ！！

(if t (princ 10) (princ 100))

(if 1 (print 10) (print 100))

(if nil (princ 10) (print nil))
(princ #\LineFeed)

(setq x 10)

(if (< x 100) (setq y 100) (setq y x))
(format t "x : ~A  y : ~A~A" x y #\LineFeed)

(if (= y 100) (setq x y) (setq x 0))
(format t "x : ~A  y : ~A~A" x y #\LineFeed)


; progn {form}* : 引数となる式を複数与えた場合、先頭から全ての
;                 式が評価されるが、この特別式の結果となるのは
;                 最後に評価された値のみ。その他の式は結果に
;                 反映されないため、通常は何らかの副作用を持つ
;                 式となる。progn特別式に引数を与えなかった場合
;                 は何も評価せずにnilを返す。

(format t "(progn (setq x 10) (setq y 20) (setq 30)) : ~A~A"
            (progn (setq x 10) (setq y 20) (setq z 30)) #\LineFeed)

(format t "x : ~A  y : ~A  z : ~A~A" x y z #\LineFeed)

; if特別式で複数処理を行う場合
; (if test (progn form...) (progn form...))

; whenマクロ : if特別式を任意の数の引数受け取れるようにしたもの
; ＊以下の2式は等価である
; (when test {form}*)
; (if test (progn form...))

; unlessマクロ : whenマクロと逆の働きをする
; ＊以下の2式は等価である
; (unless test {form}*)
; (if (not test) (progn form...))

(setq x 25)

(format t "~A : ~A~A"
	x
	(if (= (mod x 15) 0) "FizzBuzz" (progn (if (= (mod x 3) 0) "Fizz") (if (= (mod x 5) 0) "Buzz")))
	#\LineFeed
)


(format t "~A : ~A~A"
	x
	(if (= (mod x 15) 0) "FizzBuzz" (when t (if (= (mod x 3) 0) "Fizz") (if (= (mod x 5) 0) "Buzz")))
	#\LineFeed
)


(format t "~A : ~A~A"
	x
	(if (= (mod x 15) 0)
		(setq str "FizzBuzz")
		
		(progn
    		(if (= (mod x 3) 0) (setq str "Fizz")
			)
			(if (= (mod x 5) 0) (setq str "Buzz")
			)
		)
	)
	#\LineFeed
)

(unless nil (princ "NIL"))

