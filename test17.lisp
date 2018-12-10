; 条件分岐

; 述語 : 真(true)か偽(false)を返す関数。Common Lispでは
;		 偽 : nil   真 : nil以外の値
; ＊述語は条件を満たす場合はシンボルtを返す。

; equal関数 : 2つの引数が同じ値か調べる。
; (equal num1 num2)

(format t "~A : ~A~A" "(equal (+ 1 2 3) 6)" (equal (+ 1 2 3) 6) #\LineFeed)
(format t "~A : ~A~A" "(equal (+ 2 3 4) 7)" (equal (+ 2 3 4) 7) #\LineFeed)
(format t "~A : ~A~A" "(equal 4 4.0)" (equal 4 4.0) #\LineFeed)
(format t "~A : ~A~A" "(equal 'a 'a)" (equal 'a 'a) #\LineFeed)
(format t "~A : ~A~A" "(equal 'a 'b)" (equal 'a 'b) #\LineFeed)
(format t "~A : ~A~A" "(equal '(a b c) '(a b c))"
						(equal '(a b c) '(a b c)) #\LineFeed)
(format t "~A : ~A~A" "(equal '(a b c) '(a b d))"
						(equal '(a b c) '(a b d)) #\LineFeed)

; eq関数 : 2つの引数が全く同じであるかどうかを調べる。
;          コンピュータのメモリの番地を調べて、全く同じ
;          シンボルであればtを返す。
;          そのため、全く同じ値でもeqはtを返さない場合がある。
;		   Lispは数値atomを生成する場合、同じ数値でも違うメモリ番地に
;          実体を割り当てる場合があるため。リストの場合も同様。

(format t "~A : ~A~A" "(eq 'a 'a)"
						(eq 'a 'a) #\LineFeed)
(format t "~A : ~A~A" "(eq 1d100 1d100)"
						(eq 1d100 1d100) #\LineFeed)

(format t "~A : ~A~A" "(eq '(a b c) '(a b c))"
						(eq '(a b c) '(a b c)) #\LineFeed)
 

; eq   2つの引数が同一オブジェクト(アドレスが等しい場合)に真
; eql  2つの引数がeqを満たす、または同じ型で同じ値の数値や
;      同じ文字列であれば真を返す

; equal   eqlを満たす、またはequalを満たすリストや内容が等しい文字列
;         であれば真を返す

; equalp  equalを満たす、または型が違っても同じ値の数値、
;         文字や文字列では大文字・小文字を区別しない、
;		  equalpを満たすリストや配列であれば真を返す

; 等しいとされる数値の範囲の広さ
; eq < eql < equal < equalp


(setf str "hoge" str2 "hoge")
(format t "~A : ~A~A" "(eq str str)" (eq str str) #\LineFeed)
(format t "~A : ~A~A" "(eq str str2)" (eq str str2) #\LineFeed)
(format t "~A : ~A~A" "(eql str str2)" (eql str str2) #\LineFeed)
(format t "~A : ~A~A" "(equal str str2)" (equal str str2) #\LineFeed)

(format t "~A : ~A~A" "(equalp 4 4.0)" (equalp 4 4.0) #\LineFeed)
(format t "~A : ~A~A" "(equalp 4 #C(4 0))" (equalp 4 #C(4 0)) #\LineFeed)

; 数値比較を行う述語
; =, /=, <, <=, >, >=
; = は引数の型を区別せずに比較を行う


; データ型を調べる述語
; atom		: アトムか？
; numberp	: 数値か？
; integerp	: 整数か？
; floatp	: 浮動小数点数か？
; symbolp	: シンボルか？
; stringp	: 文字列か？
; listp		: リストか？
; consp		: コンスセルか？

; ＊各述語の末尾に付くpは、predicate(述語)の頭文字より
; ＊いずれの述語も引数を1つ取り、引数が条件を満たしたらt
; 　そうでなければnilを返す。

; nilは偽を表すシンボルだが、空リストも表す
; => listpとsymbolpをnilに適用するとどうなるのだろうか？

(format t "(listp nil)   : ~A~A" (listp nil) #\LineFeed)

(format t "(symbolp nil) : ~A~A" (symbolp nil) #\LineFeed)

(format t "(atom nil)    : ~A~A" (atom nil) #\LineFeed)

(format t "(consp nil)   : ~A~A" (consp nil) #\LineFeed)


; 型指定子 : Common Lispにおいてデータ型を表現するためのシンボル

; number  : 数値
; integer : 整数
; float   : 浮動小数点数
; symbol  : シンボル
; string  : 文字列
; list    : リスト
; cons    : コンスセル

; typep   : データ型を調べる述語
; (typep object data-type-symbol)

; type-of : 引数のデータ型を型指定子で返す

(format t "~A : ~A~A" "(typep '(1 2 3) 'list)"
						(typep '(1 2 3) 'list) #\LineFeed)
(format t "~A : ~A~A" "(typep \"abcdef\" 'string)"
						(typep "abcdef" 'string) #\LineFeed)
(format t "~A : ~A~A" "(typep 100 'integer)"
						(typep 100 'integer) #\LineFeed)
(format t "~A : ~A~A" "(typep 100 'float)"
						(typep 100 'float) #\LineFeed)
(format t "~A : ~A~A" "(typep 'a 'symbol)"
						(typep 'a 'symbol) #\LineFeed)


(format t "~A : ~A~A" "(type-of '(1 2 3))"
						(type-of '(1 2 3)) #\LineFeed)
(format t "~A : ~A~A" "(type-of \"abcdef\")"
						(type-of "abcdef") #\LineFeed)
(format t "~A : ~A~A" "(type-of 100)"
						(type-of 100) #\LineFeed)
(format t "~A : ~A~A" "(type-of 100)"
						(type-of 100) #\LineFeed)
(format t "~A : ~A~A" "(type-of 'a)"
						(type-of 'a) #\LineFeed)

; 複数の述語を組み合わせる場合は、andマクロとorマクロを用いる

; andマクロ : 与えられたS式を左から順番に評価する。S式の評価結果
;			  がnilであれば残りのS式を評価せずにnilを返す。
;			  ただし最後までS式がnilに評価されなかった場合は
;			  一番最後のS式の評価結果を返す。

; (and S式1 S式2 S式3 ...)

; orマクロ : 与えられたS式を左から順番に評価する。S式の評価結果が
;			 nil以外の場合は、残りのS式を評価せずにその評価結果を返す。
;			 全てのS式がnilに評価された場合はnilを返す。

; (or S式1 S式2 S式3 ...)

; not関数 : 引数がnilであればt、それ以外であればnilを返す。

(defun check-number(x) (and (> x 10) (<= x 20)))

(defun check-number-else(x) (or (<= x 10) (> x 20)))

(format t "(check-number 15) : ~A~A" (check-number 15)
										#\LineFeed)

(format t "(not (check-number 15)) : ~A~A" (not (check-number 15))
										#\LineFeed)

(format t "(check-number-else 15) : ~A~A" (check-number-else 15)
										#\LineFeed)

; evenp : 引数が偶数ならtを返す述語
; oddp  : 引数が奇数ならtを返す述語

(defun even-or-odd(x)
	(if (evenp x)			; 条件部
		(print "偶数です")	; then節
		(print "奇数です")	; else節
	)
)


; whenマクロ : ifのelse節がnilである場合、ifの代わりに使用できる。
;			   最初にtestを評価し、その結果がnilであればその後ろの
;			   S式を評価せずにnilを返す。そうでなければ、S式を順番に
;			   評価し、最後のS式の評価結果を返す。

; (when test S式1 S式2 S式3 ...)

; unlessマクロ : ifのthen節がnilである場合、ifの代わりに使用できる。
;				 述語が偽(nil)に評価されたとき、引数のS式を順番に
;				 評価する。

; (unless test S式1 S式2 S式3 ...)

; (when (not test) ...) == (unless test ...)
; (when test ...)       == (unless (not test) ...)

; ifのthen節やelse節は1つのS式しか受け付けないのに対して
; whenマクロとunlessマクロは複数のS式を受け取ることが可能。


; condマクロ
; (cond (条件部A S式A1 S式A2 ...)
;		(条件部B S式B1 S式B2 ...)
;		...
;		(t		 S式T1 S式T2 ...)
; )

; condマクロの返り値は最後に評価されたS式の評価結果となる。

; ＊各節の先頭には条件をチェックする述語があり、条件が成立した場合は
;	残りのS式を評価する。条件が不成立であれば次の節に移る。
; ＊一度節が選択されるとそれ以降の節は評価されない。
; ＊上記の最後の節のように述語部分をtにした節を用意すれば、他の節が
;   選ばれなかった場合に行う処理を定義できる。

(defun data-type(obj)
	(cond
		((integerp obj) (print "整数です"))
		((floatp obj)   (print "浮動小数点数です"))
		((stringp obj)  (print "文字列です"))
		((listp obj)    (print "リストです"))
		((consp obj)    (print "コンスセルです"))
		((symbolp obj)  (print "シンボルです"))
		(t              (print "その他のデータ型です"))
	)
)

(data-type (list 1 2 3))
(data-type '(1 2 3))
(data-type 3)
(data-type 3.0)

(data-type "a")

(princ #\LineFeed)


; 昔のLispには繰り返し用の関数がなかったらしく、再帰定義(recursive definition)
; を使って繰り返しを実現していたらしい。(^^;

; ＠再帰定義(Recursive definitotn)  再帰呼び出し(Recursive call)
; => 関数がその中で自分自身を呼び出すこと

; 階乗
; 0! = 1
; x! = x * (x-1)!

(defun fact(n) (if (> n 1) (* n (fact (- n 1))) 1))

(setf num 6)
(format t "~A! = ~A~A" num (fact num) #\LineFeed)


; リストの要素をカウントする
; length関数があるけど練習として再帰定義で自作

(defun len(list_)
	(if (eql (car list_) nil)
		0
		(+ 1 (len (cdr list_)))
	)
)

; 空リストになるということはnilであるということで、
; するとデータ型がatomになる。よってそこで再帰呼び出し
; が終了するようにしている。

; (defun my-length(x)
;	(if (atom x)
;		0
;		(+ 1 (my-length (cdr x)))
;	)
; )

(setf list_ '(1 2 3 4 5))
(format t "~A : ~A~A" list_ (len list_) #\LineFeed)


; リストを結合するappend関数を練習のために再帰定義で自作

(defun append_(listA listB)
	(if (atom listA)
		listB
		(cons (car listA) (append_ (cdr listA) listB))
	)
)

(setf listA '(2 3 1 4) listB '("hoge" "fuga" "foo"))

(format t "listA : ~A~A" listA #\LineFeed)
(format t "listB : ~A~A" listB #\LineFeed)

(format t "append => ~A~A" (append_ listA listB) #\LineFeed)


(defun dive(list_)
	(if (atom list_) list_ (cons (car list_) (cdr list_)))
)

(format t "dive=> ~A~A" (dive '(1 2 ("A" "B" "C") (3 2 4) 4 5)) #\LineFeed)


; リスト内要素を置き換えるsubst関数を再帰定義で自作
(defun subst_(old new list_)
	(if (atom list_)
		list_
		(progn
			(set 'e (car list_))

			(if (listp e) (subst_ old new e))

			(if (equal e old) (set 'e new))

			(cons e (subst_ old new (cdr list_)))
		)
	)
)

(setf ret '(a b (a c) (d . a)))

(format t "Before : ~A~A" ret #\LineFeed)
(set 'ret (subst_ 'a 1 ret))
(format t "After  : ~A~A" ret #\LineFeed)

; 

(defun my-subst(old new tree)
	(cond
		((equal old tree) new)
		((atom tree) tree)
		(t (cons (my-subst old new (car tree))
					(my-subst old new (cdr tree))))
	)
)

(setf tree '(a b (a c) (d . a)))
(format t "Before : ~A~A" tree #\LineFeed)

(setf ret (my-subst 'a '1 tree))
(format t "After  : ~A~A" ret #\LineFeed)

