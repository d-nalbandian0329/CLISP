; マクロを使うことで標準的なLISPの文法を拡張できる。
; 技術的にはマクロはS式を実引数としてとり、評価済みの
; LISPの式を返す関数です。

; defmacroマクロ : マクロを定義する

; (defmacro macro-name (parameter-list) 
; 	"Optional Documentation String"
;   body-form
; )

; Definition macro.
(defmacro setTo10(num) (setq num 10))

(setq x 25)
(format t "Before : ~A ~A~A" (quote x) x #\LineFeed)
(setTo10 x)
(format t "After  : ~A ~A~A" (quote x) x #\LineFeed)

; LISPではどの変数もsymbolとして表される。変数名は
; symbolとして名付けられ、symbolの記憶セルに格納される。

; グローバル変数は、LISPシステムの初めから終わりまで永久的に値を
; 持ち、実際には新しい値を指定するまで残り続けます。グローバル変数
; は通常、defvarマクロを使って定義します。

(defvar x 234)
(format t "~A : ~A~A" 'x x #\LineFeed)

; LISPには変数宣言時の型指定はない為、値を変数に代入するときは
; 直接指定すれば良い。

; setfマクロ, psetfマクロ

; setf {pair}* => result*

; psetf {pair}* => nil

; pair::= place newvalue

; place    : 格納対象
; newvalue : 式
; result   : 最後の格納先からの戻り値、又はペアが無ければnil


; (setf place_1 newvalue_1
;		place_2 newvalue_2
;		...
;		place_N newvalue_N
; )

; 上記の式は以下の式と等価

; (progn (setf place_1 newvalue_1)
; 		 (setf place_2 newvalue_2)
; 		 ...
;		 (setf place_N newvalue_N)
; )

(setq x (cons 'a 'b) y (list 1 2 3))

(format t "--- Before ---~A" #\LineFeed)
(format t "x : ~A~A" x #\LineFeed)
(format t "y : ~A~A" y #\LineFeed)

(setf (car x) 'x (cadr y) (car x) (cdr x) y)

(format t "---  After  ---~A" #\LineFeed)
(format t "x : ~A~A" x #\LineFeed)
(format t "y : ~A~A" y #\LineFeed)


(defun fact(num) (if (<= num 1) 1 (* num (fact (- num 1)))))

(setf hoge 5)
(format t "~A : ~A~A" 'hoge hoge #\LineFeed)
(format t "~A! = ~A~A" hoge (fact hoge) #\LineFeed)

(setf lst nil)
(setf i 1)
(block nil
	(tagbody
		next
		(setf lst (cons i lst))
		(setf i (+ i 1))
		(if (< i 10) (go next))
	)
)

(defun countList(lst) (if (atom lst) 0 (+ 1 (countList (cdr lst)))))

(format t "lst : ~A~A" lst #\LineFeed)
(format t "Number of elements : ~A~A" (countList lst) #\LineFeed)


; append
(defun add(lst1 lst2) (if (= lst1 nil) (cons (car lst1) lst2) (add (cdr lst1) lst2)))

(defmacro makelist(lst stn edn)
	(set 'i edn)
	(loop
		(if (< i stn) (return))
		(setf lst (cons i lst))
		(setf i (- i 1))
	)
)

(setf list1 nil list2 nil)

(makelist list1 10 19)

(makelist list2 5 14)

(format t "List_1 : ~A~A" list1 #\LineFeed)

(format t "List_2 : ~A~A" list2 #\LineFeed)

(add list1 list2)
(format t "List_1 : ~A~A" list1 #\LineFeed)


; 床関数
; floor num1 [num2]
; i <= num1 < i + 1  を満たす整数iを求める。

(setf num1 10.03 num2 20.21)
(format t "~A / ~A => ~A~A" num1 num2 (floor num1 num2) #\LineFeed)

; 天井関数
; ceiling num1 [num2]
; i - 1 < num1 <= i  を満たす整数iを求める

(format t "~A / ~A => ~A~A" num1 num2 (ceiling num1 num2) #\LineFeed)

; 小数部分の切り捨て
; truncate num1 [num2]
; 小数点以下を切り捨てる

(format t "~A / ~A => ~A~A" num1 num2 (truncate num1 num2) #\LineFeed)

; round num1 [num2]
; 近い方の整数に丸める。0.5ちょうどの場合は偶数方向に丸める。

(format t "~A / ~A => ~A~A" num1 num2 (round num1 num2) #\LineFeed)

(setf num1 1.5)
(format t "~A => ~A~A" num1 (round num1) #\LineFeed)

; ＊引数num2が与えられるた場合は(/ num1 num2)を評価し
;   その結果を整数に変換する


