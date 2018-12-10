; LISt Processor

; LISPの式はsymbolic expression又はS-式と呼ばれる。
; S-式はアトム, リスト, 文字列の3つの有効なオブジェクトによって構成される。
; LISPのプログラムはインタプリタ上で実行するか、コンパイル済みのコード
; を実行するかどちらか選べます。インタプリタはソースコードをループして
; チェックします。このループはREPL(Read-Eval-Print Loop)と呼ばれ、内容も
; そのままです。コードを読み込み、評価し、プログラムから返された値を表示する
; という一連の流れをループするのです。

; ＊LISPは前置記法を用いていることに注意！！
; ((60 * 9 / 5) + 32)  =>  (+ (/ (* 60 9) 5) 32)


; LISPプログラムの評価は2段階に分けられる

; 1. 読み込み用プログラムで、テキスト形式のプログラムをLISPオブジェクトに
;    翻訳。
; 2. 評価プログラムにより、これらのオブジェクトに関して、言語のセマンティクスの
;    の実装。


; 評価プロセスは以下のステップに従う

; i.  ) 読み込みプログラムは、文字列をLISPオブジェクトやS-式に翻訳する。
; ii. ) 評価プログラムはS-式から作られたLISPのフォームの文法を定義する。
; 		この評価の2段階目でS-式をLISPのフォームに決める文法が定義される。
; iii.) 評価プログラムは、有効なLISPフォームを引数として受け取り、値を返す
;		関数として機能する。これが、LISPの式を丸括弧内に収めなければならない
;		理由である。つまり、評価プログラムへ式やフォーム全体を引数として送る
;		ためである。

(write-line "Hello,world!!")

(write-line "I am at 'Tutorials Point'! Learning LISP.")

; LISPプログラムは基本となる以下の3要素から成る。
; atom   : 数値や文字列。特殊記号も含む。

; ex.)
;		hello-form-tutorials-point
;		name
;		123008907
;		*hello*
;		Block#221
;		abc123

; list   : atomの連なりを丸括弧で囲ったもの。listをこれに含めても良い。
; ex.)
;		( i am a list )
;		(a ( a b c) d e fgh)
;		(father tom ( susan bill joe))
;		(sun mon tue wed thur fri sat)
;		( )

; string : ダブルクウォーテーションで囲まれた記号の集合
; ex.)
;		"I am a string"
;		"a ba c de f ghi"
;		"Please enter the following details : "
;		"Hello from 'Tutorials Point'!"

; セミコロン記号はそれ以降がコメントであることを示します。

; ＊LISPの式は大文字と小文字が区別されないことに注意！！

; LISPは関数の引数に含まれるすべてを評価しようとする。次の3つの要素のみ
; 定数であり、常に自身の値を返す。
; 1. 数値
; 2. 文字 t    真偽値の真を表す。
; 3. 値   nil  真偽値の偽を表し、空のリストも意味する。

; 名前やsymbolは英数字から成ります。ただし、空白、左右の丸括弧、ダブル・シングル
; クウォート、バックスラッシュ、コンマ、コロン、セミコロン、垂直バーは使用不可。
; それらの記号を名前として用いるためにはバックスラッシュでエスケープする必要がある。

; 名前に数字を用いることはできるが、数字だけで構成することはできない。数値として
; 認識されてしまうためである。同様にピリオドを名前に含むことはできるが、それのみ
; にはできない。


; LISPは関数の引数やlistの要素を含めて、すべてを評価します。
; 時には、atomやlistをリテラルで扱って、関数呼び出し時に評価や扱われたく無い
; こともあります。そのためにはatomやlistの前にシングルクウォーテーション記号を
; つける必要があります。

(write-line "Single quote used, it inhibits evaluation.")

(write (quote (* 2 3)))

(write-line " ")

(write-line "Single quote not used, so expression evaluated.")

(write (* 2 3))


; LISPでは変数は種類分けされないが、データオブジェクトはされる。
; LISPのデータ型は次のように分類される。

; スカラー型 : 数値型、文字、symbolなど。

; データ構造 : list、ベクトル、ビットベクトルや文字列など。

; 明示的に変数を宣言しない限り、変数はいかなるLISPオブジェクトの値も
; 格納することができる。LISPの変数はデータ型を指定する必要は無いが、
; loopの拡張やメソッドの宣言などの幾つかの状況では役に立つ。

; システム定義の型とは別に、ユーザ定義の型を作成できる。defstruct関数
; を用いて構造型が定義されると、その名前は優子な型symbolになる。

(setq x 10)
(setq y 34.567)
(setq ch nil)
(setq n 123.78)
(setq bg 11.0e+4)
(setq r 124/2)

(print x)
(print y)
(print ch)
(print n)
(print bg)
(print r)



(defvar x 10)
(defvar y 34.567)
(defvar ch nil)
(defvar n 123.78)
(defvar bg 11.0e+4)
(defvar r 124/2)

(print (type-of x))
(print (type-of y))
(print (type-of ch))
(print (type-of n))
(print (type-of bg))
(print (type-of r))

; 床関数
; floor num1 [num2]
; i <= num1 < i + 1  を満たす整数iを求める。

(setf num1 10.03 num2 2.21)
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

; 1+  インクリメントするための演算子
; 1-  デクリメントするための演算子

(format t "(1+ 2)   : ~A~A" (1+ 2) #\LineFeed)

(format t "(1+ 3.2) : ~A~A" (1+ 3.2) #\LineFeed)

(format t "(1- 2/3) : ~A~A" (1- 2/3) #\LineFeed)

(format t "(1- #C(2 3.1)) : ~A~A" (1- #C(2 3.1)) #\LineFeed)

; (float num)  整数や分数を浮動小数点数に変換する
(format t "(floor 1/3) ~A~A" (floor 1/3) #\LineFeed)

(format t "初期値 ~A : ~A~A" 'num (setf num 100) #\LineFeed)
; incf : インクリメント演算後に、値を更新
; (setf a (1+ a)) と等価
(incf num)
(format t "Increment  1 => ~A : ~A~A" (quote num) num #\LineFeed)

(incf num 10)
(format t "Increment 10 => ~A : ~A~A" (quote num) num #\LineFeed)

(incf num 3)
(format t "Increment  3 => ~A : ~A~A" (quote num) num #\LineFeed)


; decf : デクリメント演算後に、値を更新
; (setf b (1- b)) と等価
(decf num)
(format t "Decrement  1 => ~A : ~A~A" (quote num) num #\LineFeed)

(decf num 100)
(format t "Decrement 100=> ~A : ~A~A" (quote num) num #\LineFeed)

(decf num 5)
(format t "Decrement  5 => ~A : ~A~A" (quote num) num #\LineFeed)

