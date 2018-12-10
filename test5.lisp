; データの保存
; 変数 : プログラムの実行時に値が決定する。
; 定数 : プログラムコードを記述した時点で値が決定する。

; 識別子 : データのアドレスの代わりに人間にわかりやすい文字の
;          羅列を用いたもの。
; 変数名 : アルファベット、数値、記号で構成される識別子。
;          a-z,A-Z,0-9,+,-,*,/.@,$,%,^,&,_,=,<,>,~,.

; 変数への代入

; setq {var form}*
; {var form}* に変数の識別子と変数に保存する式を指定

; setq特別式(special form)
; 通常の関数は呼び出された際に引数を評価した後に、対象の内容を参照する。
; しかし、setq特別式では引数の評価が行われない。
; ex.) (setq x 10)
;       x と 10 の評価は行われず、そのまま識別子xに10を代入する。
; ＊つまり、変数名を変数に代入可能!!

; ＊Lisp言語において識別子とは、単なるテキスト上で識別するためのシンボル
;   ではなく、独立した型として認識されている。

(setq x 11)

(setq 1+ 21)

(setq +$ 31)

(setq $$$ 41)

(format t "~A * ~A + ~A  * ~A = ~A" x 1+ +$ $$$ (+ (* x 1+) (* +$ $$$)))
(princ #\LineFeed)

; 一度に複数の変数への代入が可能
(setq a 10 b 100 c 1000)
(format t "~A : ~A~A~A : ~A~A~A : ~A~A" #\a a #\LineFeed #\b b #\LineFeed #\c c #\LineFeed)

(princ #\LineFeed)

(setq y (* x 10) z (/ x 2.0))
(format t "~A : ~A~A~A : ~A~A~A : ~A~A" #\x x #\LineFeed #\y y #\LineFeed #\z z #\LineFeed)

; quote特別式 : 引数に指定された識別子をデータとして返す。
;               この変数名などの型を記号型、または記号と呼ぶ。
; (quote 識別子)

; set関数 : 変数に値を代入する
; (set symbol-value)

; ＊setq特別式とは違って、set関数は関数なので引数の評価が行われる！！

; そのため、quote特別式でsymbol型にした識別子を受けとって代入先に指定
; しなければならない！間接参照代入している。

(set (quote hoge) 512)
(format t "~A : ~A~A" "hoge" hoge #\LineFeed)

(set (quote fuga) (quote hoge))
(format t "~A : ~A~A" "fuga" fuga #\LineFeed)

; quote特別式はとても冗長でコードの可読性を著しく下げる。
; そのため、'識別子 という形でシングルクウォーテーションを
; 識別子の直前に置くことで同じ効果が得られることになっている。
; ＠以下の2式は等価
; (set (quote var) 100)
; (set 'var 100)

; 記号型の値を評価しても得ることができるのは、記号型が表す変数の識別子。
; では記号型の値から、それが表す変数の値にアクセスするためには？

; symbol-value関数 : 記号型の値から変数の値を求める。
; (symbol-value 記号)

(format t "~A ~A : ~A~A" "hoge" (quote hoge) (symbol-value 'hoge) #\LineFeed)

(format t "~A ~A : ~A~A" "fuga" (quote fuga) (symbol-value fuga) #\LineFeed)


