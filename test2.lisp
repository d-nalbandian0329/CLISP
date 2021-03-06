; Lispの基本

; >式
; 式にはLispシステムに計算させるべき数値などのデータを入力する。
; Lispは定められた手順に従ってコードを実行し、結果を出力する。
; 通常、Lispシステムに入力する式は、複数の関数から成り立つ。

; (関数名 引数1 引数2 ...)

; 関数とは何らかの仕事を行う処理コードの集まり。いくつかの
; データを受け取り、そのデータを処理して結果を返す。
; この時、関数が受け取るデータを引数と呼ぶ。関数が返す値を
; 関数の値と呼ぶ。
; 関数式は必ず（）で囲み、関数名に続いて引数を列挙する。
; ただし、関数名と引数の間は1以上の空白で区切られている必要がある。

; @ Common Lispではシステムが予め提供しなければならない標準関数を
;   定めている。

; 演算用関数の例
;
; + &rest numbers
;
; - number &rest more-numbers
;
; * &rest numbers
;
; / number &rest more-numbers

; ＊関数定義オプション&restは,任意の数の引数を受け取ることができるという意味

; 上の定義より、加算と乗算はデータの個数が0でも許される。


(print(+ 1 2 3 4 5))

(- 100 10 1)

(* 1 2 3 4 5)

(/ 100 10 2)





