; 真偽値判定は関数で行う。関数は以下の通り。

; =   等しい
; /=  等しくない
; <   小なり
; >   大なり
; <=  以下
; >=  以上

; 戻り値 : 真(t)   偽(nil)
; tとnilは変数として使えない特殊な定数

(format t "(= 10 10)  : ~A~A" (= 10 10) #\LineFeed)

(format t "(/= 10 10) : ~A~A" (/= 10 10) #\LineFeed)

(format t "(/= 10 20) : ~A~A" (/= 10 20) #\LineFeed)

(format t "(< 10 20)  : ~A~A" (< 10 20) #\LineFeed)

(format t "(< 20 10)  : ~A~A" (< 20 10) #\LineFeed)

(format t "(> 10 20)  : ~A~A" (> 10 20) #\LineFeed)

(format t "(> 20 10)  : ~A~A" (> 20 10) #\LineFeed)

(format t "(<= 10 20) : ~A~A" (<= 10 20) #\LineFeed)

(format t "(<= 20 10) : ~A~A" (<= 20 10) #\LineFeed)

(format t "(>= 10 20) : ~A~A" (>= 10 20) #\LineFeed)

(format t "(>= 20 10) : ~A~A" (>= 20 20) #\LineFeed)

; 引数が3つ以上ある場合は各引数が比較演算子に従う大小順で
; 並んでいればT, そうでなければnil。

(format t "(< 1 2 3) : ~A~A" (< 1 2 3) #\LineFeed)

(format t "(< 1 3 2) : ~A~A" (< 1 3 2) #\LineFeed)

(format t "(< 2 3 1) : ~A~A" (< 2 3 1) #\LineFeed)


; 複数の式(forum)の比較
; not関数    not x          nilが渡された場合はT
;                           それ以外が渡された場合はnilを返す

; andマクロ  and {forum}*   引数にnilが指定された場合はnil,それ以外の場合は
;                           最後に指定された引数を返す。

; orマクロ   or {forum}*    引数にnil以外の値が発見されればその値が結果に
;                           なり、全ての引数がnilの場合にのみnilを返す。


; andマクロ、orマクロは対象発見時に評価を終了する。
; つまり、短絡評価(ショートサーキット)するようになっている。

(format t "(not T)   : ~A~A" (not t) #\LineFeed)
(format t "(not NIL) : ~A~A" (not nil) #\LineFeed)
(format t "(not 10)  : ~A~A" (not 10) #\LineFeed)

(format t "(and 1 10 100)           : ~A~A" (and 1 10 100) #\LineFeed)
(format t "(and nil 10 100)         : ~A~A" (and nil 10 100) #\LineFeed)
(format t "(and T NIL T)            : ~A~A" (and t nil t) #\LineFeed)
(format t "(and T)                  : ~A~A" (and t) #\LineFeed)
(format t "(or NIL NIL NIL)         : ~A~A" (or nil nil nil) #\LineFeed)
(format t "(or T NIL T)             : ~A~A" (or t nil t) #\LineFeed)
(format t "(or NIL 10 NIL 20)       : ~A~A" (or nil 10 nil 20) #\LineFeed)
(format t "(or NIL (not T) NIL 100) : ~A~A" (or nil (not t) nil 20) #\LineFeed)

; 式(forum)の評価について : 実行後の値がその式の値となる
; (setq x 10) の値は10

(format t "(and NIL (= (setq x 100) 100)) : ~A~A"
            (and nil (= (setq x 100) 100)) #\LineFeed)

(format t "(and T (= (setq x 100) 100)) : ~A~A"
            (and t (= (setq x 100) 100)) #\LineFeed)

