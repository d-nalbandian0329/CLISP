; loopマクロ : 指定した任意の数の式を無限ループで実行する。
;              指定された式を最初から順番に実行し、すべて実行し終えた後に
;			   そのまま最初に戻って実行する。

; (loop {form}*)

; form : 任意の式、複数指定可能。
; returnマクロを利用するとループを抜けることができる。

(setq stn 1 edn 10 ret 0)

(set 'i stn)

(loop
	(if (> i edn) (return))
	(setq ret (+ ret i))
	(setq i (+ i 1))
)

(format t "sigma(~A, ~A) = ~A~A" stn edn ret #\LineFeed)


(set 'i edn)
(setq lst nil)  ; lstに()をぶち込む

(format t "Before : ~A~A" lst #\LineFeed)
(loop
	(if (< i stn) (return))
	(setq lst (cons i lst))   ; 変数の時と同様、追加後のリストを再代入しないと意味無
	(setq i (- i 1))
)
(format t "After  : ~A~A" lst #\LineFeed)


; block特別式, tagbody特別式, go特別式を用いることでloopマクロを
; 簡単に実装することができる。

(setq stn 10 edn 20)
(set 'i stn)

(setq lst nil)  ; 空リスト作成

(block nil
	(tagbody loop
		(if (> i edn) (return))
		(setq lst (cons i lst))
		(setq i (+ i 1))
		(go loop)
	)
)
(format t "loopマクロ再現~Alist : ~A~A" #\LineFeed lst #\LineFeed)


; ユーザによる関数の定義はdefunマクロで行う
; defun function-name lambda-list form*

; function-name : 新しく定義する関数の名前
; lambda-list   : 呼び出されるときにリストから関数に渡される仮パラメータ。
;				  この変数は評価されずに、関数の呼び出し元から指定された
;				  データを格納するためのもの。関数が特に値を受け取らない場合
;				  は空のリストを指定する。
; form : 新しく定義する関数の本体になる部分。任意の数の式を記述することができる。

; ＠defunマクロの実行結果は常に関数名function-nameに指定した記号になる。
;	定義済みの関数名が指定された場合、既存の関数の上から新しい定義に上書きされる。

; ＊特別式に指定されている識別子は使えないことに注意！！

(format t "defunマクロ実行結果 : ~A~A" (defun test () 10) #\LineFeed)

(format t "~A~A" (test) #\LineFeed)

(defun addnum (a b) (+ a b))

(format t "~A + ~A = ~A~A" 10 20 (addnum 10 20) #\LineFeed)


; 簡単なリスト作成関数

(defun make_list (arg_list stnum ednum)
	(set 'stn stnum)
	(set 'edn ednum)
	(if (> stnum ednum) (progn (set 'stn ednum) (set 'edn stnum)))

	(set 'i edn)

	(loop
		(setq arg_list (cons i arg_list))
		(setq i (- i 1))
		(if (< i stn) (return arg_list))
	)
)

(setq lst1 () lst2 ())

(setq lst1 (make_list lst1 10 20))

(setq lst2 (make_list lst2 4 -5))

(format t "lsit1 : ~A~A" lst1 #\LineFeed)

(format t "lsit2 : ~A~A" lst2 #\LineFeed)


