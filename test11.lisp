; ジャンプ処理 : 条件などによらず特定のタイミングで指定した式に制御を
;				移行させるような処理を行うときに必要

; tagbody特別式 : 引数にtag又は式を指定できる。引数にリストが指定されている場合
;				  は評価対象の式であると認識され、そうでなければtagであると認識
;				  される。tagとstatementは任意の数だけ指定可能。最後に評価された
;				  式がtagbodyの戻り値になる。

; (tagbody {tag|statement}*)

; tagbody内ではgo特別式を利用でき、go特別式が評価されるとtagに移動する。
; このとき、goで指定するtagはtagbodyで指定されていなければならない。

; (go tag)

; ex.) (tagbody (go label) (str1) label (str2))

; 1.tagbody特別式は引数を先頭から評価する
; 2.上の式では1つ目の(go label)によって、制御がlabelに移行
;   そのため、(str1)は無視されてlabelの次の処理、(str2)が
;   評価される。

; ＠tagbodyとgoは特別式なので、tagに記号などを表記してもそれは評価されずに
; 　そのまま認識されることに注意！！


; while もどき
(setq i 0)
(tagbody
	next
	(if (< i 10) (progn (format t "~A~A" i #\LineFeed) (setq i (+ i 1)) (go next)))
)

; 同上
(setq i 0)
(tagbody
	next
	(when (< i 10) (format t "~A~A" i #\LineFeed) (setq i (+ i 1)) (go next))
)

; tagbodyが入れ子になってる場合は、最も内側のtagが優先される！
; つまり、同名tagが外側のスコープにある場合は外側のものが隠蔽される！

(setq x 10)
(setq y 10)
(setq z 1000)

(format t "x: ~A  y : ~A  z : ~A~A" x y z #\LineFeed)

(tagbody
	(tagbody
		(go label)
		(setq x 0)
	label
		(setq y 0)
	)
label
	(setq z 0)
)

(format t "x: ~A  y : ~A  z : ~A~A" x y z #\LineFeed)



; 複数の式を実行する場合に、prognマクロを用いると無条件に全ての式を
; 実行するが、処理によって途中で中断する必要がある場合もある。その
; 場合は、ブロック構造が必要になる。

; block特別式 : ブロックを定義できる。prognのように任意の式を評価するが
;				必要に応じて途中で中断できる。blockの値は最後に評価された
;				式の値になる。

; block name {name}*

; name : ブロックの名前
; form : 任意の数の式を指定可能

; blockスコープ内であればブロック名を用いてブロックの評価を中断することができる。
; 評価を中断してブロックを抜け出すには、return-from特別式を用いる。


; return-from name [result]

; name   : 中断するブロックの名前
; result : ブロックの結果となる値。省略した場合はNILが結果となる。


(format t "(block test 1 10 100) => ~A~A"
				(block test 1 10 100) #\LineFeed)

(format t "(block test 1 (return-from test 12345) 100) => ~A~A"
				(block test 1 (return-from test 12345) 100) #\LineFeed)

(format t "(block test 1 (return-from test) 100) => ~A~A"
				(block test 1 (return-from test) 100) #\LineFeed)


; returnマクロ
; (return [result])  (return-from nil [result])と同義


; NILというブロック名を発見できないのでエラーが発生する
;(format t "(block test 1 (return 12345) 100) => ~A~A"
;				(block test 1 (return 12345) 100) #\LineFeed)

(format t "(block nil 1 (return 12345) 100) => ~A~A"
				(block nil 1 (return 12345) 100) #\LineFeed)

; 外側のスコープのブロック名を指定することで、一気に抜けることができる。
(format t "(block outside (block inside (return-from outside) 100) 1000) : ~A~A"
			(block outside
				(block inside
					(return-from outside 12345)
					10
				)
			)
			#\LineFeed
)

