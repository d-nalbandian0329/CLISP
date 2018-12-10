; 階層的条件 : 他言語でよく見られるif - else if - else文による階層構造
;			   関数型言語であるLISPでこれを頻繁に記述するとコードの可読性
;              が著しく下がってしまう。

; condマクロ : testがnil以外の値であればformを評価する。formには
;			   任意の数の式を記述することができる。condマクロの
;			   返す値は最後に評価したformの式になる。
; ＠特徴：testとformを任意の数だけ記述できる！！
;         condマクロはnil以外の条件が検出されるまで、指定された式を
;		  次々と評価する。

; (cond{(test{form}*)}*)

; (cond (条件1  式...)
;	    (条件2  式...)
;       (条件3  式...)
;       (条件4  式...)
;   	...
; )


(setq x 2)
(princ (cond ((= x 1) "TsukuyomiMode") ((= x 2) "NekomimiMode")))
(princ #\LineFeed)

(setq x 1)
(princ (cond ((= x 1) "TsukuyomiMode") ((= x 2) "NekomimiMode")))
(princ #\LineFeed)

(setq x 0)
(princ (cond ((= x 1) "TsukuyomiMode") ((= x 2) "NekomimiMode")))
(princ #\LineFeed)


; 単一の値について階層的に調べる
; caseマクロ : キー式の値とリストを比較して実行する式を決定する。
;              C言語のswitch文に該当。

; case keyform{({({key}*)|key}{form}*)}*

; keyform : キーとなる値
; ＊設定されたkeyformの値と任意の数のリストkeyを比較して、一致した場合
;   formを実行する

; keyはリストで指定可能。リストにした場合は、要素のどれか一つがヒットすればおk

(setq x 1)
(format t "~A~A" (case x ((1 10) (quote TsukuyomiMode)) ((2 20) (quote NekomimiMode)))
					#\LineFeed)
(setq x 2)
(format t "~A~A" (case x ((1 10) (quote TsukuyomiMode)) ((2 20) (quote NekomimiMode)))
					#\LineFeed)
(setq x 0)
(format t "~A~A" (case x ((1 10) (quote TsukuyomiMode)) ((2 20) (quote NekomimiMode)))
					#\LineFeed)
; ＊最後のkeyリストにt, 又はotherwiseを指定するとC言語のdefault句同様
;   どれもに一致しなかった場合の処理を記述できる。

(setq x 0)
(format t "~A~A" (case x (1 (quote TsukuyomiMode)) (2 (quote NekomimiMode))
							(t (quote Kiss)))
					#\LineFeed)

(format t "~A~A" (case x (1 (quote TsukuyomiMode)) (2 (quote NekomimiMode))
							(otherwise (quote Kiss)))
					#\LineFeed)

; keyformが指定した記号と等しいかどうかを調べる場合は、記号を直接指定するのではなく
; リストに入れて指定しなければならないことに注意！！
; ex.) keyformがtやnilの場合、(case keyform (t form) ...)とすると
;      tはcaseの最後に指定しなければならないことから、caseマクロが
;      エラーしてしまう！！
;      また、(case keyform (nil form) ...) とした場合、keyformがnilであっても
;      formが評価されない。nilはそのまま評価され (case keyform (() form) ...)
;      と解釈されてしまうのでcaseマクロがエラーを起こす！！

(setq x t)
(format t "~A~A" (case x ((t) 'TsukuyomiMode) ((nil) 'nekomimiMode)) #\LineFeed)

; クウォートして明示的に記号型の値そのままを与えてもおk
(format t "~A~A" (case x ((quote t) (quote TsukuyomiMode))
						 ((quote nil) (quote NekomimiMode)))
					#\LineFeed)

(setq x nil)
(format t "~A~A" (case x ((t) 'TsukuyomiMode) ((nil) 'NekomimiMode)) #\LineFeed)

(format t "~A~A" (case x ((quote t) (quote TsukuyomiMode))
						 ((quote nil) (quote NekomimiMode)))
					#\LineFeed)


