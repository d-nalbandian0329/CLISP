; 動的なデータの並び
; Lispで扱うことができる何らかのデータをまとめてListとして管理できる。
; Listは配列と似たような存在であるが、Lispには配列も存在するため、
; 配列とは異なるものである。明示的にListを宣言するには組み込み関数listを使用
;
; (list &rest args)
;
; args : リストのデータ並びに指定する人にのデータ。
;        リストに配置される各々のデータを要素と呼ぶ。

(list 10 20 30)

(list (list 1 2 3) (list 10 20 30) 100 200 300)

; 上記のようにlist関数でlistを定義するとシステムは丸括弧で
; 要素を括ってリストを表示する。リストも一つのデータなので
; リストを含むリストも定義可能。

; またquote特別式は評価せずに値を返すという性質を持つので
; リストを評価せずにリストとして返すことになる。
; つまり、要素を評価せずにリストを作成する場合はquote特別式
; を使えば良い。


; リスト操作関数
; car関数  (car list)  リストの戦闘要素の取得
; cdr関数  (cdr list)  リストの戦闘要素以外の取得

; --- psuedo code ---
; loop <list>:
;   print (car list)
;   list <- (cdr list)

(setq lst (quote (10 20 30)))

(princ (car lst))
(setq lst (cdr lst))

(print (car lst))
(setq lst (cdr lst))

(print (car lst))
(setq lst (cdr lst))

; cons関数 : リストの先頭に新規要素を追加する
; (cons addValue list)

(setq lst (quote (30)))
; (setq lst '(30))

(setq lst (cons 20 lst))

(setq lst (cons 10 lst))

(print (car lst))
(setq lst (cdr lst))

(print (car lst))
(setq lst (cdr lst))

(print (car lst))
(setq lst (cdr lst))

; 空のリスト : cdr関数で要素がすべて失われたリストや最初から
;              要素が存在しないリスト。表記上は(),システム的には
;              定数nilと同義。
; ＠つまり、リストの評価結果がnilであればリストは要素を持たない
;   解釈することができる。

(print (list))

(print (quote ()))

(princ #\LineFeed)

; length関数 : リストの要素数を返す
; (length sequence)
; ＊リストが空であった場合やnilを指定した場合は0を返す

(format t "(length '(10 20)) : ~A~A" (length (quote (10 20))) #\LineFeed)

(format t "(length (cdr (quote 10))) : ~A~A" (length (cdr (quote (10)))) #\LineFeed)

(format t "(length nil) : ~A~A" (length nil) #\LineFeed)

