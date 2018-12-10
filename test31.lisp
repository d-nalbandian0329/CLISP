; read 関数 : データを読み込んでS式に変換して返す
;             read関数はデータを読み込むだけで、S式の評価は行わない
;             read関数で読み込んだS式を評価するにはeval関数を使用する

;(format t "(read)~%=>")
;(format t "~A~%" (read))

;(format t "(read)~%=>")
;(format t "~A~%" (read))

;(format t "(eval (read))~%=>")
;(format t "~A~%" (eval (read)))


; print関数
; print関数によって出力されるデータはエスケープコード
; を使って印字される。例えば文字列は"括られる。
; ==>readで読み込まれる形で印字されるということ
; print関数は改行してからデータを出力し最後に空白文字
; を一つつける。printの返り値は出力したデータです。

; prin1関数 : printと同じ形式で同じ形式で出力
;             改行と空白文字を出力しない

; princ関数 : エスケープコードを使わないで出力する
;             つまり文字列は"で括られずそのまま出力される

;(setf ope '(progn (print "Hello,world!!") (format t "]")))
;(format t "~A~%" ope)
;(format t "~A~%" (eval ope))

;(setf ope '(progn (prin1 "Hello,world!!") (format t "]")))
;(format t "~A~%" ope)
;(format t "~A~%" (eval ope))

;(setf ope '(progn (princ "Hello,world!!") (format t "]")))
;(format t "~A~%" ope)
;(format t "~A~%" (eval ope))

; ストリーム
; => ファイルとプログラム間でデータをやりとりする際の
;    データの流れ
; Common Lispではストリーム型データを介してファイルにアクセスする
; ファイルとデータをやりとりする際にはストリームを経由して行う

; ＊ファイルにアクセスする場合の3つの基本操作
; 1. アクセスするファイルをオープンする  open関数
; 2. 入出力関数を使ってファイルを読み書きする
; 3. ファイルをクローズする              close関数

; ファイルをオープンする
; => アクセスするファイルを指定してそれと1対1に
;    対応するストリームを生成すること

; ＠開いたファイルは必ず閉じること！！

; Common Lispにはファイルオープンとクローズを行う便利なマクロ
; がある => with-open-file

; with-open-fileマクロ
; => 与えられたファイルをキーワード:directionで指定した方向で
;    オープンし、生成したストリームを変数にセットする。
;    マクロ終了時にファイルは自動的にクローズされる。
;
; (with-open-file (variable
;             FileName
;             :direction [:input or :output])
; ...)

(with-open-file (out "./buff.txt" :direction :output)
	(format out "Data :")
    (dotimes (i 10) (format out " ~A" i))
    (format out "~A" #\LineFeed))

(with-open-file (in "./buff.txt" :direction :input)
	(let ((num))
		(loop
			(setf data (read in nil))
			(if (eq data nil) (return))
			(format t " ~A" data))
		(format t "~A" #\LineFeed)))


