; #\文字(特殊文字) : 視覚的に表現できない文字

; #\Space    : 空白
; #\Tab      : タブ
; #\Rubout   : 消去
; #\Return   : 行頭復帰
; #\LineFeed : 改行
; #\Page     : 改ページ

(princ "kotoko")
(princ #\Space)
(princ "yukarin")

(princ #\LineFeed)

(princ "hogerin")
(princ #\Tab)
(princ "fuga")

(princ #\Return)
(princ "Nana")
(princ #\Rubout)

; (princ #\Page)
(princ #\LineFeed)

; 文字データは内部では数値表現になっているので、その文字に対応した
; 値を取得することも出来る。
; 文字・コード変換関数
; char-code   : 文字から文字コードへ
; code-char   : 文字コードから文字へ

; #\ を文字の前につけることで、その文字を単なる文字として扱うことを明示
(format t "code of \"A\" : ")
(princ (char-code #\A))
(princ #\LineFeed)

(format t "character of 65 : ")
(princ (code-char 65))
(princ #\LineFeed)

(format t "character of 0x42 : ")
(princ (code-char #x42))
(princ #\LineFeed)

(format t "code of \"Space\" : ")
(princ (char-code #\Space))
(princ #\LineFeed)

(format t "code of \"Tab\" : ")
(princ (char-code #\Tab))
(princ #\LineFeed)

