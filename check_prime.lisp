; 素数判定のなんたらかんたら

; 素数判定 : 受け取ったリスト内要素に、第一引数の約数が含まれるかどうか判定
;			 デフォルトの戻り値をtにして、対象が素数でなければnilを返す。

; condマクロを使えば特定条件式を実行した後に、それ以外は実行されなくなる

; 下のcondの記述について

;	(cond
;		((zerop (mod p e)) (return))
;		((> e (isqrt p))   (return t))
;	)

; シンボルpが素数でなければ上の条件文後の処理実行
; 一方、それ以外のケースの内でリスト内要素の内、
; 比較対象の二重根より大きな値は比較する必要なし。

(defun primep(p list_)
	(dolist (e list_ t)
		(cond
			((zerop (mod p e)) (return))
			((> e (isqrt p))   (return t))
		)
	)
)

; 素数のリスト作成
(defun prime(num)
	(do
		((i 3 (+ i 2)) (plist (quote (2))))
		((> i num) plist)
		(if (primep i plist) (setf plist (append plist (list i))))
	)
)

(format t "~A~A" (prime 100) #\LineFeed)

