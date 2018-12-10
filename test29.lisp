; lambda list : defunやlambda式の仮引数を定義するリスト

; lambda listではlambda list keyword を使える

; &optional  引数のデフォルト値を設定
; &rest      引数をリストにまとめて関数へ渡す
; &key       キーワードの設定
; &aux       補助変数の指定


; &optionalで指定されたパラメータをオプションパラメータという
; 指定範囲は次のラムダリストキーワードまで、又はラムダリストの
; 終わりまで

(setf ope '((lambda (a &optional (b 10)) (+ a b)) 1 2))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '((lambda (a &optional (b 10)) (+ a b)) 1))
(format t "~A => " ope)
(eval ope)
; &optional (var value)  の形で指定する
; &optional var          のように値を省略すると初期値はnil

(defun my-reverse(tree)
	(if (atom tree) nil (append (my-reverse (cdr tree)) (list (car tree)))))

(setf hoge '(1 2 3 4 5))
(format t "~A => ~A~%" hoge (my-reverse hoge))

; リストの反転
; appendを使用しているのでリストが長くなると
; 処理時間が長くなる
(defun my-reverse(l)
	(if (atom l)
		l
		(append (my-reverse (cdr l)) (list (car l)))))

(setf hoge '(1 2 3 4 5 6 7 8 9))
(format t "~A => ~A~%" hoge (my-reverse hoge))


(defun my-reverse(tree) (my-reverse-sub tree nil))

(defun my-reverse-sub(old new)
	(if (atom old)
		new
		(my-reverse-sub (cdr old) (cons (car old) new))))


(setf hoge '(1 2 3 4 5 6 7 8 9))
(format t "~A => ~A~%" hoge (my-reverse hoge))


; lambda list keywordを使うと一つにまとめられる
; &optionalで設定したオプションパラメータはデフォルト値nil

(defun my-reverse(tree &optional ret)
	(if (atom tree)
		ret
		(my-reverse (cdr tree) (cons (car tree) ret))))

(setf hoge '(1 2 3 4 5 6 7 8 9))
(format t "~A => ~A~%" hoge (my-reverse hoge))

; &rest  引数を一つのリストにまとめて関数へ渡す
;        可変長引数の定義が可能

; ＊ &restの後ろは引数かlambda list keyword
;    又はlambda listの終わりでなければならない

; 二個以上の引数を受け取る関数
; (defun foo (a b &rest para) ...)

; 可変長引数を受け取る関数
; (defun foo(&rest args) ...)


(setf ope '(defun foo(a b &rest z) (format t "a = ~S, b = ~S, c = ~S~%" a b z)))
(format t "~A~%=> ~A~%" ope (eval ope))

(setf ope '(foo 1 2 3 4 5))
(format t "~A~%=> ~A~%" ope (eval ope))

(setf ope '(foo 1 2))
(format t "~A~%=> ~A~%" ope (eval ope))

(setf ope '(defun foo(&rest args) (format t "args : ~S~%" args)))
(format t "~A~%=> ~A~%" ope (eval ope))

(setf ope '(foo 1 2 3 4 5))
(format t "~A~%=> ~A~%" ope (eval ope))

(setf ope '(foo))
(format t "~A~%=> ~A~%" ope (eval ope))




; キーパラメータ => &keyで指定されたパラメータ。キーワードの設定が可能
;                   有効範囲は次のラムダリストキーワード
;                   または、ラムダリストの終わりまで。

(setf ope '((lambda (a &key b c) (list a b c)) 10))
(format t "~A~%=> ~A~%" ope (eval ope))

; デフォルト値の指定
(setf ope '((lambda (a &key (b 1) (c 2) (d 3)) (list a b c d))
					10 :c 20 :b 30 :d 40))
(format t "~A~%=> ~A~%" ope (eval ope))

; オプションパラメータとの比較
(setf ope '((lambda (a &key (b 1) (c 2) (d 3)) (list a b c d))
				10 :d 20))
(format t "~A~%=> ~A~%" ope (eval ope))

(setf ope '((lambda (a &optional (b 1) (c 2) (d 3)) (list a b c d))
				10 1 2 20))
(format t "~A~%=> ~A~%" ope (eval ope))

; let* : let同様にレキシカル変数を定義する
;        ただし、変数の初期化は逐次的に行われる
; ＠setf, setq と同じく先に初期化された変数の値を使用可       

(setf ope '(let* ((a 10) (b (* a 10)))
				(format t "a = ~D, b = ~D~%" a b)))
(format t "~A~%=> " ope)
(eval ope)

; let では初期化した変数の値を利用できない

; &aux   補助(AUX)パラメータ。補助パラメータはどの実引数
;        ともマッチせず let* でレキシカル変数を定義することと
;        同じ働きをする

; &aux var   といった感じで&auxオプションに変数だけ指定
; と初期値はnilになる

; &auxの使用例
(setf ope '((lambda (a b &aux (c (car a))) (list a b c)) '(10 20) 30))
(format t "~A~%=> ~A~%" ope (eval ope))

; 上の式は下の式と等価
(setf ope '((lambda (a b) (let* ((c (car a))) (list a b c))) '(10 20) 30))
(format t "~A~%=> ~A~%" ope (eval ope))

; lambda list keywordを複数使用する場合、引数の後ろに
; &optional、その後ろに&rest、最後に&keyと&auxを定義するようにする


; nthcdr : 指定回数分cdrを実行する関数
; (nthcdr n list)

(setf ope '(nthcdr 0 '(a b c d)))
(format t "~A~%=> ~A~A" ope (eval ope) #\NewLine)

(setf ope '(nthcdr 2 '(a b c d)))
(format t "~A~%=> ~A~A" ope (eval ope) #\NewLine)

(setf ope '(nthcdr 5 '(a b c d)))
(format t "~A~%=> ~A~A" ope (eval ope) #\NewLine)




