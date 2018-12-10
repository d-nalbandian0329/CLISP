; 列(sequence) : list, string, vectorを統一的に扱える型

; 列関数 : 列型データを操作するための関数

; (elt sequence index)   indexで指定した要素をsequenceから取得
;						 sequenceのindex番目の要素を返す

; (subseq suquence start &optional end)
; 部分列を取り出す。endを省略した場合はstartから列最後尾まで取り出す。

; (copy-seq sequence)
; 列のコピーを返す。(subseq sequence 0) と等価

; (length sequence)
; 列の長さを返す。

; (reverse sequence)
; 要素を逆順にして新しい列を返す。

; (make-sequence type size)
; 型がtypeで長さがsizeの列型データを生成

; ＠列はベクタと同様、位置指定は0基底です
; ＠subseqで指定するstartは対象となるが、endは対象外
;   つまり、[start,end)

(setf ope '(setf ary '(10 11 12 13 14 15)))
(format t "~A : ~A~%" ope (eval ope))

(setf ope '(elt ary 0))
(format t "~A : ~A~%" ope (eval ope))

(setf ope '(subseq ary 2 5))
(format t "~A : ~A~%" ope (eval ope))

(setf ope '(copy-seq ary))
(format t "~A : ~A~%" ope (eval ope))

(setf ope '(subseq ary 0))
(format t "~A : ~A~%" ope (eval ope))

(setf ope '(length ary))
(format t "~A : ~A~%" ope (eval ope))

(setf ope '(reverse ary))
(format t "~A : ~A~%" ope (eval ope))

(setf ope '(make-sequence 'list 10 :initial-element 0))
(format t "~A : ~A~%" ope (eval ope))




; 文字データ
; #\文字

(setf ope '(setf a "abcdeあいうえお"))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)

(setf ope '(elt a 0))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)

(setf ope '(elt a 5))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)

(setf ope '(length a))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)

(setf ope '(reverse a))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)

(setf ope '(make-sequence 'string 10 :initial-element #\a))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)

(setf ope '(make-sequence 'string 10 :initial-element #\あ))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)


; char関数 : 文字列から文字を散りだす
; 列関数eltでもできるよ

(setf ope '(char "あいうえお" 3))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)

(setf ope '(elt "あいうえお" 3))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)


; ＠列の探索を行う関数

; find関数             itemと等しい最初の要素を返す
; (find item sequence)

; find-if関数          predicateが真となる最初の要素を返す
; (find-if predicate sequence)

; find-if-not関数      predicateが偽となる最初の要素を返す
; (find-if-not predicate sequence)

; position関数         itemと等しい最初の要素の位置を返す
; (position item sequence)

; position-if関数      predicateが真となる最初の要素の位置を返す
; (position-if predicate sequence)

; position-if-not関数
; (position-if-not predicate sequence)

; count関数            itemと等しい要素の個数を返す
; (count item sequence)

; count-if関数         predicateが真となる要素の個数を返す
; (count-if predicate sequence)

; count-if-not関数     predicateが偽となる要素の個数を返す
; (count-if-not predicate sequence)


; ＊関数名末尾に -if, -if-not をつけたものは述語(predicate)
;   を受け取る高階関数


; 単純な列関数以外では以下のキーワードが使用可能

; :start, :end      始点と終点の指定

; :test, :test-not  述語の指定

; :key              比較する際に使用するキーにアクセスする関数

; :count            個数の制限

; :from-end         列の後ろから処理を行う


(setf ary (make-array 10))

(dotimes (i 10) (setf (aref ary i) (+ i 10)))

(format t "~%Vector :")
(dotimes (i (length ary)) (format t " ~A" (aref ary i)))
(format t "~%")

(setf ope '(find-if (function oddp) ary :start 2))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(position-if #'oddp ary :start 4))
(format t "~A => ~A~%" ope (eval ope))


(setf ope '(count-if #'oddp ary :start 3 :end 7))
(format t "~A => ~A~%" ope (eval ope))


; :test, :test-not キーワードによる述語の指定
; -if, -if-notが付いている列関数では指定不可能
; であり、デフォルトではeql関数が使われる

; 列内に格納されている要素をキーとして走査する

(setf ope '(find 'b '((a . 1) (b . 2) (c . 3))))
(format t "~A => ~A~%" ope (eval ope))

; キーにアクセスする為の関数としてcarを指定
(setf ope '(find 'b '((a . 1) (b . 2) (c . 3)) :key #'car))
(format t "~A => ~A~%" ope (eval ope))



; ＠列を修正する関数

; remove 関数  itemと等しい要素を取り除く
; remove item sequence

; remove-if 関数  predicateが真となる要素を列から取り除く
; remove-if predicate sequence

; remove-if-not 関数  predicateが偽となる要素を列から取り除く
; remove-if-not predicate sequence

; substitute 関数  oldと等しい要素をnewと置き換える
; substitute new old sequence

; substitute-if 関数  predicateが真となる要素をnewと取り替える
; substitute-if new predicate sequence

; substitute-if-not 関数  predicateが偽となる要素とnewを取り替える
; substitute-if-not new predicate sequence

; fill 関数  列の要素をitemで置き換える
; fill sequence item

; remove-duplicates 関数  列内の重複要素を取り除く
; remove-duplicates sequence

; ＊remove, substituteは引数を破壊しないが
;   fillは引数を破壊する

; ＊delete, substituteも破壊的な修正を行う列関数


(setf ope '(setf ary '(10 11 12 13 14 15 16 17 18 19)))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(remove-if #'oddp ary))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(remove-if #'oddp ary :start 2 :end 8))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(substitute-if 99 #'oddp ary))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(substitute-if-not 99 #'oddp ary))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(fill ary 99 :start 3 :end 7))
(format t "~A => ~A~%" ope (eval ope))

(format t "~A : ~A~A" 'ary ary #\LineFeed)


; ＊:countキーワードによる処理対象要素の個数制限
;   によって、削除処理を調整

(setf ope '(setf ary '(a b c a b c a b c)))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(remove 'a ary))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(remove 'a ary :count 2))
(format t "~A => ~A~%" ope (eval ope))

; :from-end キーワードにt指定で後ろからの処理開始
(setf ope '(remove 'a ary :count 2 :from-end t))
(format t "~A => ~A~%" ope (eval ope))


; :test-notキーワードを用いて等しくない要素を削除する
(setf ope '(remove 'b '((a . 1) (b . 2) (c . 3) (b . 4)) :test-not #'eql :key #'car))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)

(setf ope '(remove-if-not (lambda (var) (eql var 'b))
				'((a . 1) (b . 2) (c . 3) (b . 4)) :key #'car))
(format t "~A => ~A~A" ope (eval ope) #\NewLine)


(setf ary '(a b c b d d d e))

(setf ope '(remove-duplicates ary))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(remove-duplicates ary :from-end t))
(format t "~A => ~A~%" ope (eval ope))

(setf ary '("abc" "def" "abc" "ghi"))

; remove-duplicates 関数についての補足
; 通常時は、前の要素が削除される
; :from-end キーワードがtに指定されていると後ろの要素が削除される


(setf ope '(remove-duplicates ary :test (function equal)))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(remove-duplicates ary :test #'equal :from-end t))
(format t "~A => ~A~%" ope (eval ope))


; ＠mapcar 関数はリストにしか適用できなかったが、列型用のマップ関数がある

; map 関数       列の要素にfuncを適用して、その結果を列に格納して返す
; map result-type func sequence

; result-type  返り値のデータ型
; func         要素に適用する関数
; sequence     対象となる列型データ



; map-into 関数  列の要素にfuncを適用して結果をresult-seqに代入する
; map-into result-seq func sequence

; result-seq  結果の格納先となる列, 内容は戻り値で書き換えられることに注意!!
; func        要素に適用する関数
; sequence    処理対象

; ＊result-seq には文字列を指定できません！listかvectorのみ指定可能！

(setf ope '(map 'list #'+ '(1 2 3) '(10 20 30 40)))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(map 'vector #'* '(10 20 30) '(1 2 3)))
(format t "~A => ~A~%" ope (eval ope))

(setf vec (make-array 4))
(dotimes (i (length vec)) (setf (aref vec i) (* (1+ i) 10)))

(format t "~A => ~A~%" 'vec vec)

(setf ope '(map-into vec #'+ '(1 2 3) '(4 5 6)))
(format t "~A => ~A~%" ope (eval ope))

(format t "~A => ~A~%" 'vec vec)



; concatenate 関数  引数を連結した結果をresult-typeで指定した列で返す
; concatenate result-type sequence ...

; ＊リストやベクタを文字列に変換する場合は、列の要素が文字でなければならない

(setf ope '(concatenate 'list '(a b c) '(d e f g)))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(concatenate 'vector '(a b c) '(d e f g)))
(format t "~A => ~A~%" ope (eval ope))

(setf ope '(concatenate 'string '(#\a #\b #\c) '(#\d #\e #\f #\g)))
(format t "~A => ~A~%" ope (eval ope))










