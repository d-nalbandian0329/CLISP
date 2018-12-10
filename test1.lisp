; Comment

; 関数の宣言
; defunマクロ
; defun function-name lambda-list form*

; function-name  新しい関数名
; lambda-list    呼び出されるときにリストから関数に渡される仮パラメータ
;                ラムダリストと呼ばれる。パラメータの変数は評価されずに、
;                関数の呼び出し元から指定されたデータを格納するためのもの。

; ＊ 関数が特に値を受け取らない場合は、空のリスト () を指定する

; form*  新しく定義する関数の本体になる部分。任意の式を記述可能。
;        関数定義後に関数が呼び出されるとformが実行される。

; defun マクロの実行結果は常に関数名function-nameに指定した記号になる。
;       既に定義されている関数名が指定された場合、既存の関数から新しい
;       定義に上書きされる。
; ＊特別式に指定されている名前setqなどを定義することはできません！



(defun hello ()
	(print "Hello,world!")
	(exit))

; ext:saveinitmem イメージをファイルに保存する関数
; =>キーワード引数executableにnil以外の値を渡す
(ext:saveinitmem "hello-clisp"
					:quiet t				; バナーの非表示
					:norc t					; 初期化ファイルをロードしない
					:init-function 'hello	; REPELの前にhelloを呼ぶ
					:executable t)
(hello)

; ランタイム : Lispのプログラムを動かすために必要なもの
;			   S式を読んだり、評価したりといった中核の部分(カーネル)
; 			   とその中核部分を利用して動かす基本的なライブラリが
;			   含まれる部分を合わせたもの
;@ANSI Common Lispで決められている処理に必要な振る舞いのうち、
; どこまでを中核に含むか、どこまでをどの言語で書くかは処理系に
; よって異なる。実際のLispのプログラムはランタイムを利用して動く。

; イメージ : コンパイルされた関数の定義や、変数の定義、型の定義、
;			 クラスの定義、処理系内部の状態など、その時点での
;			 処理系の状態を再現するために必要な様々な状態のこと
;			 コアイメージやヒープイメージと呼ばれる。

; Lisp処理系はランタイムとイメージを組み合わせて実行ファイルを作成する

; @トップレベルの処理(HyperSpecで定義されているトップフォームは無関係)
; =>ランタイムによるスタートアップコードが起動する最初に行われる処理のこと
;   エントリーポイントとも呼ばれる

; REPL(A Read-Eval-Print Loop) 式の読み込み->式の評価->結果の表示の繰り返し

;@ランタイムのトップレベルの処理は多くの場合REPLである。だが、ほとんどの
; アプリケーションでは起動直後にREPLに入られても困る。
;!! REPLが不要な場合、適切なトップレベル処理を指定したり、REPLに入る前に
;   プログラムを終了させたりする必要がある。

; Tree-shaking
; 商用処理系などにあるランタイムとイメージを組み合わせる際に、配布に
; 不要な部分(コンパイラやデバッガなど)を省いて出来上がるファイルサイズ
; を抑える機能

; Common Lispは標準ライブラリが大きいので出来上がる実行ファイルも
; 大きくなりがち。ので、Tree-shakingがある処理系では多少抑えることが可能。

;@Purifying
;=>ヒープ領域にあるオブジェクトを静的領域に移動することで、GCの効率を
;  上げることができる機能。
;  イメージを保存するときにこの処理をすることで、効率良く動作するイメージ
;  を作成できる場合がある。

