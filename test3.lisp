; 算術で用いる「数」を数値型(Numbers)と呼ぶ。
; 数値型には整数型や実数型などがある。
; Common Lispでは整数型の許容範囲を定めていないので
; システムが許容する範囲であれば、どのような数でも扱える。

; 基数指定を以下の形式で行うことができる
; #基数r整数  #基数R整数

; ＊ただし、よく使う2進数、8進数、16進数は別の表記も可能

; #b整数  #B整数    2進数
; #o整数  #O整数    8進数
; #x整数  #X整数   16進数

(princ (+ #b1011 #B1011 #2r1011 #2R1011))

(print (+ #o100 #O100 #8r100 #8R100))

(print (+ #x4A #X4A #16r4A #16R4A))

(print #B1000)
(print #8r777)
(print #2r1010)
(print #4R321)
(print #X-FF)
(print #3r22)

; 下の形式だとインタプリタならいけたんだけどコンパイルすると認識されん、、
; (format nil "~A ~A ~A" #o755 #Xa 12)

(print (+ #XA #b0101))

; 分数型(Ratio)
; 分子 / 分母  どちらも整数型。基数を指定する場合はどちらか一方ということは
;              できない。#b1/10 といった感じで全体指定しなければならない。

(print (/ 2 4))

(print (+ (/ 1 2) (/ 1 4)))

(print (/ 1 -2))

(print #b11/1111)

; 浮動小数点数(Floating Point Number)
; 数値リテラルに小数点を指定するか、指数を用いることで
; その数値が浮動小数点数リテラルであると認識できる。

(print (+ 1.2 -0.7))

(print (+ 1. .3))

(print (+ 1. -.3))

(print (/ 0.0123E2 314E-2))

; 複素数(Complex Numbers)
; #C(実数部 虚数部)
; 数学の複素数と同じ。実数部と虚数部を持つ。
; 数値は整数以外の数を指定可能(分数なども)。

(princ #\LineFeed)
(format t "(4 + 3i) * (4 - 3i) : ")
(princ (* #C(4 3) #C(4 -3)))

(princ #\LineFeed)
(format t "(1.34 + 3i) * (1/2 - 3i) : ")
(princ (* #C(1.34 3) #C(1/2 -3)))
