; Tower of Hanoi

(defun hanoi(disk src dest mid)
	(if (> disk 1) (hanoi (- disk 1) src mid dest))

	(format t "Move Disk No.~A from ~A to ~A.~A" disk src dest #\LineFeed)

	(if (> disk 1) (hanoi (- disk 1) mid dest src))
)

(hanoi 3 "A" "C" "B")

