;
; From page 183 of Lisp book.
;

(defun threat (i j a b)
  (or (eq i a)              ; Same row.
      (eq j b)              ; Same column.
      (eq (- i j) (- a b))  ; SW-NE diagonal.
      (eq (+ i j) (+ a b))  ; NW-SE diagonal.
  )
)

(defun conflict (n m board)
  (cond ((null board) nil)
        ((or (threat n m (caar board) (car (cdar board)))
	     (conflict n m (cdr board))
        ))
  )
)



;
; From page 367 & 368 of Lisp book.
;



(defun queen (size)
   (setq *count* 0)
   (queen-aux nil 0 size)
   (princ *count*) (princ " solutions.")
   (terpri)
)

(setq queens queen) ; Alias.


(defun queen-aux (board n size)                  ; Start on next row.
   (cond ((eq n size)                            ; Found a solution.
            (board-print (print (reverse board)))
	    (terpri) (setq *count* (add1 *count*)))
          (t (queen-sub board n 0 size))))       ; Try in this row.

(defun queen-sub (board n m size)                ; Try next column.
   (cond ((eq m size))                           ; Hit end of row?
          (t (cond ((conflict n m board))        ; Conflict?
	           (t (queen-aux (cons (list n m) board) (add1 n) size))) ;No
	     (queen-sub board n (add1 m) size)   ; Now move right one.
	  )
   )
)

(defun board-print (board) (board-print-aux board (length board)))

(defun board-print-aux (board size)
  (terpri)
  (cond ((null board))                                 ; Stop if nothing left
        (t (board-print-sub (car (cdar board)) 0 size) ; Do this row,
	   (board-print-aux (cdr board) size)          ;  then the next row.
	)
  )
)


(defun board-print-sub (column n size)
   (cond ((eq n size))                        ; Enough columns.
         (t (cond ((eq column n) (princ 'Q))  ; Place Q here.
	          (t (princ "."))             ; Dot elsewhere.
	    )
	    (princ " ")                       ; and space between.
	    (board-print-sub column (+ n 1) size)
         )
   )
)

(progn
(princ
 "Use (queens n) to print all solutions to queens problem with nxn board")
(terpri)
)

