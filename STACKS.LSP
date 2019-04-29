;
; Coded by Antti Karttunen once upon a time. FORTH-code also coded
; by the same person. Thrice-recursive reverse-function in lisp is
; quite old algorithm, I think.
; Algorithms and code presented here in various languages are,
; of course, PUBLIC DOMAIN.
;
;
; This is little demo program to demonstrate some algorithms for
; reversing order of the elements of the stack with the help of
; one additional stack. (Somewhat related to the Hanoi towers???)
; These were originally written for FORTH so names of the stack
; manipulating primitives correspond to those of FORTH.
; (SWAP, DROP, DUP, R>, >R, etc).
;
; (fill '(a b c d e)) Clears the screen and "fills" the "parameter"
; stack, so that element a goes to the top, and element e to the bottom.
;
; (LIFTDOWN)   rotates the topmost element of the stack to the bottom,
;              rest of stack one upward.
; (LIFTUP)     rotates the bottommost element of the stack to the top,
;              rest of stack one downward.
; (REV1)       reverses the parameter stack using LIFTDOWN.
;
; (REV2)       reverses the parameter stack using LIFTUP.
;
; (REV3)       reverses the parameter stack using hairy algorithm
; developed from the notorious "thrice-recursive"
; formula first used in Lisp reverse -function. Takes much longer than REV1
; & REV2.
;
;
; Note! This program writes directly to CGA-videomemory in textmode
; (from B000:8000 onward), so you must have a compatible screen,
; or use something like SIMCGA with Hercules.
; Or code new plotchar function! (Using ANSI-codes for example, and
; making this much slower).
; Also cls -function uses ANSI-sequence to clear the screen, so
; check that your machine has ansi-emulation on.
; Note! Arguments for fill -function should be atoms or numbers with
; just one letter or digit in their names. For example:
; after you have loaded this file with (load 'stacks) to St. Vitus Lisp,
; type (fill '(1 2 3 4 5 6 7 8 9)) to fill the stack with nine elements.
; Turn them upside down with (REV1), (REV2) or (REV3).
; The last one takes much longer, but uses more interesting algorithm.
;
 
/* Here are LIFTDOWN, LIFTUP, REV1, REV2 and REV3 written in FORTH:
( Must check that these actually WORK in some Forth! )
  
: LIFTDOWN DEPTH 0= IF ELSE DEPTH 1 = IF ELSE SWAP >R RECURSE R> THEN THEN ; 
 
: LIFTUP   DEPTH 0= IF ELSE DEPTH 1 = IF ELSE >R RECURSE R> SWAP THEN THEN ; 
 
: REV1 DEPTH 0= IF ELSE DEPTH 1 = IF ELSE >R RECURSE R> LIFTDOWN THEN THEN ; 
 
: REV2 DEPTH 0= IF ELSE DEPTH 1 = IF ELSE LIFTUP >R RECURSE R> THEN THEN ; 
 
: REV3
   DEPTH 0= IF
    ELSE DEPTH 1 = IF
     ELSE DEPTH 2 = IF SWAP
      ELSE >R RECURSE R> SWAP >R >R RECURSE R> RECURSE R>
      THEN
     THEN
    THEN
; 
 
 
*/
 
; Here is that notorious version of reverse-function which inspired me
; to write REV3 in FORTH:
 
(defun rewerse (lista)
       (cond ((null (cdr lista)) lista)
              (t (cons (car (rewerse (cdr lista)))
                        (rewerse (cons
                                   (car lista)
                                   (rewerse (cdr (rewerse (cdr lista))))))))))

; This function returns the count how many times rewerse is called when
; rewersing lista of length n. Sequence runs like this:
; (1 1 5 17 57 189 625 2065 6821 22529 74409 ...)
(defun rewerse-count (n)
       (cond ((lessp n 2) 1)
              (t (add1 
                   (+ (* 3 (rewerse-count (sub1 n)))
                           (rewerse-count (- n 2))
                   )
                 )
              )
       )
)

; This returns the same result for REV3 (for n stack elements).
; (1 1 1 4 10 25 61 148 358 865 2089 5044 12178 29401 70981 ...)
(defun rev3-count (n)
       (cond ((lessp n 3) 1)
              (t (add1 
                   (+ (* 2 (rev3-count (sub1 n)))
                           (rev3-count (- n 2))
                   )
                 )
              )
       )
)


(defun cls () (princ `\e`) (princ "[2J") ())


(setq PAR-STACK (new-clist 50))
(setq RET-STACK (new-clist 50))

(setq P -1) ; Parameter Stack Pointer
(setq R -1) ; Return Stack Pointer

(setq base 23)

(defun fill (lista)
       (setq P -1)
       (setq R -1)
       (cls)
       (mapc #'PUSH (reverse lista))
       (length lista)
)


(defun PUSH (elem)
       (setq P (add1 P))
       (rplacx P PAR-STACK elem)
       (plot-stack-elem elem P 'P))

(defun POP (&aux topmost)
       (setq topmost (TOP))
       (DROP)
       topmost)


(defun TOP ()
       (cxr P PAR-STACK))


(defun DROP ()
       (plot-stack-elem () P 'P)
       (setq P (sub1 P)))

 
; Asterisked ones use the "return stack" instead of the "parameter stack":

(defun *PUSH (elem)
       (setq R (add1 R))
       (rplacx R RET-STACK elem)
       (plot-stack-elem elem R 'R))

(defun *POP (&aux topmost)
       (setq topmost (*TOP))
       (*DROP)
       topmost)

(defun *TOP ()
       (cxr R RET-STACK))

(defun *DROP ()
       (plot-stack-elem () R 'R)
       (setq R (sub1 R)))

(defun DUP () (PUSH (TOP)))       

(defun SWAP (&aux veba hiba)
       (setq veba (POP))
       (setq hiba (POP))
       (PUSH veba)
       (PUSH hiba))

(defun >R ()
       (*PUSH (POP)))

(defun R> ()
       (PUSH (*POP)))


(defun DEPTH () (add1 P)) ; How many elements in the parameter stack?


; This is the device dependent code! Program your own if you don't
; have the notorious PC with CGA compatible screen:
(defun plotchar (char line column)
       (@= *screen* char (+ (* line 160) (* column 2)))
       ())

(defun plot-stack-elem (elem n stack-id)
       (plotchar (cond ((intp elem) (+ `0` elem))
                        ((null elem) ` `)
                         ((@ elem)))
                 (- base n)
                 (cond ((eq stack-id 'P) 32)
                       ((eq stack-id 'R) 36)
                       (t 40))))

; Rotate the topmost stack element to the bottom.
; ("Lift" is going down.)
(defun LIFTDOWN ()
   (cond ((lessp (DEPTH) 2)) ; Do nothing if just one element (or none).
         (t
            (SWAP)     ; Swap the topmost to the second topmost.
            (>R)       ; Toss the second topmost to the return stack.
            (LIFTDOWN) ; Recurse.
            (R>)       ; When returning, toss elems back from return stack.
         )
   )
)

; Opposite to previous one. Rotate the bottommost stack elem to the top.
; ("Lift" is coming up.)
(defun LIFTUP ()
   (cond ((lessp (DEPTH) 2)) ; Do nothing if just one element (or none).
         (t
            (>R)      ; Toss elements to the return stack.
            (LIFTUP)  ; recurse.
            (R>)      ; Restore from the ret stack
            (SWAP)    ; And swap with the elem we got from the bottom.
         )
   )
)

; Reverse the stack elements, using LIFTDOWN as auxiliary procedure.
(defun REV1 ()
   (cond ((lessp (DEPTH) 2)) ; Do nothing if just one element (or none).
         (t
            (>R)        ; Topmost to the return stack
            (REV1)      ; Reverse the remaining.
            (R>)        ; Return the original topmost,
            (LIFTDOWN)  ; And rotate it to bottom.
         )
   )
)


(defun REV2 ()
   (cond ((lessp (DEPTH) 2)) ; Do nothing if just one element (or none).
         (t
            (LIFTUP)  ; Get the bottommost to the top of the stack.
            (>R)      ; Toss it to return stack
            (REV2)    ; Reverse the remaining.
            (R>)      ; And return the original bottommost from the ret stack.
         )
   )
)



;(setq *COUNT* 0) ; This was for testing the correctness of rev3-count

(defun REV3 ()
;  (setq *COUNT* (add1 *COUNT*))
   (cond ((zerop (DEPTH))) ; Do nothing if no elements,
         ((eq (DEPTH) 1))  ; or just one.
         ((eq (DEPTH) 2) (SWAP)) ; If two, then just swap them.
         (t ; Else
            (>R)    ; Toss top elem to return stack.
            (REV3)  ; Reverse the remaining.
            (R>)    ; Restore the original top elem from ret stack.
            (SWAP)  ; Swap them.
            (>R)    ; And put both to ret stack. Orig last one is now
            (>R)    ; on bottom of it, and orig top one as second.
            (REV3)  ; reverse to get orig seq. without first and last.
            (R>)    ; take the orig top one back.
            (REV3)  ; reverse to get orig sequence reversed without original
            (R>)    ; last elem, which is got here from return stack.
         )
   )
)


; In Japanese NEC PC98 compatibles the text screen is from the
; address A000:0000 onward. (On some another models it is in E000:0000 -)
; Make new *screen* variable (plotchar uses it) with fake function.
; C00 is the flag bit indicating that its type is a string.
(defun set-epson-mode () (not (setq *screen* (fake 0xAC00 0x0000))))

