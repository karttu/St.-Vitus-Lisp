;
; ALARUHO.LSP -- Hairy "AI"-program by Antti Karttunen, coded once upon
;  a time (198?) and again at the year 1991.
;
/*
   Copyright (C) 1991  Antti J. Karttunen

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License (in file GPL.TXT) for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
;
;
; This software tries to find solutions for given "stack-reordering-problem",
;  using the basic stack manipulators of Forth-language (here called
;  primitives). They are SWAP, DUP, OVER, ROT, DROP, 2SWAP, 2DUP, 2OVER,
;  2DROP and 2ROT. The state of stack is described by usual list notation,
;  like (a b c -- b c a) where the elements at left, before -- describes the
;  original state of stack, and the elements at right, after --
;  is state of stack after the operations, here called the goal sequence.
;  Leftmost elements are at bottom of stack, and rightmost at the top,
;  so above mentioned list describes the ROT-operation, i.e. rotate the
;  third topmost element from stack to the top.
; Solutions are searched by breadth-first method, applying "randomly" all
;  primitives to current "stack", and pruning off those sequences
;  which certainly don't lead to the solution, or which are above the
;  limits length_grow_limit or primitive_limit.
;
; Originally the primitives were applied to "stack" by kind of interpreter,
;  but now they are first compiled to corresponding lisp-functions,
;  which when applied to the list simulating stack, produce the
;  corresponding rearrangement. Functions theseus, terrace, threadtoexpr
;  and compile-primitives among others are for that purpose.
;
; Functions solve and solve-perm are the functions called when needed
;  to solve the reordering-problem. Former searches the primitive
;  sequence to reorder the stack described in the list argument,
;  like '(a b c d -- b a d c) and latter does the same for all
;  permutations of given list, i.e. (a b c) produces six permutations.
;
; If global variable *M* is set non-nil, then multiple solutions are
;  searched. (And all the memory is probably devoured...)
;
; Function (permute lista) produces the all permutations of the list
;  given as argument.
;
; Hint! Set environment variable MAXMEM as big as possible, so that
; memory won't exhaust immediately. Something like SET MAXMEM=330000
; or more is good.
;  
 

(set-max-args 15)


(defun error  (x y)
       (princ x)
       (cond ((not (endmarkp y)) (spaces 1) (princ y)))
       (reset))

; Kind of comment-macro producing NIL-body, like (* comment) in Interlisp.
; Commented out because there's no macros implemented in St. Vitus's Lisp.
; (defmacro *** (&rest rest)
;	  ''())

; (defmacro getd (fun-name)
;	  `(symbol-value ,fun-name))

(setq getd symeval)

; (setq getprop get)

(setq :SPECIAL ':SPECIAL)

(setq :f ':f)
(setq :s ':s)

(defun to-the-first (alkio lista)
       (cond
             ((null lista) ())
              ((equal alkio (car lista)) ())
               (t
                  (cons (car lista) (to-the-first alkio (cdr lista))))))


/* The old remove-if
 (defun remove-if (function lista)
        (cond
              ((null lista) ())
               ((apply function (list (car lista)))
                  (remove-if function (cdr lista)))
                (t
                  (cons (car lista) (remove-if function (cdr lista))))))
 */

/*
(defun delete-if  (function lista prev-lista result)
  (setq prev-lista ())
  (setq result lista)
  (while lista
      (cond ((funcall function (car lista))
             (if (null prev-lista)
                 (progn (setq result (cdr result)) (setq lista (cdr lista)))
; Else:
                 (rplacd prev-lista (cdr lista))
                 (rplacd lista ())
                 (free-list lista)
                 (setq lista (cdr prev-lista))
             )
            )
           (t
             (setq prev-lista lista)
             (setq lista (cdr lista))
           )
      )
  )
  result
)
 */

;(setq *primitives* '(SWAP DUP OVER ROT DROP 2SWAP 2DUP 2OVER 2DROP 2ROT))

(setq *all-primitives*
 '( ;       Min. stack size       Change in stack size.
    DROP        ; 1                  -1
    DUP         ; 1                  +1
    SWAP        ; 2                   0
    OVER        ; 2                  +1
    2DROP       ; 2                  -2
    2DUP        ; 2                  +2
    ROT         ; 3                   0
    2SWAP       ; 4                   0
    2OVER       ; 4                  +2
    2ROT        ; 6                   0
  ))

(setq *primitives* *all-primitives*)

/* Only "non-destructive" no-stack-size-changing primitives which do not
    require more than 4 elements: */
(setq *s-primitives* '(SWAP ROT 2SWAP))


(defun init-primitives ()
       (setplist 'SWAP   '(n1 n2 -- n2 n1))
       (setplist 'DUP    '(n1 -- n1 n1))
       (setplist 'OVER   '(n1 n2 -- n1 n2 n1))
       (setplist 'ROT    '(n1 n2 n3 -- n2 n3 n1))
       (setplist 'DROP   '(n1 -- ))
       (setplist '2SWAP  '(n1 n2 n3 n4 -- n3 n4 n1 n2))
       (setplist '2DUP   '(n1 n2 -- n1 n2 n1 n2))
       (setplist '2OVER  '(n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2))
       (setplist '2DROP  '(n1 n2 -- ))
       (setplist '2ROT   '(n1 n2 n3 n4 n5 n6 -- n3 n4 n5 n6 n1 n2)))
; Remember ! (bottom ... top -- bottom ... top)


(defun get-destination (format-list)
       (reverse (cdr (memq '-- format-list))))


(defun get-source (format-list)
       (reverse (to-the-first '-- format-list)))


(defun getdestination (primitive)
       (get-destination (plist primitive)))

(defun getsource (primitive)
       (get-source (plist primitive)))



(defun check-primitives (primitives)
       (mapcar #'(lambda (X) (check-source-part
       		  (get-source (plist X))))
       	        primitives))


(defun duplp (lista) (memq (car lista) (cdr lista)))


(defun contains-duplp (lista)
       (cond ((null lista) ())
       	      (t (or (duplp lista) (contains-duplp (cdr lista))))))


(defun check-source-part (source-list)
       (cond ((contains-duplp source-list)
       	      (error
               "(source-part of format list contains duplicated elements)"))))




(defun theseus (alkio lista &optional thread)
   (cond ((atom lista) ())
         ((equal alkio (car lista))  (reverse (cons 'car thread)))
         ((equal alkio (cadr lista)) (reverse (cons 'cadr thread)))
         ((equal alkio (cdr lista))  (reverse (cons 'cdr thread)))
         (t (or (theseus alkio (cddr lista) (cons 'cddr thread))
                (theseus alkio (cdr lista)  (cons 'cdr  thread))
                (theseus alkio (car lista)  (cons 'car  thread))
            )
         )
   )
)



(defun terrace  (lista &optional option)
       (cond ((null (cdr lista)) (cond ((equal option :SPECIAL) (car lista))
	      				 (t lista)))
       	      (t (list (car lista) (terrace (cdr lista) option)))))



(defun n-exprs (n expr)
       (cond ((zerop n) ())
       	      (t (cons expr (n-exprs (sub1 n) expr)))))


(defun threadtoexpr (thread argument)
       (terrace (append (reverse thread) (list argument)) :SPECIAL))


(defun compile-primitives (primitives &optional stream)
   (mapcar
     #'(lambda (p stream)
         (progn
               (terpri stream)
               (terpri stream)
               (terpri stream)
               (princ "; " stream)
               (princ p stream)
               (princ `\t` stream)
               (print (plist p) stream)
               (eval (print (compile1 p) stream))
         )
       )
      primitives (mcl (if (endmarkp stream) *stdout* stream))
   )
)



/* This was for Xlisp:
(defun compile1 (primitive)
       (funcall (function (lambda (source destination)
       `(defun ,primitive (XXX)
       	        (cond ((or (< (length XXX) ,(length source))
			    (*** ,primitive ,(plist primitive)))
			     '(*UNDERFLOW*))
			(t
      ,(compile-aux (threadtoexpr (n-exprs (length source) 'cdr) 'XXX)
       		     source destination))))))
       	  (getsource primitive)
           (getdestination primitive)))
*/

(defun compile1  (primitive &aux source destination srclen)
  (setq source (getsource primitive))
  (setq destination (getdestination primitive))
  (setq srclen (length source))
  (list 'defun primitive '(XXX)
        (list
          'if (list 'lessp '(length XXX) srclen)
          '(quote (*UNDERFLOW*))
          (compile-aux (threadtoexpr (n-exprs srclen 'cdr) 'XXX)
              source destination)
        )
  )
)


(defun compile-aux (rest-of-stack source destination)
       (cond ((null destination) rest-of-stack)
       	      (t (list 'cons
	      	    (threadtoexpr (theseus (car destination) source ()) 'XXX)
		     (compile-aux rest-of-stack source (cdr destination))))))





(defun make-pairlist (list1 list2)
       (cond
             ((or (null list1) (null list2)) ())
              (t
                 (cons (cons (car list1) (car list2))
                        (make-pairlist (cdr list1) (cdr list2))))))

(defun pairlist-to-2lists (pairlist)
       (list (mapcar 'car pairlist) (mapcar 'cdr pairlist)))

(defun subsetp (a b) ; Is a subset of b ?
       (cond
             ((null a) t)
              ((memq (car a) b) (subsetp (cdr a) b))
               (t ())))


(defun definedp (fun-name)
       (and (boundp fun-name)
             (listp (getd fun-name))
;             (listp (car (getd fun-name)))
	       (or (eq (car (getd fun-name)) 'lambda)
	    	    (eq (car (getd fun-name)) 'macro))))

(setq *M* ())
(setq *LENLIM* 4)
(setq *DEFAULT_PRIMLIM* -1)
(setq *stream* *stdout*)

(defun maybe-continue (x)
       (if *M* (progn (print x *stream*) (drain *stream*) ()) (return x)))


;
;
;

(defun brief () (setq *primitives* *s-primitives*))


(defun solve-perm  (lista &optional primlim lenlim filename &aux flist)
   (cond ((not (consp lista))
            (princ
"usage: (solve-perm '(a b c d) [primitive_limit] [length_limit] [output_file])"
            )
            (terpri)
            (princ "(setq *M* t) for multiple solutions !") (terpri)
            (princ
 "And use function (brief) for solutions using only SWAP, ROT and 2SWAP.")
            (terpri)
            (reset)
         )
   )
   (setq *stream*
     (if (or (endmarkp primlim) (endmarkp lenlim) (endmarkp filename))
         *stdout*
         (outfile filename 'a)
     )
   )
   (setq perms (permute lista))
   (while perms
          (setq flist (append lista (cons '-- (car perms))))
          (terpri *stream*) (terpri *stream*)
          (print flist *stream*) (drain *stream*)
          (print (solve flist primlim lenlim) *stream*)
          (setq perms (cdr perms))
   )
   (if (not (eq *stream* *stdout*)) (close *stream*))
)

;
; This should be called for example as:
;
;  (solve '(a b c d -- d c b a))
;
; And program tries to find solution which turns four topmost elements
;  in "stack" upside down. If global variable *M* is non-nil, then multiple
;  solutions are searched.
; Optional variables primlim and lenlim determine how many primitives
;  the solution is allowed to contain and how many more elements than 
;  final goal-sequence any sequence on the path is allowed to contain.
;
;

(defun solve  (format-list &optional primlim lenlim)
       (setq *primlim* (if (endmarkp primlim) *DEFAULT_PRIMLIM* primlim))
       (setq *goal-seq* (cdr (memq '-- format-list)))
       (setq *seqlen-uplim*
             (+ (length *goal-seq*)
                (if (or (endmarkp primlim) (endmarkp lenlim)) *LENLIM* lenlim)
             )
       )
       (setq *allowed-primitives* *primitives*)
       (breadth-search1
              (topcopy '(()))
    (topcopy (list (topcopy (list (topcopy (to-the-first '-- format-list))))))
       )
)


(defun breadth-search1 
    (queue-of-primitives queue-of-seqs)
      (while queue-of-seqs
        (cond
          ((and (equal (car (car queue-of-seqs)) *goal-seq*)
                  (maybe-continue
                   (list (reverse (car queue-of-primitives))
                          (reverse (car queue-of-seqs))))))
             (t
                (*expand queue-of-primitives queue-of-seqs)
                (setq queue-of-primitives (cdr queue-of-primitives))
                (setq queue-of-seqs (cdr queue-of-seqs))
             )
        )
     )
)


(defun *expand  (queue-of-primitives queue-of-seqs 
                 &aux primitives test-primitive temp-seq temp2seq
                          path-of-primitives path-of-seqs)
 (setq path-of-primitives (car queue-of-primitives))
 (if (eq (length path-of-primitives) *primlim*)
     '*PRIMITIVE_LIMIT*
/* Else: */
   (setq path-of-seqs       (car queue-of-seqs))
   (setq primitives *allowed-primitives*)
   (while (setq test-primitive (car primitives))
          (setq temp-seq (topcopy (car path-of-seqs)))
          (setq temp2seq (eval-primitive test-primitive temp-seq))
          (if (equal temp2seq *goal-seq*)
                  (add-to-queues queue-of-primitives
                                 path-of-primitives
                                 test-primitive
                                 queue-of-seqs
                                 path-of-seqs
                                 temp2seq
                  )
/* If primitive returned (*UNDERFLOW*) then break from loop because there's
   only primitives left which require more elements than is in temp-seq: */
              (if (eq (car temp2seq) '*UNDERFLOW*) (return '*ALIVUOTO*))
              (if (check-seq temp2seq path-of-seqs)
                  (progn /* (free-list temp-seq) */ 
                            (free-list temp2seq)
                  )
/* Else */
                  (add-to-queues queue-of-primitives
                                 path-of-primitives
                                 test-primitive
                                 queue-of-seqs
                                 path-of-seqs
                                 temp2seq
                  )
              )
          )
          (setq primitives (cdr primitives))
   )
/*
   (free-list path-of-seqs)
   (free-list path-of-primitives)
   queue-of-primitives
 */
 )
)


(defun add-to-queues (queue-of-p path-of-p p queue-of-s path-of-s s)
       (putlast queue-of-p (cons p path-of-p))
       (putlast queue-of-s (cons s path-of-s))
)

(defun check-seq (new-seq old-seqs)
       (or ; new-seq is invalid in following cases:
; If new-sequence is longer than *seqlen-uplim*
; Only needed with (2)DUP and (2)OVER:
            (greaterp (length new-seq) *seqlen-uplim*)
; If circular sequence-paths?
            (member new-seq old-seqs)
; If in new sequence there is not something what is in *goal-seq* then
; stack elements are lost so there is no sense to continue:
; (Only needed with DROP and 2DROP)
            (not (subsetp *goal-seq* new-seq))
/* This test is now in *expand:
; If primitive requires more elements than is in sequence:
            (equal  new-seq '(*UNDERFLOW*))
*/
       )
)

(setq eval-primitive funcall)


/* Apply primitive sequence (like (ROT SWAP ROT) ) to itself, and
    return list of intermediate results (last one is true result):
 */
(defun self-apply (primseq) (apply-seq-aux primseq (list primseq)))

(defun apply-seq-aux (prims result)
   (cond ((null prims) (nreverse result))
          (t
             (apply-seq-aux (cdr prims)
                            (cons (eval-primitive (car prims) (car result))
                                  result)
             )
          )
   )
)

(defun N () (self-apply (car (last $))))


/* Apply first of primseq continuously to primseq, until primseq comes to
    nil or underflow occurs:
 */

(defun cap (primseq)
   (while (and primseq (neq (car primseq) '*UNDERFLOW*))
      (print (setq primseq (eval-primitive (car primseq) primseq)))
   )
)


; Permutation stuff:

(defun permute (lista)
   (cond ((null lista) ())
         ((null (cdr lista)) (list lista))
          (t
             (mapcan  #'(lambda (first rest len)
                         (permute-apu first rest len ())
                        )
                       (mcl (car lista))
                       (permute (cdr lista))
                       (mcl (length lista))
             )
          )
   )
)

(defun permute-apu (alkio lista n result)
       (while (not (zerop n))
              (setq n (sub1 n))
              (setq result (cons (insert-to-n alkio lista n) result))
       )
       result
)


/* This doesn't work properly if the result of mapcar is longer than current
    max-args, set with function (set-max-args n)
   (Write better mapcan with C...)

(defun mapcan3 (fun list1 list2 list3)
       (apply #'nconc (mapcar fun list1 list2 list3)))

 (* So use this (old) one:
(defun mapcan3 (fun list1 list2 list3)
       (nconc-together (mapcar fun list1 list2 list3)))

(defun nconc-together (l)
   (cond ((null l) ())
          (t (nconc (car l) (nconc-together (cdr l))))))
 */

/*
(defun old-insert-to-n (alkio lista n)
   (cond ((zerop n) (cons alkio lista))
          (t (cons (car lista) (insert-to-n alkio (cdr lista) (sub1 n))))
   )
)
 */

; Now utilizing the built-in function linsert to do the job.
; Note that because it's destructive we must copy the top level of lista
;  before giving it to linsert:
(defun insert-to-n (alkio lista n)
       (linsert (list alkio) (topcopy lista) n))


(init-primitives)

(compile-primitives *primitives*)

