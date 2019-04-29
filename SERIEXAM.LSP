;
; SERIEXAM - Mathematical series examine software by Antti Karttunen.
; (Jan-Feb 1992)
; Tries to find the successor for given numerical series.
; If doesn't find, then returns ().
; Example: (test squares)
;
;
; Program (namely function test-aux which does the most of work)
; tries to find some pattern in series given as argument for it.
; There is two basic patterns:
; (X X X ... X) i.e. series contains just one and same number
; (tested with all-samep). In that case successor is of course
; the same element.
; Another pattern is that the "child series" is the same as the
; beginning of "parent series", e.g. like (1 2) and (1 2 4),
; so the successor is the first one missing from "child series",
; in this case 4. Actually it's also tested so that the first one
; in child-series is ignored, because it can be some "garbage"
; number. For example, with fibonacci series:
;(test fibs)
;(> 1 1 2 3 5 8 13 21 34 55)
;(- 0 1 1 2 3 5 8 13 21)
;(+ 55 34)
;(1 1 2 3 5 8 13 21 34 55 89)
;
; If the child series doesn't match the above two cases, then
; the differences of the adjacent numbers are computed
; (resulting one element shorter list), and again this
; is tested recursively if it contains any pattern.
; If that doesn't find anything, then remainders and quotients
; of the adjacent numbers are generated, and pattern is searched
; from those lists (recursively). When the successor number is finally
; found, it is returned together with the last child-series, and when
; coming back from recursion the expression is constructed from
; the last numbers of the child-lists together with the corresponding
; operators. When back at the top level, the child series leading to
; solution are printed, the constructed expression is printed, evaluated,
; and appended to original series given as argument to test function.
;
; In some cases method works also to descending direction, if you
; try something like (test (reverse (cddr cubes))) you should find
; the number 8, which is predecessor to the number 27.
; However, with St. Vitus' Lisp this doesn't work (yet!) with the
; most of cases, as *, / and % operations work as unsigned.
; So usually numbers should be in the ascending order.
;
; In future algorithm could be expanded to handle more cases of basic
; patterns, for example the following ones:
; (X Y X Y X Y ...) i.e. an alternating pattern.
;
; (<X1 f(1) times> <X2 f(2) times> <X3 f(3) times>, etc.),
; i.e. pattern where the count of elements are significant
; (simple example: (1 2 2 3 3 3 4 4 4 4 ...)).
;
; Maybe also the sums (or even differences/products) of the digits
; of the series elements should be considered. (In different bases?)
;
; And algorithm should also be aware of the tricks where the digits
; of original series is reversed, or some other simple "textual"
; trick is made. For example: (61 52 63 94 46) (successor should be 18).
; 
;
; Note that this code uses some functions peculiar for St. Vitus' Lisp.
;
; (while test-expr [expr1 [expr2 [expr3 [... exprn]]])
; executes the test-expr, and exits if it's nil (returns number
; of times "went around in loop"). Otherwise the expressions
; after it are executed from left to right, and then test-expr
; is executed once again, etc.
; If return expression is encountered somewhere inside the while
; then it is exited immediately, so that while returns the value
; specified in return, instead of the loop count. E.g. (return ())
; forces while to stop and return the nil.
; Create corresponding macro using do-function, or do something else.
;
; free-cons and free-list free their arguments to free memory, to be
; usable later by cons. (As St. Vitus' Lisp has no Garbage Collector).
; Just remove them with other lisps, or define them as null macros.
;
; (print expr) should return expr as its value.
;
; if works like this:
; (if test-expr then-expr [else-expr1 [else-expr2 [... else-exprn]]])
; If test-expr is non-nil returns then-expr, otherwise executes
; else-expr<s> and returns the value of last of them.
; (nil if there is no else-expr's).
;
; member should know the keyword :TEST-NOT or you should code
; the function all-samep differently.
;
; #' is read macro for function quote. E.g. #'fun-name => (function fun-name)
;
; All the arithmetic in St. Vitus' Lisp is 16-bit unsigned, although
; signed printing is possible by executing the following function:
;(rplacx 0 *integer-printtypes* "%d")
;
; Arithmetic functions relevant to this program:
;
; +  add two integers
; -  get difference of two integers.
; /  get quotient of two integers, result is also integer.
; %  get remainder of two integers.
;

; ======================================================================
; Some testing functions not really needed with function test:

; Ackerman's function. Don't really know what this does in this file...
(defun ack (a b)
       (cond ((lessp a 2) (* 2 b))
             ((lessp b 2) a)
             (t (ack (sub1 a) (ack a (sub1 b))))
       )
)

; X power Y. Both must be integers, Y also non-negative.
; Very simple algorithm.
(defun pow (x y)
   (cond ((zerop y) 1)
         ((* x (pow x (sub1 y))))
   )
)

; Apply function fun to all elems with argument x, and return the
; corresponding list. fun should take two arguments.
; Example: (setq kala (apply-to-all #'+ '(1 2 3 4 5) 5)) returns
; (6 7 8 9 10)
(defun apply-to-all (fun elems x)
   (setq elems (mapcar fun elems (setq x (mcl x))))
   (free-cons x) ; Free circular list made as argument for mapcar.
   elems         ; Return the result.
)

; Return the dotted pair (quotient . remainder)
(defun divide (x y) (cons (/ x y) (% x y)))

; Return the list of those dotted pairs when applied to number list.
; Not really needed, except for debugging.
(defun getdivs (nums)
   (mapcar #'divide (cdr nums) nums))


; ========================================================================
;              Functions needed for the test function
;  getdiffs, getfactors, all-samep, sublistp, test, test-aux, test1, test-1


; This returns a list of differences of the adjacent numbers in a list nums.
; That is, a number is always subtracted from the next number to right.
; Result list is one element shorter.
(defun getdiffs (nums)
   (mapcar #'- (cdr nums) nums))



; This returns quotients & remainders, or () if there is zero somewhere
; as divisor. Sorry, the name is inconsistent.
; Result is like (quotient-list . remainder-list) if not ().
;
(defun getfactors (nums &aux quotients remainders)
   (setq quotients ())  ; These statements not needed
   (setq remainders ()) ; in other lisps than St. Vitus'
   (if (while (cdr nums) ; Continue as long as length is at least 2
; If divisor is zero then free the result lists, and break from the
; while loop:
         (if (zerop (car nums))
             (progn (free-list quotients) (free-list remainders)
                    (return ())
             )
         )
         (setq quotients  (cons (/ (cadr nums) (car nums)) quotients))
         (setq remainders (cons (% (cadr nums) (car nums)) remainders))
         (setq nums (cdr nums))
       ) ; If while loop is not exited with (return ()) it returns non nil
; and this cons is returned:
       (cons (nreverse quotients) (nreverse remainders))
   ) ; End of if.
)


; If your lisp's member doesn't know keywords (Common Lisp knows them,
; but for example Franz Lisp doesn't), then code this with some other
; way. This should test that all the elements are same in nums, and it's
; longer than one element long.
(defun all-samep (nums) ; All elements same ?
   (not (member (car nums) (cdr nums) :TEST-NOT #'equal)))


; If l2 is a sublist of l1, then return the next following element
; in l1, otherwise ().
; Example: (sublistp '(a b c d e) '(a b c)) returns d
(defun sublistp (l1 l2) ; Is l1 starting with l2 ?
   (cond ((null l2) (car l1))  ; If l2 is finished, but l1 is not, then
;                                return the next one in l1.
         ((null l1) ()) ; If other way then false.
         ((equal (car l1) (car l2)) ; If first elems equal then recurse...
             (sublistp (cdr l1) (cdr l2))
         )
   ) ; Otherwise false.
)


; If *DEBUG* is non-nil, then print all the lists recursed to, and before
; them the sign of operation how they were derived from the "parent"
; list.
(setq *DEBUG* ())

(setq *KALA* t) ; Debug variable KALA, remove this later!

; test is the main function. It takes the series list, and tries to
; find successor for it. If can't find anything, then returns ().
; But if finds, then prints all the "child-lists" leading to solution
; and after them the expression constructed to evaluate the successor value
; is printed, and a new list list is returned where that successor is
; appended to the end of argument list.
; (So that (test $) can be repeated in St. Vitus' Lisp to get more
; successors.)
;

; Example of test:
;->(test squares)
;(> 1 4 9 16 25 36 49 64 81)   ; > indicates test was started with this list.
;(- 3 5 7 9 11 13 15 17)       ; - indicates that this list was derived from
;(- 2 2 2 2 2 2 2)             ;   the previous list by getdiffs function.
;(+ 81 (+ 17 2))               ; Expression used to evaluate the next member.
;(1 4 9 16 25 36 49 64 81 100) ; Which is appended to the list given as arg.
;->

(defun test (nums &aux x)
   (setq x (test-aux () nums '>)) ; Call the main routine.
   (mapcar #'print (cdr x)) ; Print the lists, if any.
; Evaluate it and append it to end (if it's not nil):
   (if x
       (append nums (list (eval (print (car x)))))
   )
) ; If it's not nil...


; Returns list of the form:
; (result-expression nums nums nums)
; and nums are of the form (cons sign nums2) from each level.
; nums1 is "parent series", and nums2 is "child series".
;
(defun test-aux (nums1 nums2 sign &aux x y z)
   (if (and (boundp '*DEBUG*) *DEBUG*) (progn (princ sign) (print nums2)))
   (cond ((null (cdr nums2)) ()) ; If length is less than 2, then fail.
; If all the elements are equal, then continue with the same element:
         ((all-samep nums2) (cons (car nums2) (list (cons sign nums2))))
; If nums2 series begin just as nums1 ("parent"-series), then
; the successor element should be of course the next one in nums1.
         ((or (setq x (sublistp nums1 nums2))
              (and *KALA* (setq x (sublistp nums1 (cdr nums2)))))
            (cons x (list (cons sign nums2)))
         )
; Test if successor is found for the difference list:
         ((setq x (test-aux nums2 (getdiffs nums2) '-))
            (cons (list '+ (car (last nums2)) (car x))
                  (cons (cons sign nums2) (cdr x))
            )
         )
; Get quotients & remainders if there is no zero divisors:
         ((setq x (getfactors nums2))
            (if (setq y (test-aux nums2 (cdr x) '%)) ; Test remainders
                (if (setq x (test-aux nums2 (car x) '/)) ; Test quotients
; If found successor both for remainders and quotients, then:
                    (progn ; Construct multiplication expression:
                       (setq z (list '* (car (last nums2)) (car x)))
; If remainder is zero then return just mult-expr, otherwise add remainder:
                       (cons (if (zerop (car y)) z (list '+ z (car y)))
                             (cons (cons sign nums2) 
                                   (nconc (cdr y) (cdr x)) ; Should be safe?
                             )
                       )
                    )
                )
            )
         )
   )
)


; For testing series with one added to each element.
(defun test1  (x) (test (mapcar #'add1 x)))

; For testing series with one subtracted from each element.
(defun test-1 (x) (test (mapcar #'sub1 x)))



; Example series.
; After the series in list there are one or more "should-be" successor
; numbers after ;-comment char, and in the end of row a character !,?,- or
; + indicating how well
; test finds a successor. (Or verbal explanation how test behaves with
; that series).
; (meaning of symbols: ! = Doesn't find a successor, ? finds a successor
; but "incorrect" one, - finds a successor, but not when some constant
; is added or subtracted from the numbers (test1 & test-1).
; + Finds a correct successor even in that case.

; Next one is the sum of the two previous (Fibonacci numbers)
(setq fibs  '(1 1 2 3 5 8 13 21 34 55)) ; 89                               +

; Also in this one. (Lucas numbers)
(setq lucas '(1 3 4 7 11 18 29 47 76)) ; 123                               +

; Next one is the sum of the three previous. (Tribonacci numbers)
(setq tribs '(1 1 2 4 7 13 24 44 81 149)) ; 274                            !
; Another one:
(setq tribs2 '(1 1 1 3 5 9 17 31 57 105)) ; 193 355                        !

; Next one is square of the previous, or Xn = 2^(2^n)
(setq squareprev '(2 4 16)) ; 256 65536 (= 0)                              -

(setq powers-of2 '(1 2 4 8 16 32 64 128 256)) ; 512                        +

(setq powers-of3 '(1 3 9 27 81 243 729 2187 6561)) ; 19683                 +

(setq squares '(1 4 9 16 25 36 49 64 81)) ; 100 121                        +

(setq cubes   '(1 8 27 64 125 216 343 512 729)) ; 1000 1331                +

; n^4
(setq sq4     '(1 16 81 256 625 1296 2401 4096 6561)) ; 10000 14641        +

; n^5
(setq sq5     '(1 32 243 1024 3125 7776 16807)) ; 32768                    +

; n^n
(setq npowern '(1 4 27 256 3125)) ; 46656 n^n                              !

(setq tbl     '(4 7 10 16 28 52)) ; Titius Bode's Law for Merkurius - Jupiter
; 100 = Saturnus.
; (test (cdr tbl)) works correctly, but (test tbl) returns "incorrect" 97
; because Merkurius is really an exception in this series.
;
(setq cdr_of_tbl '(7 10 16 28 52)) ; Titius Bode's Law for Venus - Jupiter +
; 100 = Saturnus.

(setq sums '(1 3 6 10 15)) ; 21                                            +

(setq factorials '(1 2 6 24 120 720)) ; 5040 40320                        ?-
; Returns "incorrect" result 5872 instead of 5040, if *KALA* is non-nil
; as default, but if *KALA* is set to () then returns the correct 5040.
; (but in that case many other series cease to work, e.g. fibs, lucas
; mult2last).
; (test-1 factorials) nor (test1 factorials) still find nothing.
; (I.e. if some constant is added to the factorials).

; 2*2+1=5, 3*5+1=16, 4*16+1=65, etc.
(setq faq1 '(2 5 16 65 326 1957)) ; 13700                                  +

; 2*2+2=6, 3*6+3=21, 4*21+4=88, 5*88+5=445, etc.
(setq faq2 '(2 6 21 88 445 2676)) ; 18739                                  +

; Previous times 5 plus 7:
(setq hamyt '(3 22 117 592 2967)) ; 14842                                  +


; Various formulas for generating some amount of primes:
; I got these from "The Penguin dictionary of curious and interesting
; numbers", by Dawid Wells 1986, reprinted 1988. ISBN 0-14-008029-5

; n^2 + n + 17
(setq primes0 '(17 19 23 29 37 47 59 73 89 107 127 149 173 199 227)) ; 257 +

; 2n^2 + 29
(setq primes1 '(29 31 37 47 61 79 101 127 157 191 229 271 317 367
 421 479 541 607 677 751 829 911 997 1087 1181 1279 1381 1487)); 1597      +

; x^2 + x + 41
(setq primes2 '(41 43 47 53 61 71 83 97 113 131 151 173 197 223 251
 281 313 347 383 421 461 503 547 593 641 691 743 797 853 911 971 1033
 1097 1163 1231 1301 1373 1447 1523)); 1601                                +

; 199 + 210n
(setq primes3 '(199 409 619 829 1039 1249 1459 1669 1879)) ; 2089          +

; * 2 + 1 -> Six primes.
(setq primes4 '(89 179 359 719 1439)) ; 2879                               +

; * 3 + 16 -> 5 primes.
(setq primes5 '(587 1777 5347 16057)) ; 48187                              +

; Next one is the product of the two previous.
(setq mult2last '(1 2 2 4 8 32 256)) ; 8192 2097152                        -

; Note that (test (cddr mult2last)) finds the successor element
; with different criteria. And list starts to grow with 2480
; after 256, instead of 8192. And every member of the series
; is the result of the function g(nth). I.e. g(0) -> 2, g(1) -> 4, etc
;
; Like this:              0 1 2 3  4       5    6     7
(setq cddr_of_mult2last '(2 4 8 32 256)) ; 2480 24704 246928               +

; This returns result for function 2*(1+n+(((10^n-1)/9)-n)/9))
; (If using 16-bit arithmetics as in St. Vitus' Lisp, then from (g 5)
; onward the results are incorrect.
(defun g (n)
   (* 2 (+ (add1 n) (/ (- (/ (sub1 (pow 10 n)) 9) n) 9))))


; Same defined as recursive function: (Correct answers to (f 6) with
; 16-bit arithmetics.)
(defun f (n)
   (cond ((zerop n) 2)
         (t (+ (f (sub1 n)) (f2 (sub1 n))))
   )
)

(defun f2 (n)
   (cond ((zerop n) 2)
         (t (+ (f2 (sub1 n)) (* 2 (pow 10 (sub1 n)))))
   )
)


; Series for the rewerse recursion count:
(setq rews '(1 1 5 17 57 189 625 2065 6821)) ; 22529 74409                 +

; Series for the rev3-count:
(setq rews3 '(1 1 1 4 10 25 61 148 358 865 2089 5044 12178)) ; 29401 70981
; (test rews3) returns (), but (test (cdr rews3)) returns 29305, an
; "incorrect" result (see below).

;Note that rews is the series for the rewerse-count(0), rewerse-count(1), etc
; Here is the function:
;
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

; And here is that notorious hyper-recursive version of reverse:
; (No other functions than the most fundamental cond, null, car, cdr,
; cons, and of course rewerse itself are used.)
(defun rewerse (lista)
       (cond ((null (cdr lista)) lista)
              (t (cons (car (rewerse (cdr lista)))
                        (rewerse (cons
                                   (car lista)
                                   (rewerse (cdr (rewerse (cdr lista))))))))))


; This returns the same result for the same kind of function,
; but which "reverses the stack" instead of list. (for n stack elements).
; (See STACKS.LSP if present.)
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


; Example:
;->(test rews)
;(> 1 1 5 17 57 189 625 2065 6821)
;(- 0 4 12 40 132 436 1440 4756)
;(- 4 8 28 92 304 1004 3316)
;(- 4 20 64 212 700 2312)
;(- 16 44 148 488 1612)
;(- 28 104 340 1124)
;(- 76 236 784)
;(% 8 76)
;(/ 3 3)
;(+ 6821 (+ 4756 (+ 3316 (+ 2312 (+ 1612 (+ 1124 (+ (* 784 3) 236)))))))
;(1 1 5 17 57 189 625 2065 6821 22529)
;->
; Note that first three elements of the lists computed by continuing
; subtraction seem to satisfy the relation 3*second + first = third.
; Similarly, difference lists of (test (cdr rews3)) seem to satisfy
; the relation 2*second + first = third:
; (This is all probably very trivial, but I have forgot my mathematics...)
;->(test (cdr rews3))
;(> 1 1 4 10 25 61 148 358 865 2089 5044 12178)
;(- 0 3 6 15 36 87 210 507 1224 2955 7134)
;(- 3 3 9 21 51 123 297 717 1731 4179)
;(- 0 6 12 30 72 174 420 1014 2448)
;(- 6 6 18 42 102 246 594 1434)
;(- 0 12 24 60 144 348 840)
;(- 12 12 36 84 204 492)
;(- 0 24 48 120 288)
;(- 24 24 72 168)
;(- 0 48 96)
;(- 48 48)
;(+ 12178 (+ 7134 (+ 4179 (+ 2448 (+ 1434 (+ 840 (+ 492 (+ 288 (+ 168 (+ 96 48))))))))))
;(1 1 4 10 25 61 148 358 865 2089 5044 12178 29305)
; The last element "should" be 29401 instead of 29305.
;

(defun print-all ()
  (mapc #'(lambda (s) (terpri) (print s) (print (test (eval s))))
        series
  )
)

(setq series '(fibs lucas tribs tribs2 squareprev powers-of2 powers-of3
squares cubes sq4 sq5 npowern tbl cdr_of_tbl sums factorials faq1 faq2
primes0 primes1 primes2 primes3 primes4 primes5 hamyt mult2last
cddr_of_mult2last rews rews3))

(defun start ()
(terpri)
(princ
"SERIEXAM - Series Examinator, simple program to find the successor for")
(terpri)
(princ
"the `arbitrary' series. Coded by A. Karttunen once upon a time.")
(terpri)
(princ "Example series are: ") (print series)
(princ
 "Use function test to find the successor, if there's any. E.g: (test fibs)"
)
(terpri)
(princ "Enter series to see the available example-series again.")
(terpri)
(princ "Or use (print-all) to print all the series and their successors.")
(terpri) (terpri)
)

(start)




