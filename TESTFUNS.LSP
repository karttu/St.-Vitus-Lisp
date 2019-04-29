


(setq alli
 '((a l l i) (p a m a u t t i) (v i t t u a) (m a p i l l a)))
(setq sika
  '((i s o) (r i k a s) (s i k a) (s | k |) (s a k i s s a) (k i r o s i)))
(setq isi '((I S I) (A H !) (E L L I) (K A N I) (N A K I L L E) (H A I S I)))

(defun soy (sauce)
       (soy (add1 (print sauce))))

(defun ! (x)
       (cond ((zerop x) 1)
              ((* x (! (sub1 x))))))

(defun fib (n) /* From LISP, page 65 */
       (cond ((zerop n) 1)
              ((eq n 1) 1)
               (t (+ (fib (sub1 n))
                      (fib (- n 2))))))

(defun count-atoms (l) ; From LISP, page 69
       (cond ((null l) 0)
              ((atom l) 1)
               ((+ (count-atoms (car l))
                    (count-atoms (cdr l))))))


(defun mystery (s) ; From LISP, page 71, counts maximum depth of list
       (cond ((null s) 1)
              ((atom s) 0)
               (t (max (+ (mystery (car s)) 1)
                        (mystery (cdr s))))))

(defun strange (l) ; From LISP, page 71, copies a list (at every level)
       (cond ((null l) nil)
              ((atom l) l)
               (t (cons (strange (car l))
                         (strange (cdr l))))))


/* Problem 4-3 from LISP, page 71, solution at page 342 */
(defun tail-reverse (lista) (tail-reverse-aux lista ()))


(defun tail-reverse-aux (lista rev)
       (cond ((null lista) rev)
              (t (tail-reverse-aux (cdr lista) (cons (car lista) rev)))))



/* Rekursiivisia funktioita Hyvosen & Seppasen Lisp-kurssista,
    Prosessori 3/1983:sta:
 */

(defun *member (alkio lista)
       (cond ((null lista) nil)
              ((equal (car lista) alkio) lista)
               (t (*member alkio (cdr lista)))))

(defun *append (vasen oikea)
       (cond ((null vasen) oikea)
              (t (cons (car vasen)
                        (*append (cdr vasen) oikea)))))

(defun *remove (alkio lista)
       (cond ((null lista) nil)
              ((equal alkio (car lista)) (*remove alkio (cdr lista)))
               (t (cons (car lista)
                         (*remove alkio (cdr lista))))))

(defun *reverse (lista)
       (cond ((null lista) nil)
              (t (append (*reverse (cdr lista))
                           (cons (car lista) nil)))))

(defun *deepreverse (lista)
       (cond ((atom lista) lista)
              ((null (cdr lista)) (cons (*deepreverse (car lista)) nil))
               (t (append (*deepreverse (cdr lista))
                           (*deepreverse (cons (car lista) nil))))))

(defun latista (lista) (litistys lista nil))

(defun litistys (lista tulos)
       (cond ((null lista) tulos)
              ((atom lista) (cons lista tulos))
               (t (litistys (car lista)
                             (litistys (cdr lista) tulos)))))

(defun rewerse (lista)
       (cond ((null (cdr lista)) lista)
              (t (cons (car (rewerse (cdr lista)))
                        (rewerse (cons
                                   (car lista)
                                   (rewerse (cdr (rewerse (cdr lista))))))))))
                                                

; Count how many times rewerse is called when rewersing lista of length n:
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


