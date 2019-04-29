

; From LISP Second Edition, page 348 (Solution 4-24).

(defun rewrite (l)
   (cond ((atom l) l)

         ((eq (car l) 'nand)
            (list 'nand
                  (rewrite (cadr l))
                  (rewrite (car (cddr l)))
            )
         )

         ((eq (car l) 'not)
            (rewrite (list 'nand (cadr l) t))
         )

         ((eq (car l) 'and)
            (rewrite (list 'not
                           (list 'nand (cadr l) (car (cddr l)))))
         )

         ((eq (car l) 'or)
            (rewrite (list 'nand
                           (list 'not (cadr l))
                           (list 'not (car (cddr l)))))
         )

         ((eq (car l) 'xor)
            (rewrite (list 'and
                           (list 'or (cadr l) (car (cddr l)))
                           (list 'or
                                 (list 'not (cadr l))
                                 (list 'not (car (cddr l))))))
         )

         (t (list 'error l))
   )
)

