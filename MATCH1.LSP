
(setq then progn) ; "macro".


(defun match (patterns datums &aux pat item var tmp)
  (while t
    (setq pat  (car patterns))
    (setq item (car datums))
    (cond 

      ((null datums) ; If datums finish.
          (cond ((null patterns) (return t)) ;Succ if both are in the end
                ((eq pat '*)) ;If * or (?VAR *) then continue.
                ((and (consp pat) (eq (cadr pat) '*)))
                ((return ())) ;Otherwise fails.
          )
      )

      ((null patterns) (return ())) ;Fail if patterns finished, but datums not

      ((or  (eq pat '?) (eq pat item))) ;If ? or identical elems, then cont.

      ((eq pat '*)
        (return (or (match patterns (cdr datums))
                    (match (cdr patterns) datums))
        )
      )

      ((stringp pat) (if (not (istrmatchp item pat)) (return ())))

      ((atom pat) (return ())) ; Losing atom.

      ((eq (setq var (car pat)) '/)
        (if (not (match-list item (cdr pat))) (return ()))
      )

      ((eq (setq pat (cadr pat)) '?) (set var item))

      ((eq pat '*)
         (set var (cons item (consp (symeval var))))
         (if (match patterns (cdr datums)) (return t)
             (set var (free-cons (symeval var)))
             (return (match (cdr patterns) datums))
         )
      )

      ((stringp pat)
        (if (not (setq tmp (istrmatchp item pat))) (return ())
            (set var item) (setplist var tmp)
        )
      )

      ((and (consp pat) (eq (car pat) '/))
        (if (not (setq tmp (match-list item (cdr pat)))) (return ())
            (set var (car tmp))
        )
      )

      ((nonnilsymbolp pat)
        (if (not (setq tmp (funcall pat item))) (return ())
            (set var tmp)
        )
      )

      (t (return ()))

    ) ; cond

  (setq patterns (cdr patterns))
  (setq datums   (cdr datums))
  ) ; while
)


(defun match-list (item lista)
       (car (memq item (car lista))))

;
; Simple doctor program (Eliza) from page 386 of Lisp.
;
; 
(defun doctor (&aux s a-list mother)
  (setq mother ())
  (print '(Speak up!))
  (terpri)
  (while (setq s (read))
     (cond ((setq a-list (match '(i am worried (?l *)) s))
              (print (append '(How long you have been worried)
                              (nreverse ?l)
                              (setq ?l ())
                     )
              )
            )

           ((match '(* mother *) s)
            (setq mother t)
            (print '(Tell me more about your family))
           )

           ((match '(* computers *) s)
            (print '(Do machines frighten you))
           )

           ((or (match '(no) s)
                (match '(yes) s)
            )
            (print '(Please do not be so short with me))
           )
 
           ((match '(* ($tuhma badwordp) *) s)
            (print (append '(Please do not use words like) (list $tuhma)))
           )

           (mother (setq mother nil)
                   (print '(earlier you spoke of your mother))
           )

           (t (print '(I am sorry, our time is up))
              (return 'goodbye)
           )
     )
  )
)


(defun badwordp (word)
       (memq word '(shucks hell darn fuck cunt cock asshole)))

