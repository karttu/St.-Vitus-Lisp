;
; This piece of code written at 10-11. AUG 1991
; by Antti Karttunen �u�A���b�e�B�@�J���b�g�D�l���v
; Routines presented here are Public Domain.
;
;
; Use (start1 1923) and (start1 1983) to produce the Chinese-Japanese
; calendar output with Shift-JIS-kanji-characters.
;
 
(setq eto '(�Z ��)) ; (e=elder) and (to=younger).
 
(setq eto_kun '(e to))
  
(setq gogyoo '(�� �� �y �� ��)) ; �܍s
 
(setq gogyoo_kun '(ki hi tsuchi ka mizu)) ; Kun-readings of gogyoo
 
(setq jikkan '(�b �� �� �� �� �� �M �h �p �)) ; �\��
 
(setq juunishi '(�q �N �� �K �C �� �� �� �\ �� �� ��)) ; �\��x
 
(setq juunishi_kun '(ne ushi tora u tatsu mi uma hitsuji saru tori inu i))
 
(defun assign_values (list1 list2)
       (mapc #'(lambda (x y) (set x y))
             list1 list2)
)
 
(assign_values eto eto_kun)
(assign_values gogyoo gogyoo_kun)
(assign_values juunishi juunishi_kun)
  
(defun start1 vlambda (x filename)
  (testi1 x 1 (if (endmarkp filename) *stdout* (outfile filename 'a))
          jikkan jikkan juunishi juunishi gogyoo gogyoo eto eto))
 
(defun start2 (x) (testi2 x jikkan jikkan juunishi juunishi))

(defun testi1 (year n output set1 pset1 set2 pset2 set3 pset3 set4 pset4)
; Stop when �\�� and �\��x become nil at the SAME time:
   (while (or set1 set2)
; If some of the sequences became nil, then circulate it back to start:
          (if (null set1) (setq set1 pset1))
          (if (null set2) (setq set2 pset2))
          (if (null set3) (setq set3 pset3))
          (if (null set4) (setq set4 pset4))
          (prin1 (+ year n) output)
          (spaces 1 output)
          (prin1 n output)
          (princ `\t` output)
          (prin1 (car set1) output) ; �\��
          (prin1 (car set2) output) ; �\��x
          (spaces 1 output)
          (prin1 (car set3) output) ; �܍s
          (prin1 (car set4) output) ; �Z or ��
          (spaces 1 output)
          (prin1 (eval (car set3)) output) ; �܍s �P
          (prin1 'no output)
          (prin1 (eval (car set4)) output) ; e or to
          (spaces 1 output)
          (print (eval (car set2)) output) ; �\��x �P
          (setq n (add1 n))
          (setq set1 (cdr set1))
          (setq set2 (cdr set2))
          (setq set4 (cdr set4))
; Advance �܍s only after every second step:
          (if (null set4) (setq set3 (cdr set3)))
   ) ; If wrote to �t�@�C��, then close it:
   (if (neq output *stdout*) (close output))
)
 
(defun testi2 (year jikkan1 jikkan2 juunishi1 juunishi2)
  (cond ((null jikkan1)
           (if (null jikkan2) () ; The END
               (testi year (cdr jikkan2) () juunishi2 juunishi2)
           )
        )
        ((null juunishi1)
           (testi year (cddr jikkan1) jikkan2 juunishi2 juunishi2))
        (t
           (prin1 (car jikkan1))
           (prin1 (car juunishi1))
           (princ `\t`)
           (print year)
           (testi (add1 year) jikkan1 jikkan2 (cdr juunishi1) juunishi2)
        )
  )
)

