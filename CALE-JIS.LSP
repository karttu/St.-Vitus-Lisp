;
; This piece of code written at 10-11. AUG 1991
; by Antti Karttunen 「アンッティ　カルットゥネン」
; Routines presented here are Public Domain.
;
; Use (start1 1923) and (start1 1983) to produce the Chinese-Japanese
; calendar output with JIS-kanji-characters.
;
 
(setq eto '(兄 弟)) ; (e=elder) and (to=younger).
 
(setq eto_kun '(e to))
  
(setq gogyoo '(木 火 土 金 水)) ; 五行
 
(setq gogyoo_kun '(ki hi tsuchi ka mizu)) ; Kun-readings of gogyoo
 
(setq jikkan '(甲 乙 丙 丁 戊 己 庚 辛 壬 癸)) ; 十干
 
(setq juunishi '(子 丑 寅 卯 辰 巳 午 未 申 酉 戌 亥)) ; 十二支
 
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
; Stop when 十干 and 十二支 become nil at the SAME time:
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
          (prin1 (car set1) output) ; 十干
          (prin1 (car set2) output) ; 十二支
          (spaces 1 output)
          (prin1 (car set3) output) ; 五行
          (prin1 (car set4) output) ; 兄 or 弟
          (spaces 1 output)
          (prin1 (eval (car set3)) output) ; 五行 訓
          (prin1 'no output)
          (prin1 (eval (car set4)) output) ; e or to
          (spaces 1 output)
          (print (eval (car set2)) output) ; 十二支 訓
          (setq n (add1 n))
          (setq set1 (cdr set1))
          (setq set2 (cdr set2))
          (setq set4 (cdr set4))
; Advance 五行 only after every second step:
          (if (null set4) (setq set3 (cdr set3)))
   ) ; If wrote to ファイル, then close it:
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

