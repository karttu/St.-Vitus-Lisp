;
;
; This function prints the length of every hash-bucket in *obtable*
;
(defun thc (&opt filename)  ; Test Hash Calistenics
   (thc-aux *obtable* (if (endmarkp filename) filename (outfile filename)))
)

(defun thc-aux (obt output &aux kala liha)
   (cond ((null obt) 0)
          (t (setq kala (thc-aux (cdr obt) output))
             (if (zerop (setq liha (length (car obt)))) (print 0 output)
/* Else */       (print (- liha kala) output) (setq kala liha)
             )
             kala
          )
   )
)
 
(defun kala (&aux veba)
       (setq veba (delete-if #'zerop (mapcar #'length *obtable*)))
       (maprplaca #'- veba (cdr veba))
)


(defun thc2 (&aux i template result)
   (rplacx 15 *integer-printtypes* "G%03u")
   (setq i 1000)
   (setq template "XXXX")
   (setq result ())
   (while (not (zerop i))
          (setq i (sub1 i))
          (princ (setsubtype i 15) template)
          (setq result (cons (intern template) result))
   )
   result
)
 

(defun thc3 (&aux i template result)
   (rplacx 15 *integer-printtypes* "%04u")
   (setq i 10000)
   (setq template "XXXX")
   (setq result ())
   (while (not (zerop i))
          (setq i (sub1 i))
          (princ (setsubtype i 15) template)
          (setq result (cons (intern template) result))
   )
   result
)
 
