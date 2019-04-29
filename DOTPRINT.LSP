

(defun dotprint (expr) (progn () (dotauxprint expr) (terpri) expr))

(defun dotauxprint (expr)
       (cond ((atom expr) (princ expr))
              (t (progn () 
                   (princ "(")
                   (dotauxprint (car expr))
                   (princ " . ")
                   (dotauxprint (cdr expr))
                   (princ ")")))))

