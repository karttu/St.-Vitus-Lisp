
(defun new-perm (seq1 seq2 &aux tmp)
       (setq seq1 (mapcar #'nth seq2 (setq tmp (mcl seq1))))
       (free-cons tmp)
       seq1
)


(defun memberqnth (elem lista &aux i)
       (setq i 1)
       (while t
              (if (null lista) (return ()))
              (if (equal (car lista) elem) (return i))
              (setq lista (cdr lista))
	      (setq i (add1 i))
       )
)


(defun get1cycle (seq &aux origseq seqs cycle)
       (setq origseq seq)
       (while (not (setq cycle (memberqnth seq seqs)))
              (print seq)
	      (setq seqs (cons seq seqs))
	      (setq seq (new-perm seq origseq))
       )

       (setq *seqs* seqs)
       (terpri)
       (print seq)
       cycle
)



(defun get2cycle (seq &aux seqs cycle)
       (while (not (setq cycle (memberqnth seq seqs)))
              (print seq)
	      (setq seqs (cons seq seqs))
	      (setq seq (new-perm seq seq))
       )
       
       (setq *seqs* seqs)
       (terpri)
       (print seq)
       cycle
)

