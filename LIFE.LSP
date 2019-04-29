;
; Coded by Antti Karttunen once upon a time.
; This code is Public Domain.
;
; This code is written in quite hairy way, so that current generation
; is just huge list of lists forming grid of life. And that list
; is given as argument to function *!* which computes the new generation
; of grid from it. Because there is no garbage collection in St. Vitus Lisp,
; and those previous generations are not free'ed, the memory is exhausted
; very soon after some generations.
; It's good to set environment variable MAXMEM as big as possible before
; starting lisp. Something like 300000 - 450000 or more if possible.
; Note that function live-or-dead which decides whether cell will be live
; or dead in next generation doesn't use numerical arithmetic at all.
; Of course this is probably the slowest possible way to code life,
; even with lisp! So if you want quicker one then use rplaca and other
; destructive functions to directly change cells from grid. Or write
; and read directly from the video memory. Or code in assembly language.
;
; Start demo with (start) after you have loaded this to St. Vitus Lisp
; with (load 'life)   or with  lisp -llife
;
 
 
 
(defun caddr (x) (car (cddr x)))

(setq oldtime 0)
(setq newtime 0)

(defun time ()
       (setq oldtime newtime)
       (setq newtime (seconds-of-day))
       (print (daytime))
       (princ "newtime - oldtime: ")
       (- newtime oldtime))

/*
(defun time ()
       '(time not implemented))
 */



(defun fold (matrix)
       (fold-up-and-down (fold-sides matrix)))

/*
(defun fold-sides (matrix)
       (cond ((null matrix) ())
              (t (cons
                       (append (last (car matrix))
                         (append (car matrix) (list (caar matrix))))
                        (fold-sides (cdr matrix))))))
 */
(defun fold-sides (matrix)
       (mapcar #'(lambda (x)
                  (append (last x) (append x (list (car x)))))
                matrix))


(defun fold-up-and-down (matrix)
       (append (last matrix) (append matrix (list (car matrix)))))
/*     (append (last matrix)
                matrix
                 (list (car matrix))))
 */


(defun member-if-not-nil (lista)
       (memq '* lista))

/*
(defun member-if-not-nil (lista)
       (member () lista :test '(lambda (hink honk) honk)))
 */


;(defun live-or-dead (cell list-of-neighbors)
;       (or                                                              
;           (and (not (member-if-not-nil (cdr                            
;                      (member-if-not-nil (cdr                           
;                       (member-if-not-nil (cdr                          
;                        (member-if-not-nil list-of-neighbors))))))))    
;                 (car (member-if-not-nil (cdr                           
;                       (member-if-not-nil (cdr                          
;                        (member-if-not-nil list-of-neighbors)))))))
;            (and cell                                                   
;                 (and (not (member-if-not-nil (cdr                      
;                            (member-if-not-nil (cdr                     
;                             (member-if-not-nil list-of-neighbors)))))) 
;                       (car (member-if-not-nil (cdr                     
;                             (member-if-not-nil list-of-neighbors))))))))


(defun live-or-dead (cell list-of-neighbors)
       (funcall (function (lambda (cell twice-membered)
        (funcall (function (lambda (cell twice-membered thrice-membered)
		  (or (and (not (member-if-not-nil (cdr thrice-membered)))
		            (car thrice-membered))
		        (and cell
		     	     (and (not thrice-membered)
			  	   (car twice-membered))))))
		  cell twice-membered (member-if-not-nil
		  		       (cdr twice-membered)))))
		 cell (member-if-not-nil (cdr
		       (member-if-not-nil list-of-neighbors)))))


(defun live-or-dead-old (cell list-of-neighbors)
       (or                                                              
           (and (not (member-if-not-nil (cdr                            
                      (member-if-not-nil (cdr                           
                       (member-if-not-nil (cdr                          
                        (member-if-not-nil list-of-neighbors))))))))
                 (car (member-if-not-nil (cdr                           
                       (member-if-not-nil (cdr                          
                        (member-if-not-nil list-of-neighbors)))))))
            (and cell                                                   
                 (and (not (member-if-not-nil (cdr                      
                            (member-if-not-nil (cdr                     
                             (member-if-not-nil list-of-neighbors)))))) 
                       (car (member-if-not-nil (cdr                     
                             (member-if-not-nil list-of-neighbors))))))))


(defun *&* (upper-row current-row under-row)
       (cond
             ((or (null (cddr upper-row))
                   (null (cddr current-row))
                    (null (cddr under-row))) ())
              (t (cons
                       (live-or-dead (cadr current-row)   ;The cell itself.
                                      (list   ;Make list from neighbor-cells
                                            (car   upper-row)
                                            (cadr  upper-row)
                                            (caddr upper-row)
                                            (car   current-row)
                                            (caddr current-row)
                                            (car   under-row)
                                            (cadr  under-row)
                                            (caddr under-row)))
                          (*&* (cdr upper-row)
                                (cdr current-row)
                                 (cdr under-row))))))


/*
(defun *!* (matrix)
       (cond ((null (cddr matrix)) ())
              (t (cons (*&* (car matrix) (cadr matrix) (caddr matrix))
                        (*!* (cdr matrix))))))
 */

(defun *!* (matrix)
       (setq tulos ())
       (while (cddr matrix)
         (setq tulos
          (cons (*&* (car matrix) (cadr matrix) (caddr matrix)) tulos))
         (setq matrix (cdr matrix)))
       (nreverse tulos))



/* Jotain t{m{n tapaista:
(defun *!* (matrix)
       (maplist #'(lambda (x) (*&* (car x) (cadr x) (caddr x)))
                matrix))
              
 */


 (defun princ!list (list)
        (cond
              ((null list) (terpri))
               (t (cond ((car list) (princ (car list)))
                         (t (princ ".")))
                   (princ!list (cdr list)))))


 (defun princ!matrix (matrix)
        (cond
              ((null matrix) ())
               (t (princ!list (car matrix))
                   (princ!matrix (cdr matrix)))))



(defun princ-world (matrix generation)
       (print (list 'generation generation))
	 (print (time))
	  (terpri)
           (princ!matrix matrix)
            (terpri)
             (terpri)
               matrix)


(defun start () (life collission 0))

(defun life (matrix generation)
       (while t
         (setq matrix (*!* (fold (princ-world matrix generation))))
         (setq generation (add1 generation))))

/*
(defun life (matrix generation)
       (do ((generation 0 (+ 1 generation))
       	    (world matrix (*!* (fold (princ-world world generation)))))
	   (NIL)))
 */

(setq d '(fold fold-up-and-down fold-sides member-if-not-nil live-or-dead
	  *&* *!* princ!list princ!matrix princ-world life))

;
; Two gliders colliding to each other, and forming new glider going to
;  NE direction.
;
 (setq collission
;;  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
 '((() () () () () () () () () () () () () () ())   ;1
   (() () () () () () () () () () () () () () ())   ;2
   (() () () () () () () () () () () () () () ())   ;3
   (() () () () () () () () () () () () () () ())   ;4
   (() () () () () () () () () () () () () () ())   ;5
   (() () () () () () () () () () () () () () ())   ;6
   (() () () () () () () () () () *  () () () ())   ;7
   (() () () () () () () () () () *  () *  () ())   ;8
   (() () () *  *  *  () () () () *  *  () () ())   ;9
   (() () () () () *  () () () () () () () () ())   ;10
   (() () () () *  () () () () () () () () () ())   ;11
   (() () () () () () () () () () () () () () ())   ;12
   (() () () () () () () () () () () () () () ())   ;13
   (() () () () () () () () () () () () () () ())   ;14
   (() () () () () () () () () () () () () () ()))) ;15


