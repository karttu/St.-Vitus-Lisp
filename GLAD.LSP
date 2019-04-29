
/* 
    Some code for "Generic" Lisp Assembler/Disassembler (GLAD)
    Written by Antti Karttunen A.D. 1991
    Now code for both directions (assemble & disassembly)
    And only for PDP-11 code. Loads also file PDP11SET.LSP
    Use function (dis 'progname.sav) to disassemble some PDP-11
    task file.


   Copyright (C) 1991, 1992  Antti J. Karttunen

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License (in file GPL.TXT) for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */



/*

base  width_of_digits   prefix   name
16          4             ^x     hexadecimal
8           3             ^o     octal
4           2             ^q     quaternary
1           1             ^b     binary

*/

/* Maximum number of arguments for vlambda-functions: */
(set-max-args 20)

(setq else t)

(setq RESETREADMODE reset-readmode)

(setq *chunk-size* 1024) ; This must be power of two.
; So that we can get the remainders by masking with this:
(setq *rem-mask* (sub1 *chunk-size*))
(setq *chunk-list* '())


(defun get-byte-from-memory (address)
   (@ (nth (/ address *chunk-size*) *chunk-list*) ($and address *rem-mask*))
)

(defun put-byte-to-memory (address byte &aux chunk-num remainder chunk i)
   (setq remainder ($and address *rem-mask*))
   (setq chunk-num (/ address *chunk-size*))
   (if (setq chunk (nth chunk-num *chunk-list*)) ;If chunk for that addr exists
       (@= chunk byte remainder) ; Then put the byte there.
; Else we must allocate one or more chunks of memory:
       (setq i (add1 (- chunk-num (length *chunk-list*))))
; Allocate (add1 (- chunk-num len) chunks more:
       (while (not (zerop i))
              (setq *chunk-list*
	            (nconc *chunk-list*
                           (list (setq chunk (new-string *chunk-size*)))))
              (fill-with-char chunk `\0` *chunk-size*)
              (setq i (sub1 i))
       )
       (@= chunk byte remainder) ; Then put the byte there.
   )
)


(defun fill-with-char (string char count)
   (while (not (zerop count))
      (setq count (sub1 count))
      (@= string char count)
   )
)



/* Now internal function, coded in ARITHMET.ASM:
(defun sxt (x) (if (greaterp x 0177) ($or x 0xFF00) x))
*/

(defun characterp (x base)
       (not (if (eq base 16) (isxdigitp x) (isdigitp x))))


; Define Read Macro:
(defun drm (c lambda-form)
       (rplacx c *read-macros* lambda-form))


/* Argument should be one character, corresponding symbol preceded with
    a percent sign is interned and returned.
   E.g. (intern-char `A`) return symbol %A
 */
(defun intern-char (x) (intern (@= "%*" x 1)))


/* Check whether x is digit 0-9 (or 0-F if base is 16) and return
    corresponding numeric value if it is, otherwise NIL: */
(defun digitp (x base)
  (cond ((if (eq base 16) (isxdigitp x) (isdigitp x))
          (+ 0 (- (toupper x) (if (greaterp x `9`) `7` `0`))))))


/*
   This returns 1, 2, 3 or 4 depending whether argument is letter b, q, o or x
   i.e. width of digits (in bits) of that number system.
 */
(defun select-width (c)
   (memqnth (tolower c) '(() `b` `q` `o` `x`)))

(setq maxbuf 81)
(setq buffer (new-string (add1 maxbuf)))

(defun compile-bit-pattern (line &aux w)
   (if (or (not (setq w (select-width (@ line 1)))) ;If width not b, q, o or x
           (not (iscontinuousp (+ line 2)))) ; or char after that isn't valid
       (set-lineptr line) ; Then read stuff in with normal way.
; Else compile the bit pattern:
       (read-next-to-string buffer maxbuf) ; First read stuff to buffer.
       (compile-bit-pattern1 (+ buffer 2)
                             (+ buffer (strlen buffer))
                             ($shl 1 w)
                             w
       )
   )
)


;(drm `^` #'(lambda () (compile-bit-pattern (read *curin*))))

(drm `^` compile-bit-pattern)



/* When stuff starting with caret (^) is encountered in read, then rest
    of it is read as symbol and this function is called with that
    symbol as argument.
   (Reading it as string would be less memory-consuming, and maybe that
    space could be free'ed after this ?)
 */
/* Old definition, commented out:
(defun compile-bit-pattern (pattern &aux w)
    (cond ((not (and (nonnilsymbolp pattern)
                     (setq w (select-width (@ pattern)))
                     (not (zerop (@ pattern 1))) ; At least one digit.
                     (setq w (compile-bit-pattern1 (add1 pattern)
                                                  (+ pattern (strlen pattern))
                                                  ($shl 1 w)
                                                  w)
                     )
                )
           )
           (princ "\n**Invalid bit-pattern: " *stderr*)
           (princ pattern *stderr*) (terpri *stderr*)
           (reset))
          (t w)
   )
)
*/


/* This function does the hard work in converting caret-forms: */

(defun compile-bit-pattern1
 (pat-start ptr base width ; Arguments
  &aux
 shift-count saved-shift-count and-mask op-code digit-mask fields c d prev-c)

  (setq saved-shift-count (setq shift-count 0))
  (setq and-mask 00) /* Make them octal */
  (setq op-code  00)
  (setq digit-mask (+ 00 (sub1 base))) /* Force it octal */
; (setq fields ())
  (setq prev-c `0`) /* Must be a digit */
/* Loop pattern from right to left: */
  (cond
    ((not (while (not (lessp (setq ptr (sub1 ptr)) pat-start))
       (setq c (@ ptr)) ; Get the character
       (cond ((neq c prev-c) ; If different from previous one (one to right)
               (if (characterp prev-c base) ; And previous was character
                   (setq fields ; Add a new field.
                     (add-field fields prev-c shift-count saved-shift-count))
               )
               (setq saved-shift-count shift-count)
            )
       )

       (cond ((setq d (digitp c base))
               (if (not (lessp d base)) (return ())) ; Return NIL in error.
               (setq and-mask ($or and-mask digit-mask))
               (setq op-code  ($or op-code  ($shl d shift-count)))
             )
       )

       (setq prev-c c)
       (setq shift-count (+ shift-count width))
       (setq digit-mask  ($shl digit-mask width))
    )) ()) ; Return NIL if while returned NIL.

; If shift-count is more than 18 with base 8, or more than 16 with other bases
;  then return NIL, because there is too much digits in pattern:
    ((if (greaterp shift-count 16)
; If shift count is 18 in octal base, then make it 16, and return NIL for
;  condition-test, so this clause fails, and next clause is executed:
       (not (if (and (eq base 8) (eq shift-count 18)) (setq shift-count 16))))
        ()) ; Otherwise return NIL.

    (t ; If everything OK, then continue from this.

      (if (characterp prev-c base) ; Still one field...
          (setq fields
                (add-field fields prev-c shift-count saved-shift-count))
      )

; If there is fields, then return (and-mask op-code [field ...]) as result.
; Or the remaining digit-positions to and-mask.
      (if fields
          (nconc
           (clist ($or and-mask (neg ($shl 01 shift-count))) op-code) fields)
          op-code ; otherwise return just the op-code.
      )
    )
  )
)

; Note that (2 power width-in-bits) minus 1 is always corresponding bit-mask.
; I.e. (sub1 ($shl 01 width-in-bits))

; Add a new field: (NAME DISPL MASK) to the fields
(defun add-field (fields c shift-count saved-shift-count)
       (cons (clist (intern-char c) ; One character name of the field.
                     saved-shift-count ; displacement from bit-0
                     (sub1 ($shl 01 (- shift-count saved-shift-count)))) ;mask
              fields)
)

/* Is x symbol whose first character is a percent sign?
   (That is, is it a pattern variable ?)
 */
(defun patvarp (x)
       (and (nonnilsymbolp x) (eq (@ x) `%`)))


/* def-a: Define assembly-time behaviour for pattern */
(defun def-a nlambda (args)
   (pick-subst () (cadr args))
   (def-a-aux (car args) (cadr args))
   (cadr args)
)

/* def-d: Define disassembly-time behaviour for pattern */
(defun def-d nlambda (args)
   (pick-subst () (cadr args))
   (def-d-aux (car args) (cadr args))
   (cadr args)
)

/* def-a-et-d: Define both */
(defun def-a-et-d nlambda (args)
   (pick-subst () (cadr args))
   (def-a-aux (car args) (cadr args))
   (def-d-aux (car args) (cadr args))
   (cadr args)
)

(defun def-a-aux (pat form)
   (cond ((atom pat) (set pat form))
          (t (mapc #'def-a-aux pat (mcl form)))))

(defun def-d-aux (pat form)
   (cond ((atom pat) (setplist pat form))
          (t (mapc #'def-d-aux pat (mcl form)))))

(defun call-a (pat arg)
       (if (eq (car (setq pat (symeval pat))) 'patterns)
           (assemble-code arg (cdr pat))
           (funcall pat arg) ; otherwise call the lambda form.
       )
)

(defun call-d (pat arg)
       (if (eq (car (setq pat (plist pat))) 'patterns)
           (disasm-code arg (cdr pat) ())
           (funcall pat arg) ; otherwise call the lambda form.
       )
)



/*
    These are useless now:

(defun find-bit-pattern (x patterns &aux a b)
   (while t
      (if (null patterns) (return ()))
      (if (intp (setq b (car (setq a (car patterns)))))
;If car of pattern is int and it's x then return that
          (if (eq x b) (return a))
; Else if x&and-mask == op-code, then assign bits and return that:
          (if (eq ($and (car b) x) (cadr b)) (return (bind-bits a x)))
      )
      (setq patterns (cdr patterns))
   )
)


(defun bind-bits (pattern bits)
       (bind-bits1 (cddr (car pattern)) bits ())
       pattern
)

(defun bind-bits1 (fields bits f)
   (cond ((null fields) ())
          (t (setq f (car fields))
             (set (car f) ($and ($shr bits (cadr f)) (car (cddr f))))
             (bind-bits1 (cdr fields) bits ())
          )
   )
)

*/


(defun disasm-code (x patterns tab-flag &aux a b schweinflag)
;  (setq schweinflag ()) ; Ruma koodi iskee! (Remove this later!)
   (while t
      (if (null patterns) (return ()))
	      
      (if (intp (setq b (car (setq a (car patterns)))))
;If car of pattern is int and it's x then output that: (and break from loop)
          (if (eq x b) (return (output-formlist (cdr a) b () ())))
; Else if x&and-mask == op-code, then output corresponding opcode with
;  corresponding arguments: (and break from loop)
          (if (eq ($and (car b) x) (cadr b))
             (return (output-formlist (cdr a) x (cddr b) tab-flag)))
      )
      (setq patterns (cdr patterns))
   )
   () ; Return NIL
)


/*
          (if (or (null tab-flag) schweinflag) (return ())
	      (setq schweinflag t)
	      (setq patterns *WC*)
	  )
      )
*/

(defun output-form (form x bit-fields)
  (cond ; ((null form) ())
        ((patvarp form)
; Print result only if it's non nil:
            (if (setq form (call-d form (get-bits x form bit-fields)))
                (prin1 form *d-output*)
            )
        )
        ((atom form) (prin1 form *d-output*))
        (t ; Else form is list, so print it recursively inside parentheses:
           (princ `(` *d-output*)
           (output-formlist form x bit-fields ())
           (princ `)` *d-output*)
        )
  )
)

(defun output-formlist (form x bit-fields tab-flag)
; If tab-flag is on, then print one tab between first form (should be
; instruction mnemonic), and rest of the formlist: (quite ugly kludge...)
   (cond (tab-flag
           (output-form (car form) x bit-fields)
           (princ `\t` *d-output*)
           (setq form (cdr form))))
   (mapc #'output-form form
         (setq x          (mcl x))
         (setq bit-fields (mcl bit-fields))
   )
   (free-cons x) ; Free those circular lists which
   (free-cons bit-fields) ; were given to mapc as arguments.
   () ; What the hell this should return ?
)


; Return bits from x corresponding to pattern. If pattern is not found
;  from bit-field list f, then return nil:
(defun get-bits (x pattern f)
   (cond ((null f) ())
          ((eq (caar f) pattern)
             (setq f (car f))
             ($and ($shr x (cadr f)) (car (cddr f))))
           (t (get-bits x pattern (cdr f)))))


/* This reads a word (16-bit) from the binary input stream (port)
   Returns NIL on EOF
 */
(defun getw (port &aux lo hi)
   (if (setq lo (tyi port ())) ; Read low byte, and check that it's not EOF
      (+ 00 ; Force result of following if to be octal:
        (if (setq hi (tyi port ())) ; Read high byte and check it's not EOF.
             (+ ($shl hi 8) lo) ; If both bytes OK, then make word from them.
             lo ; If high byte was EOF, then return only low byte, and EOF
        )       ;  next time (at least I hope so).
      )
   )
)

(defun putw (word port)
   (tyo ($and word 0xff) port)
   (tyo ($shr word 8) port)
   word
)


(defun write-memory-out (port &aux i)
   (setq i 0)
   (while (lessp i *LC*)
      (tyo (get-byte-from-memory i) port)
      (setq i (add1 i))
   )
   (close port)
)

; This puts word given as argument to memory location pointed by *OLD-LC*
;  and increments *OLD-LC* after that to point to next word:
;
(defun put-word-to-memory (word)
   (put-byte-to-memory *OLD-LC* ($and word 0xff))
   (put-byte-to-memory (add1 *OLD-LC*) ($shr word 8))
   (setq *OLD-LC* (+ *OLD-LC* 2))
   word
)

(setq *LC* 00) ; Location counter (printed in octal)

(defun get-LC () *LC*)
(setq *OUTQUEUE* ())

(defun input-word  ()  (setq *LC* (+ *LC* 2)) (getw *d-input*))
(defun output-word (w) (putw w *a-output*))
(setq *a-output* *stdout*)
(setq *d-output* *stdout*)


(defun outqueue (w)
   (setq *LC* (+ *LC* 2))
   (setq *OUTQUEUE* (cons w *OUTQUEUE*))
)

/*
   Currently the main disassembly routine.
   Should be called as (dis 'filename.ext [start])
   If optional start address is specified, then so many bytes are skipped
   from the beginning of file.
 */

(setq *NO-NUMBERS* ())

(defun set-octal-output () (rplacx 2 *integer-printtypes* "%06o"))

(set-octal-output)

(defun dis (file &opt start &aux word loc)
; Set printing precision of octal numbers to six digits with leading zeros:
   (set-octal-output)
   (setq *d-input* (infile file))
   (setq *LC* 00)
   (if (intp start) ; If start address specified, then skip start bytes:
        (while (lessp (get-LC) start)
                (if (input-word) ()
                     (princ "**Premature EOF at: ")
                     (print (get-LC))
                     (reset)
                )
        )
   )

   (while t
     (setq loc (get-LC))
     (if (not (setq word (input-word))) (return)) /* If EOF then break */
     (cond ((not *NO-NUMBERS*)
                (prin1 loc  *d-output*)
                (princ `\t` *d-output*)
                (prin1 word *d-output*)
                (princ `\t` *d-output*)
           )
     )
     (disasm-code word (cxr ($shr word *shiftcount*) *disasm-table*) t)
     (terpri *d-output*)
   )
   'EOF
)

; =======================================================================
;   Assembly stuff:

(setq *debug* ())

(setq *inbufsize* 1030)

(setq *inbuf* (new-string (add1 *inbufsize*)))

(defun asm (in out &aux datum linecount errors)
   (if (not (and (*stringp in) (*stringp out)))
       (ertzu "\nusage: " "(asm input-file output-file)")
   )
   (setq  a-input   (infile in))
   (setq *a-output* (outfile out))
   (set-asm-mode)
   (setq *LC* 00)
   (setq errors 0)
   (setq linecount 0)
   (while (readtostring *inbuf* *inbufsize* a-input)
      (setq datum (readstring *inbuf*))
      (setq linecount (add1 linecount))
      (setq *OLD-LC* *LC*)
      (cond (*debug*
               (princ "\n**Line: ") (princ linecount)
               (princ " location: ") (print *OLD-LC*)
	       (princ datum)
	       (terpri)
            )
      )
      (cond
        ((label-definition-p datum) (equate-label datum))
	(else
          (setq *LC* (+ *LC* 2))
          (setq *OUTQUEUE* ())
          (cond ((not (intp (setq code
                              (assemble-code datum
			                     (if (boundp (car datum))
					         (symeval (car datum)))))))
             
               (princ "**Line: ") (princ linecount)
               (princ " location: ") (print *OLD-LC*)
	       (princ datum) (princ " no match.\n")
               (if code (print code))
               (setq errors (add1 errors))
            )
	    (else
	       (put-word-to-memory code)
               (mapc #'put-word-to-memory (nreverse *OUTQUEUE*))
	    ) ; else
          ) ; cond
        ) ; else
      ) ; cond
   ) ; while
   (reset-readmode)
   (princ "\n\n**Compiled ") (princ linecount) (princ " lines.\n")
   (princ "Produced ") (princ (get-LC)) (princ " bytes. Errors: ")
   (print errors)
)

; If second element is colon or equal sign?
(defun label-definition-p (datum) (memq (cadr datum) '(: =)))

(defun equate-label (datum &aux label value)
   (setq label (car datum))
   (cond ((eq (cadr datum) ':) (setq value *LC*))
         (else (setq value (eval (nth 2 datum)))) ; Otherwise it's =
   )
   (set label value)
;  (resolve-unresolved label value)
)


(defun assemble-code
      (datum patterns &aux code pat numfields newcode latest-error)
;   (setq latest-error (setq newcode ())) ; Remove this later
   (while t
      (if (null patterns) (return latest-error))

      (setq pat (car patterns))
      (setq numfields (car pat))
      (if (intp numfields)
          (setq code numfields) ;Then
          (setq code (cadr numfields)) ;Else
	  (setq numfields (cddr numfields))
      )
      (if (intp (setq newcode (matchp (cdr pat) datum code numfields)))
	  (return newcode)
	  (if newcode (setq latest-error newcode))
      )

      (setq patterns (cdr patterns))
   )
)

 /* Old code:
      (cond ((setq bindings (matchp (setq pat (cdar patterns)) datum ()))
               (if (intp (setq numfields (caar patterns))) (return numfields))
               (setq code (cadr numfields))
	       (setq numfields (cddr numfields))
	       (print (list code numfields bindings))
	       (while numfields
	         (setq patvar (caar numfields))
	         (setq code (+ code ($shl
                   (print (call-a patvar (get bindings patvar)))
		   (cadr (car numfields))))
		 )
	         (setq numfields (cdr numfields))
	       )
               (return code)
	    )
      )
 */


(defun matchp (patterns datums code numfields &aux pat item)
   (while t
     (setq pat  (car patterns))
     (setq item (car datums))
     (cond ((and (null patterns) (null datums)) (return code))
           ((or  (null patterns) (null datums)) (return ()))
           ((patvarp pat) ; If pattern variable.
             (if (pluralp pat) ; Allow matching to more than one element?
              (cond ((null (cdr patterns)) ;This was last one in pattern list?
  	               (return (getnewcode pat datums code numfields))
		    )
                    ((patvarp (cadr patterns)) ;Next one is patvar too?
  	               (setq code (getnewcode pat (list item) code numfields))
                    )
		    (t ; Else the next one is ordinary atom or list:
		      (if (setq datums (get-sub-part (cadr patterns) datums))
  	                  (setq code
			        (getnewcode pat (car datums) code numfields)
			  )
                          (return ()) ; If not found then nil.
		      )
                    )
	      )
	      ; Else, try to match to only one singular element in datums:
	      ; Don't try to match something like @123456(R0) to @ %Z
	      (if (and (null (cdr patterns)) (cdr datums)) (return ()))
	      (setq code (getnewcode pat item code numfields))
             )
	   )
	   ((atom pat) ; Ordinary atom.
	     (if (neq pat item) (return ())) ;If it isn't eq to item, then nil.
	   )
	   (t ; Else pat is list.
	      (if (or (atom item) ;item must be list too in that case.
	              (and (null (cdr patterns)) (cdr datums))
                  ) ; And don't try to match something like (R0)+ to (%R)
	          (return ())
	      )
              (setq code (matchp pat item code numfields))
	   )
     )
     (if (not (intp code)) (return code))
     (setq datums (cdr datums))
     (setq patterns (cdr patterns))
   )
)


 
(defun getnewcode (patvar num code numfields)
  (cond ((listp (setq num (call-a patvar num))) num)
        ((or (intp numfields) (eq num 't)) code)
        ((setq numfield (assq patvar numfields))
           (if (greaterp num (car (cddr numfield)))
	       (list 'getnewcode: 'num 'too 'big (list num)
	             'numfield: numfield
	       )
	       (+ code ($shl num (cadr numfield)))
           )
	)
	((list 'getnewcode: 'internal 'error: patvar 'not 'found
	      'from 'numfields 'list numfields)
        )
  )
)


(defun get-sub-part (delimiter lista &aux result)
;   (setq result ()) ;Remove this later (when interpreter is ready!).
   (if (consp 
        (while lista
          (if (or (and (listp delimiter) (listp (car lista)))
                  (eq (car lista) delimiter)
	      )
              (return ;Then
	       (setq result
	        (cons (nreverse result) lista)
	       )
	      )
	      (setq result (cons (car lista) result)) ;Else
	      (setq lista (cdr lista))
	  )
	)
       )
       result ; If while loop return cons cell, then return it.
; Else, if it didn't find delimiter, then free the constructed result list:
       (free-list result)
       () ; And return nil.
   )
)

/* Alternative code: Commented out.
              (return ;Then
	       (setq result ; If result is just one elem. long then take car:
	        (cons (if (cdr result) (nreverse result) (car result)) lista)
	       )
	      )
 */

; From page 335 of LISPcraft, by Robert Wilensky:
(defun assq (exp assoclist)
   (car (member exp assoclist :TEST #'(lambda (x y) (eq x (car y)))))
)

;
;
; Set read mode suitable for reading assembler code.
;
(defun set-asm-mode (&aux i)
   (setq ibase 8) ; Input in octal.
   (setq i 0) ; First set charflags so that only digits, letters, $, . and _
   (while (lessp i 256) ; are continuous, others not.
      (@= *charflags* (if (isalnump i) 1 0) i)
      (setq i (add1 i))
   )
   (@= *charflags* 1 `$`)
   (@= *charflags* 1 `.`)
   (@= *charflags* 1 `_`)
; Then modify the read macro system:
; Convert all symbols to uppercase:
   (rplacx  0   *read-macros* *uppersymbol-rm*)
;  (rplacx `+`  *read-macros* ())
;  (rplacx `-`  *read-macros* ())
   (rplacx `\`` *read-macros* ()) ; Backquote, no special signifigance.
   (rplacx `'`  *read-macros* *charquote-rm*) ; Singlequote used for chars.
   (rplacx `#`  *read-macros* ()) ; No function quote.
   (rplacx `[`  *read-macros* *listbegin-rm*) ; Now brackets
   (rplacx `{`  *read-macros* *listbegin-rm*) ;  and braces
   (rplacx `]`  *read-macros* *listend-rm*)   ;   are also
   (rplacx `}`  *read-macros* *listend-rm*)   ;    list delimiters.

   (@= *dtpr-flag* 0) ; Don't handle dots in any special way.
;  (@= *esc-flag* 0)  ; Leave the ESC-sequences of the strings intact
   (@= *nil-flag* 0)  ; Symbol nil has no special signifigance.
   (@= *quote-char* `'`) ; Quote char for characters on output is also '
   (@= *speclist-flag* 1) ; Print also bracket and brace-lists in special way.
)


(defun readstring (line &aux item result)
;  (setq result ()) ; Remove this later!
   (while (neq (setq item (read line '*EOF*)) '*EOF*)
      (setq result (cons item result))
   )
   (nreverse result)
)


(defun ertzu (arg1 arg2)
       (princ arg1 *stderr*)
       (princ arg2 *stderr*)
       (terpri *stderr*)
       (reset)
)

(defun rplast (lista x) (rplaca (last lista) x) lista)

(defun optimize-set (instr-set &aux opcodename)
   (while instr-set
      (setq car-of-set (car instr-set))
      (setq opcodename (cadr car-of-set))
      (set  opcodename ; Collect list to opcodename
;consp should return its argument if it's cons cell:
            (nconc (and (boundp opcodename) (consp (symeval opcodename)))
                   (list car-of-set)
            )
      )
      (add-to-table car-of-set)
      (setq instr-set (cdr instr-set))
   )
)

(setq *bitwidth* 10) ; Index with first ten bits.

(setq *shiftcount* (- 16 *bitwidth*))

(setq *tablesize* ($shl 1 *bitwidth*))

(setq *disasm-table* (new-clist *tablesize*))

(defun add-to-item (index table item)
   (rplacx index table (nconc (cxr index table) (list item)))
)


(defun add-to-table (item &aux index indmask)
   (setq numfields (car item))
   (if (intp numfields)
       (add-to-item ($shr numfields *shiftcount*) *disasm-table* item) ;Then
;Else:
       (setq mask ($shr (car  numfields) *shiftcount*))
       (setq base ($shr (cadr numfields) *shiftcount*))
; This version should work if instructions are in the ascending order in set:
       (setq index base)
       (while (and (eq ($and mask index) base) (lessp index *tablesize*))
	  (add-to-item index *disasm-table* item)
	  (setq index (add1 index))
       )
   )
)

/* This old code browses all the items all the time. Much too slow:
       (setq index 0)
       (while (lessp index *tablesize*)
          (if (eq ($and mask index) base)
	      (add-to-item index *disasm-table* item)
          )
	  (setq index (add1 index))
       )
 */

 
(load 'pdp11set) ; Load the pdp11 instruction set in.

(optimize-set s11)

