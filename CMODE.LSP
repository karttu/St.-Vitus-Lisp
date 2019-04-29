;
; This demonstrates the flexibility of the parsing system.
; Here it is modified so that it's suitable for reading
; C-programs in. But this needs some more development.
;
; Usage: (printprog (readprog "kalaa.c"))
;

(setq *SEMICOLON*   (intern ";"))
(setq *LEFTBRACE*   ($shl `{` 8))
(setq *LEFTBRACKET* ($shl `[` 8))

(setq *C-KEYWORDS* ; Some C-keywords
  '(struct union enum void typedef sizeof if else switch case default
    while do for break continue goto return include define))

(setq *STDFUNS* ; Some standard functions & macros
 '(printf fprintf sprintf scanf fscanf sscanf getchar putchar gets puts
   fgets fputs fopen fclose fgetc fputc strcpy strncpy strcmp strncmp
   strcat strncat strlen malloc calloc free system exit feof ferror fflush
   isalpha isupper islower isdigit isxdigit isalnum isspace ispunct
   iscntrl isprint isgraph isascii toascii tolower toupper _tolower _toupper))


/* Set plists of all symbols in lista to be y: */
(defun set-plists (lista y)
   (member y lista /* :TEST */
; Return always nil, so lista is handled to the bitter end:
           #'(lambda (a b)
; Set plist of element only if it's symbol which is unbound or nil:
               (if (nonnilsymbolp b)
                   (setplist b a))
              ())
   )
)

(set-plists *C-KEYWORDS* '*IGNORE*)
(set-plists *STDFUNS*    '*IGNORE*)

/* If item is not already in lista, then add it to it: */
(defun remember (item lista)
   (cond ((memq item lista) lista)
          ((cons item lista))))


(defun create-info (fun type module)
   (cond ((plist fun)
            (if type   (set-type   fun type))
            (if module (set-module fun module))
          )
         (t (setplist fun (clist type () () module)))
   )
)

(defun get-type     (x) (car (plist x)))
(defun get-parents  (x) (cadr (plist x)))
(defun get-children (x) (nth 2 (plist x)))
(defun get-module   (x) (nth 3 (plist x)))

(defun set-type (fun type) (rplaca (plist fun) type))

(defun set-module (fun module) (rplacx 3 (plist fun) module))

(defun add-parent (fun parent)
       (rplacx 1 (plist fun) (remember parent (cxr 1 (plist fun))))
)

(defun add-child (fun child)
       (rplacx 2 (plist fun) (remember child (cxr 2 (plist fun))))
)

(setq buffer (new-string 1326))

(setq call-listbegin (list 'lambda 1 *listbegin-rm*))

(defun *numsign-rm* (line)
   ; If # not on the first column, then read it with normal way:
   (if (neq line *linebuf*) (set-lineptr line)
; Else if # is on the first column:
       (strcpy buffer line) ; Save the line
       (@= line `(`)        ; Put beginning parenthesis as first char.
       (strcpy (add1 line) buffer) ; And copy orig line after that.
       (strcat line ")")    ; Plus ending parenthesis.
       (call-listbegin line) ; And read it as list.
   )
)

(defun *doubletoken1-rm* (line &aux savec result)
   (cond ((or (eq (@ line) (@ line 1)) (eq (@ line 1) `=`))
            (setq line (+ line 2))
            (setq savec (@ line))
            (@= line `\0`)
            (setq result (intern (- line 2)))
            (@= line savec)
            (set-lineptr line)
            result
         )
         (set-lineptr line)
   )
)

;
;
; Set read mode good for reading C-programs.
;
(defun setCmode (&aux i)
   (setq i 0) ; First set charflags so that only digits, letters and _
   (while (lessp i 256) ; are continuous, others not.
      (@= *charflags* (if (isalnump i) 1 0) i)
      (setq i (add1 i))
   )
   (@= *charflags* 1 `_`)
; Then modify read macro system:
   (rplacx `+`  *read-macros* ())
   (rplacx `-`  *read-macros* ())
;   (rplacx `+`  *read-macros* *doubletoken1-rm*)
;   (rplacx `-`  *read-macros* *doubletoken1-rm*)
;   (rplacx `=`  *read-macros* *doubletoken1-rm*)
;   (rplacx `|`  *read-macros* *doubletoken1-rm*)
;   (rplacx `<`  *read-macros* *doubletoken1-rm*)
;   (rplacx `>`  *read-macros* *doubletoken1-rm*)
;   (rplacx `!`  *read-macros* *doubletoken1-rm*)
   (rplacx `\`` *read-macros* ()) ; Backquote, no special signifigance.
   (rplacx `'`  *read-macros* *charquote-rm*) ; Singlequote used for chars.
   (rplacx `;`  *read-macros* ()) ; Semicolons are no comments anymore.
   (rplacx `#`  *read-macros* *numsign-rm*)   ; Handle #define's
   (rplacx `[`  *read-macros* *listbegin-rm*) ; Now brackets
   (rplacx `{`  *read-macros* *listbegin-rm*) ;  and braces
   (rplacx `]`  *read-macros* *listend-rm*)   ;   are also
   (rplacx `}`  *read-macros* *listend-rm*)   ;    list delimiters.

   (@= *dtpr-flag* 0) ; Don't handle dots in any special way.
   (@= *esc-flag* 0)  ; Leave the ESC-sequences of the strings intact
   (@= *nil-flag* 0)  ; Symbol nil has no special signifigance.
   (@= *quote-char* `'`) ; Quote char for characters on output is also '
   (@= *speclist-flag* 1) ; Print also bracket and brace-lists in special way.
)

(defun readprog (file &aux expr lista)
   (setCmode)
   (setq lista ()) ; Not needed later
   (setq file (infile file))
   (while (neq (setq expr (read file '*EOF*)) '*EOF*)
      (setq lista (cons expr lista)))
   (reset-readmode)
   (@= *quote-char* `'`) ; Quote char for characters on output is also '
   (@= *speclist-flag* 1) ; Print also bracket and brace-lists in special way.
   (nreverse lista)
)

(defun printprog (lista file)
   (mapc #'(lambda (x f) ; If x is list starting with # then special case:
             (if (and (consp x) (eq (car x) '#)) (print-preproc x f)
                 (print x f) ; Otherwise just print it
             )
           )
         lista (mcl (setq file (if (endmarkp file) *stdout* (outfile file))))
   )
   (if (neq file *stdout*) (close file))
)

(defun print-preproc (lista port)
   (mapc #'(lambda (x f) (prin1 x f) (spaces 1 f))
         lista (mcl port)
   )
   (terpri port)
)

(defun leftbracep (x)
   (and (eq (subtype x) 3)
        (or (eq (setq x ($shr x 8)) `{`) (eq x `[`)))
)

(defun wordp (x)
   (and (nonnilsymbolp x) (isalnump (@ x))))

(defun arglistp (x)
   (and (listp x)
        (neq (setq x (car x)) *LEFTBRACE*)
        (neq x *LEFTBRACKET*)
        (neq x '#)
   )
)


(defun word-or-blockp (x)
   (or (wordp x) (and (listp x) (eq (car x) *LEFTBRACE*)))
)

(defun find-next-block (lista)
   (member () lista :TEST
      #'(lambda (x y) (and (listp y) (eq (car y) *LEFTBRACE*)))
   )
)


(defun browse-toplevel (lista module &aux fun-name)
   (while lista
     (if (and (wordp (car lista))
              (cddr lista) ; It's at least three elems long still.
              (arglistp (cadr lista))
              (word-or-blockp (car (cddr lista)))
         )
         (progn
           (setq fun-name (car lista))
           (set  module (cons fun-name (symeval module)))
           (create-info fun-name '*FUNCTION* module)
/*
           (terpri)
           (prin1 fun-name)
           (print (cadr lista))
 */
           (browse-funbody (car (setq lista (find-next-block lista)))
                           fun-name
                           module
           )
         )
     )
     (setq lista (cdr lista))
   )
)



(defun browse-funbody (lista fun-name module)
   (while lista
/*
      (if (and (cddr lista) ; This is at least 3 elements long.
               (wordp (car lista))
               (wordp (cadr lista))
               (arglistp (car (cddr lista)))
          )
          (progn
             (spaces 1)
             (prin1 (car lista)) (spaces 1) (prin1 (cadr lista))
             (print (car (cddr lista)))
             (setq lista (cddr lista))
          )
*/
          (if (and (wordp (car lista))
                   (neq (plist (car lista)) '*IGNORE*)
                   (cdr lista) ; There is something following...
                   (arglistp (cadr lista)) ; And it's arglist.
              )
              (progn
                 (create-info (car lista) () ())
                 (add-parent  (car lista) fun-name)
                 (add-child   fun-name (car lista))
/*
                 (spaces 1)
                 (prin1 (car lista))
                 (print (cadr lista))
 */
                 (setq lista (cdr lista))
              )
          )
;     )
/*
 */
      (if (listp (car lista))
         (browse-funbody (car lista) fun-name module))
      (setq lista (cdr lista))
   )
)


(defun lataa (x)
       (length (setq kala (readprog x))))


(defun test1 (module output &aux funs)
   (if (endmarkp output)
       (setq output *stdout*)
       (setq output (outfile output))
   )
   (if (or (not (boundp module)) (null (symeval module)))        
       (progn (set  module ()) ; Because it's probably *unbound*
              (setq veba (readprog module))
              (browse-toplevel veba module)
       )
   )
   (setq funs (nreverse (symeval module)))
   (princ module output) (princ ": " output) (princ (length funs) output)
   (princ " functions:" output) (terpri output)
   (print funs output) (terpri output)
   (mapc #'print-children funs (setq output (mcl output)))
   (free-cons output)
)

(defun print-children (fun-name output)
   (terpri output)
   (print fun-name output)
   (spaces 1 output)
   (print (get-children fun-name) output)
)

