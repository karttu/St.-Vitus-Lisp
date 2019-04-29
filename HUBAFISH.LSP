

/*
   HUBAFISH.LSP  --  Sentence scrambler using idea of Markov Chains,
   coded for St. Vitus' Lisp at the end of July 1990 by Antti Karttunen.
   Inspired by HUGEFISH.C by Jari Toivanen and Computer Recreations article
   of June 1989 Scientific American by A. K. Dewdney (from page 96 onward).
   (And Sci-Am January 1990, page 138 & 139)
   Note that this is still little bit buggy. Outputs some extraneous
   punctuation marks now and then (like " , ' and so on).

   Copyright (C) 1991  Antti J. Karttunen

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

    And remember! I don't take the responsibility for the stories
    the hubafish will produce.

 */

/*
    Usage:
    First load this program in to St. Vitus' Lisp with command
    (load 'hubafish) or automagically if you start the interpreter
    with command: lisp -lhubafish
    Then if you read english text execute the function (set-e-mode).
    Then read one or more textfiles in with (r 'filename) function,
    e.g. (r 'firstfile.txt) (r 'second.txt) etc. You can read also
    multiple files with wildcards, like this way:
    (mapc #'r (dir '*.fin))
    reads all the finnish texts coming as sample with this package.
    Then you can produce the text with (main n) function, where n
    is the number of sentences produced. If you think the result was
    good you can save the result with (save "filename") function,
    or you can get the next n sentences with another (main n) call.
    If you execute (ibm-output t) before using main or save then
    the seven-bit scandis are substituted to eight-bit IBM-ones
    from all the words being output. Use (exit) to exit from
    the interpreter.

    So it goes like this:
    lisp -lhubafish       ; Load the hubafish.lsp to interpreter.
    (ibm-output t)        ; Do this if you want to get IBM-scandis on output.
    (set-e-mode)          ; Do this if you read english text, but NOT with
                          ; the finnish text!
    (r 'filename.ext)     ; Read single file with name filename.ext
    (mapc #'r (dir '*.fin)) ; Or read all the files with *.FIN extensions.
    (main 10)             ; Output ten sentences to the screen.
    (save "filename.ext") ; Save it to the file named filename.ext.
    (main n)              ; N sentences more.
    (save "filename.ext") ; Save again. If filename is same then stuff
                          ;  is appended to the end of file.
    (exit)                ; When you have seen enough.
*/



(set-max-args 21)

; Handle scandinavian letters {, |, }, [, \ and ] (a, o, A & O with dots, etc)
;  as normal alphabetic letters not as special characters:
(setscandmode t) ;Comment this out if english text and contains brackets, etc.
; (or simply enter (setscandmode ()) before reading any text)

; Define read mode good for reading the human text.
(defun setsentmode (&aux i)
   (setq i 0) ; First set charflags so that only digits, letters and
   (while (lessp i 128) ; seven-bit scandis plus all eight-bit letters
                        ; are continuous, others not.
      (@= *charflags* (if (isalnump i) 1 0) i)
      (setq i (add1 i))
   )
   (setq ibase 0) ; Read all numbers as symbols.
   (rplacx `;`  *read-macros* ()) ; Semicolon, no effect.
   (rplacx `'`  *read-macros* ()) ; No quote read macro
   (rplacx `#`  *read-macros* ()) ; No function read macro
   (rplacx `\`` *read-macros* ()) ; No charquote.
   (rplacx `"`  *read-macros* ()) ; No strings.
   (rplacx `(`  *read-macros* ()) ; No lists.
   (rplacx `)`  *read-macros* ())
; Symbols are parsed with special function:
   (rplaca *read-macros* *wordsymbol-rm*)
   (@= *sent-flag* 1) ; Convert IBM-scandis on input to seven-bit ones.
   t
)

; Convert all seven-bit scandis to ibm-scandis when outputting stuff
; if this function has been executed with non-nil argument.
(defun ibm-output (x)
   (not (@= *ibmscand-flag* (if x 1 0)))
)


(setq *DOT*     (intern "."))
(setq *COMMA*   (intern ","))
(setq *SEMICOL* (intern ";"))
(setq *COLON*   (intern ":"))
(setq *DQUOTE*  (intern (@= "A" `"`)))
(setq *LPAR*    (intern "("))
(setq *RPAR*    (intern ")"))

(set *DOT*     nil)
(set *COMMA*   nil)
(set *SEMICOL* nil)
(set *COLON*   nil)
(set *DQUOTE*  nil)
(set *LPAR*    nil)
(set *RPAR*    nil)
(setq ? nil)
(setq ! nil)

(setq minus -)

(setq *MARKOV* ())
(setq *W* ())
(setq *P* ())

/* If this is t then dashes are skipped at the beginning of sentences: */
(setq *NO-DASHES* ())

/* If this is t then commas, colons and semicolons are simply skipped: */
(setq *NO-PUNCTS* ())

(setq *SENT-END-CHARS* (clist *DOT* *LPAR* () *RPAR* *DQUOTE* '! '?))
(setq *SPECIALS* (clist *DQUOTE* *LPAR* () *RPAR*))
(setq *SENT-PUNCT-CHARS* (clist *COMMA* *SEMICOL* *COLON*))

(setq *COL* 64) ; The rightest allowed column. (+/- 1)
; This affects how many sentences is printed in one paragraph:
(setq *PARA-CONSTANT* 2)

(setq *OUTPUT* *stdout*)

(setq *root-word* (setq *words-in-queue* ()))
;
; *queue1* and *queue2* are queues used by read-word to buffer sentences read
;  inside parentheses. When read-word encounters a list instead of symbol,
;  it is added to *queue1*. When current sentence ends, then *queue1* is
;  moved to *queue2* and words are read from it until it is nil.
;
(setq *queue1* (setq *queue2* ()))

(setplist '*first-words* (setq *first-words* ()))
(setq *word-count* 0)
(setq *sent-count* 0)
(setq *new-words*  0)
(setq *first-count* 0)

(setq *word-total* (setq *sent-total* (setq *new-total* 0)))

(setq *seed* 1)
(setq *save-n* 10)

(setq *sentences* 0)

/* This starts a new paragraph if global variable *sentences* has come
    to zero. Otherwise it is just decremented.
 */
(defun new-paragraph? (col)
  (cond ((zerop *sentences*) ; If *sentences* has come to zero ?
           (if (not (zerop col)) (terpri *OUTPUT*)) ;Emit CR+LF if needed.
           (spaces 1 *OUTPUT*) (setq col 1) ; Emit one space and set column.
           (setq *sentences* ; Set new count of sentences to be output.
             (add1 (plus (intran *PARA-CONSTANT*) (intran *PARA-CONSTANT*))))
        ) ; Use "two dice formula"...
        ((setq *sentences* (sub1 *sentences*))) ; Else decrement *sentences*
  )
  col) ; Return col as result.


(defun sent-endp (word)
       (memq word *SENT-END-CHARS*))

(defun punctp (word)
       (memq word *SENT-PUNCT-CHARS*))

(defun *punctp (word) (memq word '(- :)))

(defun no-value-before-p (x) (memq (symeval x) '(() *unbound*)))

/* This saves the latest story to file named filename */
(defun save (filename)
       (if (not (*stringp filename))
           (progn (princ "Give filename as argument !") (terpri))
/* else */ (main *save-n* *seed* filename)))


(defun main (n &optional seed filename)
    (setq *save-n* n)
    (setq *words-in-queue* ())
    (setq *seed*
          (cond ((intp seed) (setseed seed))
           ((setseed)))) ; If not seed specified, then use time as seed.
    (intran 100) ; Take one random number here, because first one is usually
;                   one and same number when using time as seed.
;                   (Doesn't matter anymore ?)
    (if (not (intp seed)) (setq filename seed))
    (if (*stringp filename)
/* then */ (setq *OUTPUT* (outfile filename "a")) ; Open in append mode
           (setq *OUTPUT* *stdout*)) ; Otherwise output stuff to *stdout*.
;
    (setq *sentences* (setq col 0))
    (while (not (zerop n))
       (setq col (output-sentence col t))
       (setq n (sub1 n))
    )
    (terpri *OUTPUT*)
    (terpri *OUTPUT*)
;   Close the output if it is to file:
    (cond ((not (eq *OUTPUT* *stdout*)) (close *OUTPUT*)))
    (princ 'Seed:) (spaces 1) (print *seed*)
)


(defun output-sentence (col sent-beg-flag &aux word1 word2 orig-word2)

  (setq word1 ())
  (setq orig-word2 ())
  (setq word2 (choose-word (plist '*first-words*)))

  (cond ; If - is first of sentence then print it on the new line:
    ((eq word2 '-)
        (if (not (zerop col)) (terpri *OUTPUT*)); Emit CR+LF if needed
        (spaces (setq col 1) *OUTPUT*)
    )
    ((setq col (new-paragraph? col))) ; Else check if new paragraph.
  )

  (while (not (sent-endp word2))
; Always print commas at the same line as word they follow:
      (if  (not (punctp word2))
           (cond ; If this word would go over rightest allowed column:
              ((greaterp (plus col (strlen word2)) *COL*)
                  (terpri *OUTPUT*) (setq col 0)) ; then start the new line.
               ((cond ((not (zerop col)) (spaces 1 *OUTPUT*)
                                          (setq col (add1 col)))))
           )
      )
; If beginning of the sentence, then print first word capitalized:
      (if sent-beg-flag (print-upper word2) (print-word word2))
      (setq col (plus col (strlen word2)))
      (if (not (and sent-beg-flag (eq word2 '-))) ; If first of sentence was -
          (setq sent-beg-flag ())) ; then leave sent-beg-flag still t.
;
; next-word is given args word1 and word2, except if orig-word2 is not nil,
;  then that is used instead of word2. word2 is set to result what next-word
;   returns, and word1 is set to old value of word2:
; (Note that this works only if args are evaluated from left to right).
      (setq word2 (next-word word1 (setq word1 (or orig-word2 word2))))
      (setq orig-word2 ())
; If in pattern mode, and current word2 has matched to something, then pick
;  something it has matched to, and set orig-word2 to value of original word2:
      (and *P* (patternp (symeval word2)) (setq orig-word2 word2)
        (setq word2 (choose-word (plist (symeval word2)))))
  )

  (princ (cond ((memq word2 *SPECIALS*) *DOT*) (t word2)) *OUTPUT*)
  (add1 col)
)



/* Print symbol x. If its value is *CAPITAL*, then print it capitalized. */
(defun print-word (x)
       (cond ((eq (symeval x) '*CAPITAL*) (print-upper x))
              ((princ x *OUTPUT*))))


(defun print-upper (x)
; Print first letter as uppercase, and then rest of symbol unchanged:
       (princ (toupper (@ x)) *OUTPUT*) (princ (add1 x) *OUTPUT*) x)


(defun read-word (input sent-beg-flag &aux word)
   (cond ((setq word (if *queue2* (pop-first *queue2*) (read input)))
;
; Word's value is set to *CAPITAL*, only if it has been read xbit on (it's
; capitalized in text), sent-beg-flag is not on, and symbol hasn't any 
; previous value:
           (if (nonnilsymbolp word)
            (progn
             (if (and (xbitp word)
                      (not sent-beg-flag) ; Not first word of sentence ?
                      (no-value-before-p word)) ; No value before ?
                 (set word '*CAPITAL*)
             )
             (setxbit word ()) ; Clear x-bit of the word.
            )
           )
         )
   )
; Skip commas and semicolons if *NO-PUNCTS* flag is on, and anyway
;  always at the beginning of sentences:
   (cond ((or (and (punctp word) (or sent-beg-flag *NO-PUNCTS*))
; If read a list, then add it to *queue1*, and also read a second time:
              (and (consp word) ; add dot to the end of list:
                    (setq *queue1* (nconc *queue1* (putlast word *DOT*)))))
               (read-word input sent-beg-flag)) ; Try a second time.
          (word) ; Else return this word, whether it is nil or whatever.
   )
)



(defun r (filename &aux juusto)
  (progn
    (setq *root-word* (setq *words-in-queue* ()))
    (setq *word-count* 0)
    (setq *sent-count* 0)
    (setq *new-words*  (length *oblist*))
    (setq *first-count* (or (car (plist '*first-words*)) 0))
    (setq *INPUT* (infile filename))
    (setsentmode)
    (setq ibase 0) ; Read numbers too as symbols.
    (setq juusto t)
    (while juusto
       (setq juusto (read-word *INPUT* t))
       ; Skip empty sentences:
       (cond ((not (or (sent-endp juusto) (and *NO-DASHES* (eq juusto '-))))
               (setq *sent-count* (add1 *sent-count*))
               (add-word () '*first-words* juusto)
               (setq juusto
                     (read-sentence () juusto
                                    (read-word *INPUT* (*punctp juusto))))
; Note that if previous word is - or : then read next word with beg-sent-flag
;  set t in read-word, so that next is not capitalized. (that's why *punctp)
             )
       )
    )
    (setq *new-words*
          (minus (length *oblist*) *new-words*))
    (setq *first-count*
          (minus (or (car (plist '*first-words*)) 0)
                 (if (intp *first-count*) *first-count* 0)))
    (close *INPUT*)
    (reset-readmode)
    (setq ibase 10)
    (word-status filename)
  )
)


(defun read-sentence (word1 word2 word3)
   (if (null *queue2*)
       (progn
        (setq *queue2* *queue1*)
        (setq *queue1* ())
       )
   )
   (while t
      (setq *word-count* (add1 *word-count*))
; If word3 is nil, then give dot to add-word:
      (add-word word1 word2 (or word3 *DOT*))
      (if (sent-endp word3) (return word3)) ; Stop if end of sentence.
      (setq word1 word2)
      (setq word2 word3)
      (setq word3 (read-word *INPUT* (*punctp word3)))
   )
)

/*
(defun read-sentence (preword word *INPUT*)
   (cond ((null word) ())
         (t
            (setq *word-count* (add1 *word-count*))
            (add-word preword word)
            (or (sent-endp word) ;Return if end of sentence, othewise recurse:
                (read-sentence word (read-word *INPUT* (*punctp word)) *INPUT*))
         )
   )
)
 */


/* Format of the plist's of words is:
   (total-count  count1 word1  count2 word2  count3 word3  ... countn wordn)

   So that count1 is count of word1's and so on.
   It should be guaranteed that total-count is always sum of count1 - countn,
   and that count and corresponding word are in same compact list, so that
   prevcdr can be safely used to get count when word has been found with
   member.
 */


/* This buffers words to list *words-in-queue* until word whose value is not
    *NO-CUT* is found, then that is put last to *words-in-queue*, it
    is nreversed and handled to add-word-aux.
   Of course most of the words hasn't *NO-CUT* value, so they are handled
    directly to add-word-aux, without any buffering.
 */

(defun add-word (word1 word2 word3 &aux pattern)
; If pattern-mode on, and word3 is symbol, and it has no previous value,
;  and if it matches to some pattern:
   (and *P* (nonnilsymbolp word3) (no-value-before-p word3)
        (set word3 (setq pattern (matchp word3 *patterns*)))
; Then set that pattern to be value of it, and add word3 to follower
;  list of that pattern:
       (setplist pattern (add-word-to-list word3 (plist pattern)))
   )

   (if (eq (symeval word3) '*NO-CUT*) ; Should this word be bufferized ?
       (progn (setq *words-in-queue* (cons word3 *words-in-queue*))
              (if (null *root-word*) (setq *root-word* word2)))
/* else */
; If something in *words-in-queue*, then add this word3 to it,
       (cond (*words-in-queue* ; and then nreverse it:
               (setq word3 (nreverse (cons word3 *words-in-queue*)))
; It is guaranteed that if *words-in-queue* is non-nil then *root-word*
;  is also, and vice versa.
; If there was some *root-word* in use, then use that, and reset it to nil:
               (setq word2 *root-word*)
               (setq *words-in-queue* (setq *root-word* ())) ; "Clear" them.
             )
       )
       (add-word-aux word1 word2 word3)
   )
)

/* This piece of code was in the beginning of add-word, now commented out:
;
; Check whether word2's plist is pattern-list (usually finnish word "ja"):
   (and (nonnilsymbolp (car (plist word2))) ; If plist contains only symbols,
; And if word3 matches to some of them:
        (setq pattern (matchp word3 (plist word2)))
; Then add word3 to plist of that pattern, and set word3 to be that pattern:
        (setq word3 (add-word-aux () pattern word3))) ; add-word-aux rets pat.
*/


/*
    Add some words to follower lists of another words.
    There is some possibilities for optimization here...
 */
(defun add-word-aux (word1 word2 word3 &aux x)
   (if (and *MARKOV* word1) ; If using Markov-chains, and word1 is not nil:
; Then put word3 to a follower list after pair <word1,word2>:
; (if word2 is not found after word1, then there is some bug:)
       (cond ((null (setq x (member word2 (plist word1))))
                (terpri) (princ "**Bug in add-word-aux, words: ")
                 (princ word1) (spaces 1) (princ word2) (spaces 1)
                  (print word3) (reset))
;If there is follower list already after this pair, then just add word3 to it:
             ((consp (cadr x)) (add-word-to-list word3 (cadr x)))
;If there isn't, then insert fresh follower list to appropriate place:
             (t (insert (list (add-word-to-list word3 ())) x))
       )
    )
; Anyway, always add word3 to follower list of word2:
    (setplist word2 (add-word-to-list word3 (plist word2)))
   word2 ; Return word2 as result.
)


(defun add-word-to-list (word lista)
   (if *W* (add-word-to-list-2 word lista) (add-word-to-list-1 word lista)))


(defun add-word-to-list-1 (word lista)
       (if (memq word lista)   ; If word already in lista,
           lista               ; then return that lista intact.
/* Commented currently out:
           (cons word lista)   ; Otherwise, add word to the front of lista.
 */
           (attach word lista) ; This puts word physically to lista
       )
)


/*
   Adds word to lista and updates counts.
   Note that word can also be a list, e.g. like: (, ja sitten)
    (that's why member is used instead of memq).
 */
(defun add-word-to-list-2 vlambda (word lista x)
   (cond ((null lista) ; If lista is still empty. Karhusta gaselli.
            (cons 1 (clist 1 word))) ; Then create a follower list.
         (t ; Else... (some hairy code ahead...)
            (rplaca lista (add1 (car lista))) ; Increment total count by one
            (if (setq x (member word lista)) ; If word is in lista already
/* Then */          (rplaca (prevcdr x) (add1 (car (prevcdr x)))) ; +1 count
/* else new word */ (insert (clist 1 word) lista)
; Put (1 word) between, just after total-count.
            )
            lista
         )
   )
)


/* Destructively puts lista1 just after first element of lista2.
E.g: (setq veba '(a b c))
     (setq hiba '(x y z))
     (insert veba hiba)
produces:
     hiba and result will be: (x a b c y z)
     veba will be: (a b c y z)
 */

(defun insert (lista1 lista2) (linsert lista1 lista2 1))

/* Old code:
(defun insert (lista1 lista2)
       (rplacd lista2 (nconc lista1 (cdr lista2)))
       lista2)
 */

/* Now built-in function
(defun attach (item lista)
       (if (null lista) (list item)
           (rplacd lista (cons (car lista) (cdr lista)))
           (rplaca lista item)
       )
)
 */


(defun next-word vlambda (word1 word2 lista x)
  (cond ((null *words-in-queue*) ; If no words in queue, then choose new one.
          (setq *words-in-queue* (next-word-aux word1 word2))
        )
  )

; If next-word-aux returned nil, then it is error:
  (cond ((null *words-in-queue*)
           (progn (terpri) (princ "**Bug in next-word. words: ") (princ word1)
                  (spaces 1) (print word2) (reset)))
        (t (setq x (pop-first *words-in-queue*)))
  )

  (cond ((eq (symeval x) '*PATTERN*) ; If result is pattern...
; Save old queue to lista, and set queue to nil if necessary: (commented out)
/*         (if (setq lista *words-in-queue*) (setq *words-in-queue* ())) */
; Choose the new word from pattern's plist:
           (setq x (choose-word (plist x)))
/*
           (if lista ;If there was old queue, then add it to end of new queue:
               (setq *words-in-queue* (add-to-beg *words-in-queue* lista)))
 */
        )
  )

  x ; Return x
)


(defun next-word-aux (word1 word2)
   (if (and *MARKOV* word1) ; If using Markov-chains, and word1 is not nil:
; Then take some after pair <word1,word2>:
       (choose-word (cadr (member word2 (plist word1))))
       (choose-word (plist word2)))) ; Else just take from plist of word2



(defun choose-word vlambda (lista x)
    (if (intp (setq x (car lista))) ; If first of lista is integer,
        (progn
          (setq x (intran x)) ; then it's total count.
          (cadr (member #'x (cdr lista) /* :TEST */ /* #'*/sub-while-plusp))
        )
; Else it is not weighted list, so just take randomly some element:
        (if (nonnilsymbolp (setq x (randnth lista))) x (car lista))
    )
)



/* Argument should be a global variable name. If it is a list, then
    first of it is "removed" (non-destructively) and returned as result.
   If value of variable is not a list, then that is returned and value set
    to nil.
 */
(defun pop-first nlambda (x)
       (/* funcall #' */ (lambda (b c)
                           (set b (and (consp c) (cdr c)))
                           (if (consp c) (car c) c))
                         (car x) (symeval (car x))
       ) ; Transfer a name and value of global variable to lambda-form.
)


/*
   Note that x is not used at all, instead is used free-variabled called *x*
 */
/* Now coded in C, in module lispfuns.c
(defun sub-while-plusp (x y)
       (and (* or: if *) (intp y) (minusp (setq *x* (minus *x* y)))))
 */

/* Old code:
; Subtract word counts from x until it becomes negative:
           (while (plusp (setq x (minus x (car lista))))
                  (if (null (setq lista (cddr lista)))
                      (progn (princ "**Bug in next-word. word: ")
                             (print word)
                             (reset))
                  )
           )
           (setq *words-in-queue* (cadr lista))
 */


(defun randnth (lista)
       (nth (intran (length lista)) lista))



(defun dump-words () (dump-words-aux () ()))

(defun dump-words-aux (l k)
   (setq l (cdr (plist '*first-words*)))
   (while l
      (princ (cadr l))
      (spaces (minus 32 (strlen (cadr l))))
      (princ (car l))
      (princ `\t`)
      (if (consp (setq k (cadr l))) (setq k (car (last k))))
      (princ (symeval k)) (spaces 2) (print (plist k))
/*
      (setq k (reverse (plist (car l))))
      (while k
        (princ `\t`) (princ (car k)) (spaces (minus 32 (strlen (car k))))
          (print (symeval (car k)))
        (setq k (cdr k))
      )
 */
      (setq l (cddr l))
   )
   (terpri)
   (word-status "")
)


(defun word-status (filename)
   (setq *word-total* (plus *word-total* *word-count*))
   (setq *new-total*  (plus *new-total* *new-words*))
   (setq *sent-total* (plus *sent-total* *sent-count*))
   (princ filename) (terpri)
   (princ "Words:")       (spaces 9) (princ *word-count*)
     (princ `\t`) (print *word-total*)
   (princ "New-words:")   (spaces 5) (princ *new-words*)
     (princ `\t`) (print *new-total*)
   (princ "Sentences:")   (spaces 5) (princ *sent-count*)
     (princ `\t`) (print *sent-total*)
   (princ "First-words:") (spaces 3) (princ *first-count*)
     (princ `\t`) (princ (car (plist '*first-words*)))
     (princ `\t`) (print (length (plist '*first-words*)))
   (terpri)
)

; E: poikkeukset, eli substantiivit ja muut jotka my|s m{tch{{v{t ko.
;  patterniin, tai sitten ko. verbiluokan verbit jotka eiv{t m{tch{{ siihen.

; Note that this must be scanned in reverse order, from the end to the
;  beginning, so that correct results are produced.
; (Because ??*<aeouy{|>t matches to the same words as matches
;   ??*<aeouy{|>v<a{>t  and  ??*iv<a{>t  so latter two must be matched
; first).
;
; Numerolla merkityt (oikeassa laidassa) ovat melko spesifisi{ ko.
;  sanaluokkiin, muut m{tch{{v{t mihin sattuu.
;

(setq *verb-patterns*
      (list
        '??*<aeouy{|>n      ;  000   0  1. pers, prees, yks. SURVON
; tapan, nuolen, tartun, hyydyn, heit{n, l|h||n. E: KALAN
        '??*<aeouy{|>mme    ;  001   1  1. pers, prees, mon. SURVOMME        1
; E: amme, aasimme.
        '??*in              ;  010   2  1. pers, imper, yks. SURVOIN
; E: kudin, nakin
        '??*imme            ;  011   3  1. pers, imper, mon. SURVOIMME       3
        '??*<aeouy{|>t      ;  100   4  2. pers, prees, yks. SURVOT
; E: orvot, koirat
        '??*<aeouy{|>tte    ;  101   5  2. pers, prees, mon. SURVOTTE        5
; E: putte
        '??*it              ;  110   6  2. pers, imper, yks. SURVOIT
; E: kanit
        '??*itte            ;  111   7  2. pers, imper, mon. SURVOITTE       7
        '??*<aeiouy{|>@     ; 1000   8  3. pers, prees, yks. SURVOO          8
; E: ruokaa. E: sy|, juo, nai. (a ja {-loppuiset my|s partitiiveja).
        '??*<aeouy{|>v<a{>t ; 1001   9  3. pers, prees, mon. SURVOVAT        9
; E: kanavat.    Testattava ennen 4:sta.
        '??*<^i>i           ; 1010  10  3. pers, imper, yks. SURVOI
; E: opossumi. ??*i riitt{isi jos t{m{ testattaisiin 8:n j{lkeen.
        '??*iv<a{>t         ; 1011  11  3. pers, imper, mon. SURVOIVAT      11
; E: kultarivat. Testattava ennen 4:sta.
      )
)

(setq *f-patterns* ; Finnish patterns
  (clist
    (nth 11 *verb-patterns*) ;             Huorasivat
    (nth  9 *verb-patterns*) ;             Huoraavat
;   (nth  8 *verb-patterns*) ;             Huoraa     (COMMENTED OUT)
    '??*<eiouy|>@       ; Survoo. a ja { poistettu koska liikaa partitiiveja.
    (nth  7 *verb-patterns*) ;             Huorasitte
    (nth  5 *verb-patterns*) ;             Huoraatte
    (nth  3 *verb-patterns*) ;             Huorasimme
    (nth  1 *verb-patterns*) ;             Huoraamme

; Joitakin substantiivien sijamuotoja:

    '???*ksi          ; Translatiivi.       Huoraksi, huoriksi.
; E: lasikuitusuksi, suffiksi.
    '???*ss<a{>       ; Inessiivi.          Huorassa, huorissa.
; E: vernissa
    '???*st<a{>       ; Elatiivi.           Huorasta, huorista.
; E: kapusta
    '???*<aiouy{|>@n  ; Illatiivi.          Huoraan,  huoriin.
; E: murhaan.
    '???*ll<a{>       ; Adessiivi.          Huoralla, huorilla.
; E: valhalla.
    '???*lt<a{>       ; Ablatiivi.          Huoralta, huorilta.
    '???*lle          ; Allatiivi.          Huoralle, huorille.
    '???*tt<a{>       ; Abessiivi.          Huoratta, huoritta.
    '???*neen         ; Komitatiivi. (+ 3. pers mon)  Huorineen.
;
    '???*<aiouy{|>@ns<a{> ; Illatiivi + 3. pers       Huoraansa
    '???*ns<a{>           ; Genetiivi + 3. pers, yks. Huoransa.

    '???*<^m>inen         ; Adjektiiveja: Sininen, kiimainen, maaninen.
; Poikkeuksia: maahinen, vetehinen. (mutta ei matchaa esim. sanaan ihminen)
  )
)

(setq *e-patterns* ; English patterns
  (clist
    '???*ed           ; flawed, snored
    '??*ate           ; meditate
;   '??*ates          ; meditates
;   '??*ated          ; meditated
    '???*less         ; wireless
    '??*ful           ; powerful, awful
    '??*al            ; infernal, oral
    '???*able         ; inflatable
    '???*ible         ; incredible
;   '??*ally          ; logically
;   '???*ibly         ; incredibly
    '??*ly            ; logically, incredibly, enormously
    '??*like          ; human-like
    '??*some          ; gruesome, awesome
    '??*esque         ; grotesque, kafkaesque
    '???*ing          ; flatulating
    '???*sy           ; lousy, noisy, messy, sissy, tipsy (gypsy pussy ?)
    '???*ish          ; sluggish, brownish, english (fetish, swordfish ?)
    '???*ous          ; enormous
    '???*ic           ; exotic, sadistic
;   '???*'s           ; sing. genetive, but too much words like it's there's
    '???*s'           ; plural genetive
    '?*ation          ; nation, abstraction
    '?*ations         ; nations, abstractions
  )
)

(setq *patterns* *f-patterns*)

/* Don't use this mode anymore:
(defun set-x-mode ()
;      (set  *COMMA* '*NO-CUT*) ; This system doesn't work anymore....
;      (setq ja      '*NO-CUT*)
       (set-values (setplist 'ja (nreverse *verb-patterns*)) '*PATTERN*)
)
 */

; Now these are by default on:
(setq *MARKOV* t)
(setq *P* t)

(defun set-y-mode ()
       (setq *MARKOV* t)
       (setq *P* t)
)

(defun set-no-y-mode ()
       (setq *MARKOV* ())
       (setq *P* ())
)

(defun set-e-mode ()
       (setq *patterns* *e-patterns*)
       (setq *MARKOV* t)
       (setq *P* t)
; Make sure that some of the most common words of english are not
;  capitalized, even if in source text they are in the middle of sentence:
       (set-values '(a an the at as
         for from if in into of off on out to too up down low high also
         by 
         be can do go did done is are was were have has had may must might
         yes no way man so now before because some every most during time
         it we you me my he she his him her one two our us day they them their
         where when then there here with without that this those which what
         why anything everything nothing)
       '*NO-CAPITAL*)
)

(defun patternp (x) (memq x *patterns*))


/* This returns symbol/string x matches to, if it is found from the lista,
    otherwise NIL is returned.
 */
(defun matchp (x lista)
       (and (not (eq (symeval x) '*CAPITAL*))
            (car (member x lista /* :TEST */ strmatchp))))


/* Set value of all symbols in lista which are unbound or nil to be y: */
(defun set-values (lista y)
   (member y lista /* :TEST */
; Return always nil, so lista is handled to the bitter end:
           #'(lambda (a b)
; Set value of element only if it's symbol which is unbound or nil:
               (if (and (nonnilsymbolp b) (no-value-before-p b))
                   (set b a))
              ())
   )
)

(defun add-to-beg (item1 item2)
       (cond ((listp item1) (append item1 item2)) ; If item1 is cons or nil.
              ((cons item1 item2)))) ; If item1 is atom.



(defun pm vlambda (p l)
       (terpri)
       (print p)
       (terpri)
       (setq l *oblist*)
       (while (setq l (member p l #'*strmatchp))
              (print (car l))
              (setq l (cdr l))))

(defun p-p (x) ; Print contents of plist of x
   (mapc #'(lambda (a)
              (if (consp (princ a)) (setq a (car (last a)))
              (spaces (minus 24 (strlen a))) (print (plist a)))
           )
           (plist x)
   )
)


(defun p ()
   (mapc #'(lambda (x)
                (terpri) (print x) (print (length (plist x)))
                (mapc #'print (plist x))
           )
           *patterns*
   )
   ()
)

