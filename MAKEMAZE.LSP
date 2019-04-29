
(setq *macronames*
 '(ZERO_EXIT ONE_EXIT TWO_EXIT THREE_EXIT FOUR_EXIT FIVE_EXIT SIX_EXIT
   SEVEN_EXIT EIGHT_EXIT NINE_EXIT TEN_EXIT)
)


; Define direction aliases:
(setq n 'north)
(setq e 'east)
(setq s 'south)
(setq w 'west)
(setq u 'up)
(setq d 'down)
(setq ne 'northeast)
(setq nw 'northwest)
(setq se 'southeast)
(setq sw 'southwest)


(setq MAXFILENAME 81)

(setq filename (new-string (add1 MAXFILENAME)))

(defun load-rooms (rooms)
    (setq rooms (convert-string rooms toupper))
    (strcpy filename rooms)
    (strcat filename ".ROO")
    (@= *esc-flag* 0) ;Don't handle backslashes in strings in any special way.
    (load filename)
    (@= *esc-flag* 1) ;Restore normal handling of backslash-escapes in strings

    (cond ((not (boundp rooms)) (load-fail filename rooms))
          ((not (boundp '*prefix*)) (load-fail filename '*prefix*))
	  (t rooms)
    )
)

(defun load-fail (filenam varname)
       (princ "Rooms definition file ") (princ filenam)
       (princ " is invalid! Variable ") (princ varname)
       (princ " is not declared.\r\n")
       (reset)
)


(defun write-room-file
  (roomname &aux fp description directions destroom direction n-exits)
     (setq directions (cdr roomname))
     (setq roomname (car roomname))
     (strcpy filename roomname)
     (strcat filename ".c")
     (setq fp (outfile filename))
     (princ "#include \42std.h\42" fp)
     (terpri fp)
     (terpri fp)
     (setq description (car directions))
     (if (nonnilsymbolp description) (setq description (symeval description)))
     (setq directions (cdr directions))
     (setq n-exits (/ (length directions) 2))
     (princ (nth n-exits *macronames*) fp)
     (princ "(" fp) (terpri fp)
     (while directions
        (setq direction (car directions))
        (setq destroom  (cadr directions))
        (setq directions (cddr directions))
	(princ `\42` fp)
	(princ *prefix* fp)
	(princ destroom fp)
	(princ ".c\42,\42" fp)
	(if (and (boundp direction) (nonnilsymbolp direction))
	    (setq direction (symeval direction))
	)
	(princ direction fp)
	(princ "\42,\r\n" fp)
     )
     (prin1 (car description) fp) (princ ",\r\n" fp)
     (print (cadr description) fp) (princ ",\r\n" fp)
     (princ (car (cddr description)) fp) (princ ")\r\n" fp)
     (close fp)
)


(defun start (&optional mazename)
 (cond ((not (nonnilsymbolp mazename))
 (princ "\r\nUsage: (start 'mazename)\r\n")
 (princ "File called mazename.roo should contain maze definition.\r\n")
 (princ "Where mazename of course means the name of your choice.\r\n")
 (princ "Room files will be created to the current directory.\r\n")
 (princ "For example: (start 'mymaze1) digs the maze from file MYMAZE1.ROO\r\n")
  (princ "Use (exit) to exit from Lisp.\r\n")
	   ()
	 )
       (t (mapc #'write-room-file (symeval (load-rooms mazename))))
 )
)


(start ())

