
(load 'hubafish)  ; Load the hubafish program.

(mapc #'r (dir '*.fin)) ; Read in all the files with extension .FIN
; (they are sample files containing finnish text).

(main 10) ; Produce ten sentences.

(ibm-output t) ; Set ibm-scand-flag on.

(main 10) ; Produce ten sentences more, with IBM-scandis.

; (save 'hubaa.1) ; This would save the output to the file hubaa.1

(exit) ; Exit from interpreter.
