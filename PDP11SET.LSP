
/*
   Definition of assembly & disassembly-time behaviour of patterns
   and opcodes for PDP11.
   For GLAD (Generic Lisp Assembler/Disassembler).
   By Antti Karttunen 1991
 */


/*
Patterns:

%S, %D    General addressing mode. (any of the above)
%R        Any register R0 - R7
%N        Just a number itself.
%I        Displacement, like byte but must be in branching range from branch.
%J        SOB-displacement,  like 77, but must be in branching range from SOB.

%W        Sixteen bit word located after instruction. (Or after previous %W)
%Z	  Relative address, otherwise like %W.

*/


/* Assembly & Disassembly routines for %N & %Y:
   Just pass the values intact:
 */
(def-a (%N %Y) (lambda (n) (intp n)))
(def-d (%N %Y) (lambda (n) n))


; Compute the displacement from the destination address & location counter
;  at the assembly time.
; Maybe I should add branch range checking to this in future:

(def-a %I
 (lambda (x)
    (if (intp x)
        (cond ((neq ($and x 1) 0) ; If bit-0 is on, i.e. x is odd.
	        (rplast '(Branch destination address must be even, not X) x)
              ) ; If greaterp & lessp are signed, then change and to or:
              ((and (greaterp (setq diff ($sar (- x (get-LC)) 1)) 127.)
                    (lessp diff -128.)
               )
	        (list 'Cannot 'branch 'from (- (get-LC) 2) 'to x
		      'low-lim: (- (get-LC) 256.)
                      'up-lim: (+ (get-LC) 254.)
	        )
              )
	      (($and diff 0xff)) ; Mask the upper byte off.
	)
    )
 )
)

; Compute the destination address from LC & displacement:
(def-d %I (lambda (d) (+ (get-LC) ($shl (sxt d) 1))))

; Like above, but for SOB:
(def-a %J
 (lambda (x)
    (if (intp x)
        (cond ((neq ($and x 1) 0) ; If bit-0 is on, i.e. x is odd.
	        (rplast '(SOB: destination address must be even, not X) x)
              ) ; This must be unsigned greaterp:
              ((greaterp (setq diff ($shr (- (get-LC) x) 1)) 077)
	        (list 'SOB: 'cannot 'branch 'from (- (get-LC) 2) 'to x
		      'low-lim: (- (get-LC) 0176) 'up-lim: (get-LC)
	        )
              )
	      (diff)
	)
    )
 )
)

(def-d %J (lambda (d) (- (get-LC) ($shl d 1))))

; One more word to input/output (but without any calculations):
(def-a %W (lambda (word)
           (cond ((intp word) (outqueue word) t))
          )
)

(def-d %W (lambda (turha) (input-word)))

; Like previous, but that word is understood as PC-relative address,
; so subtract LC from it at assembly time,
; and add LC to it at disassembly:
(def-a %Z (lambda (word) 
           (cond ((intp word) (outqueue (- word (get-LC))) t))
          )
)

(def-d %Z (lambda (turha) (+ (get-LC) (input-word))))


; Return the corresponding numeric value 0-7 for symbols R0 - R7
;  (SP & PC are also usable), or -1 if invalid register name.
(def-a %R
        (lambda (reg) (if (setq reg (memqnth reg regs))
                          (if (greaterp reg 7) (- reg 2)
                              reg
                          )
                      )
        )
)

; Return the corresponding symbol R0-R5, SP or PC for numeric values 0-7
(def-d %R (lambda (n) (nth n regs)))

(setq regs '(R0 R1 R2 R3 R4 R5 SP PC R6 R7))


; Define the %S & %D modes which are used by the most of instructions:

(def-a-et-d (%S %D)
 (patterns
; PC-addressing modes:
  (^o27 # %W)       ; Immediate
  (^o37 @ # %W)     ; Absolute
  (^o67 %Z)         ; Relative
  (^o77 @ %Z)       ; Relative deferred

; Normal modes:
  (^o0R %R)         ; Register
  (^o1R (%R))       ; Register deferred
  (^o2R (%R)+)      ; Autoincrement
  (^o3R @(%R)+)     ; Autoincrement deferred
  (^o4R -(%R))      ; Autodecrement
  (^o5R @ -(%R))    ; Autodecrement deferred
  (^o6R %W(%R))     ; Index
  (^o7R @ %W(%R))   ; Index deferred
 )
)

; All the pattern variables which are allowed to match more than one
; elements from the datum list, MUST be included in this list:
(setq *ALLOW-PLURALS* '(%D %S))

(defun pluralp (x) (memq x *ALLOW-PLURALS*))

; Instruction set for PDP11
;  branches and traps are best defined in hexadecimal,
;  all others in octal.

(setq s11
'(

(^o000000    HALT)
(^o000001    WAIT)
(^o000002    RTI)
(^o000003    BPT)
(^o000004    IOT)
(^o000005    RESET)
(^o000006    RTT)
(^o000007    MFPT)

(^o0001DD    JMP %D)
(^o00020R    RTS %R)
(^o00023N    SPL %N)
(^o000240    NOP)
(^o000241    CLC)
(^o000242    CLV)
(^o000244    CLZ)
(^o000250    CLN)
(^o000257    CCC)
(^o000260    NOP2)
(^o000261    SEC)
(^o000262    SEV)
(^o000264    SEZ)
(^o000270    SEN)
(^o000277    SCC)

(^o0003DD    SWAB %D)

(^x01II      BR  %I)
(^x02II      BNE %I)
(^x03II      BEQ %I)
(^x04II      BGE %I)
(^x05II      BLT %I)
(^x06II      BGT %I)
(^x07II      BLE %I)

(^o004RDD    JSR %R , %D)

(^o0050DD    CLR %D)
(^o0051DD    COM %D)
(^o0052DD    INC %D)
(^o0053DD    DEC %D)
(^o0054DD    NEG %D)
(^o0055DD    ADC %D)
(^o0056DD    SBC %D)
(^o0057DD    TST %D)
(^o0060DD    ROR %D)
(^o0061DD    ROL %D)
(^o0062DD    ASR %D)
(^o0063DD    ASL %D)
(^o0064NN    MARK %N)
(^o0065SS    MFPI %S)
(^o0066DD    MTPI %D)
(^o0067DD    SXT %D)
(^o0070DD    CSM %D)

(^o01SSDD    MOV  %S , %D)
(^o02SSDD    CMP  %S , %D)
(^o03SSDD    BIT  %S , %D)
(^o04SSDD    BIC  %S , %D)
(^o05SSDD    BIS  %S , %D)
(^o06SSDD    ADD  %S , %D)

(^o070RSS    MUL  %S , %R)
(^o071RSS    DIV  %S , %R)
(^o072RSS    ASH  %S , %R)
(^o073RSS    ASHC %S , %R)
(^o074RDD    XOR  %R , %D)

(^o077RJJ    SOB  %R , %J)

(^x80II      BPL  %I)
(^x81II      BMI  %I)
(^x82II      BHI  %I)
(^x83II      BLOS %I)
(^x84II      BVC  %I)
(^x85II      BVS  %I)
(^x86II      BCC  %I)
(^x86II      BHIS %I) ; Synonym for BCC
(^x87II      BCS  %I)
(^x87II      BLO  %I) ; Synonym for BCS

(^x88YY      EMT  %Y)
(^x89YY      TRAP %Y)


(^o1050DD    CLRB %D)
(^o1051DD    COMB %D)
(^o1052DD    INCB %D)
(^o1053DD    DECB %D)
(^o1054DD    NEGB %D)
(^o1055DD    ADCB %D)
(^o1056DD    SBCB %D)
(^o1057DD    TSTB %D)
(^o1060DD    RORB %D)
(^o1061DD    ROLB %D)
(^o1062DD    ASRB %D)
(^o1063DD    ASLB %D)
(^o1064SS    MTPS %S)
(^o1065SS    MFPD %S)
(^o1066DD    MTPD %D)
(^o1067DD    MFPS %D)

(^o11SSDD    MOVB  %S , %D)
(^o12SSDD    CMPB  %S , %D)
(^o13SSDD    BITB  %S , %D)
(^o14SSDD    BICB  %S , %D)
(^o15SSDD    BISB  %S , %D)
(^o16SSDD    SUB   %S , %D)

(^xNNNN      .WORD %N)
))

; Wildcard patterns which match everything which 
; above patterns hasn't matched, i.e. illegal instructions:
; (setq *WC* '((^xNNNN      .WORD %N)))

