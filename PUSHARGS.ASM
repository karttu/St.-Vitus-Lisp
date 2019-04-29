;
;   This module belongs to St. Vitus' Lisp which is the Lisp Interpreter
;   for the MS-DOS machines. Following text applies to this module and to
;   all other modules in this package unless otherwise noted:
;
;   Copyright (C) 1991  Antti J. Karttunen
;
;   This program is free software; you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation; either version 1, or (at your option)
;   any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License (in file GPL.TXT) for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program; if not, write to the Free Software
;   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;
        include mask-asm.h
;
codeseg segment para public 'code'
dataseg segment para public 'data'
        extrn   _max_args_:byte ; Max. arguments allowed for vlambda 0 funs.
dataseg ends
        assume  cs:codeseg,ds:dataseg,es:dataseg,ss:dataseg
;
;
; int pushargs(argument_list,argument_count)
; TOB argument_list;
; int argument_count;
;
;  Auxiliary function for gapply, by AK at 12. March 1989.
;  Pushes arguments in argument_list to stack, but doesn't evaluate them.
;  Leaves arguments to stack, so that first arg in arglist is topmost
;  in stack (where sp points after that add sp,6), and last arg
;  in arglist is lowermost in stack, so the effect is as arguments
;  where pushed to stack from right to left (from last to first one
;  in arglist). Actually they are not pushed, but put into stack
;  with STOSW from left to right. (Direction flag is off, i.e. plusward)
;  Returns in AX the count of arguments actually pushed.
;
;  Note that discipline (lambda or vlambda) is indicated in the high byte
;  of arg_count. If it's vlambda with argcount other than zero, or a lambda,
;  then space is reserved for so many TOB's in stack.
;  However, if it's vlambda 0, i.e. some function with indetermined number
;  of arguments, like plus, nconc, etc, then space is reserved for _max_args
;  plus one TOB's, where _max_args is a global variable which can be modified
;  with function (set-max-args n).
;  After the arguments are put to the space reserved for them, and if register
;  CX is still not zero after looptest, then it means that argument list was
;  shorter than arg_count specified. In that case one ENDMARK (-1L) is put
;  after the arguments if discipline is vlambda, but if it's lambda, then
;  the remaining stack positions are filled with NIL's.
;  However, in case of the vlambda 0, ENDMARK is always put after the
;  arguments to indicate that they end here.
;  This feature edited at 21/02/1992.
;
;  Modified 22. AUG 1991 to use inline car instead of function car.
;  Also checks if arglist is an atom or ends in atom, then that is
;  also put to stack.
;  E.g: (apply #'fun 'a) is equivalent to (apply #'fun '(a))
;  and  (apply #'fun '(a . b)) is equivalent to (apply #'fun '(a b))
;
;  Note that the manipulation of the stack is very delicate here, and you must
;  pay extreme caution when editing this code. Especially it's important
;  that never is any data on the "wrong" (= minus) side of the stack pointer,
;  because then it can be corrupted when interrupt comes at the unfortunate
;  moment and overwrites the data on the stack, if sp is pointing to the wrong
;  location.
;  
;  Called from gapply:
;
;  argcnt = pushargs(arglist,argcnt);
;  In assembly as:
;
;       push    arg_count            ; This is ored with 0100h if vlambda.
;       push    high_word_of_arglist
;       push    low_word_of_arglist
;       call    pushargs_
;       add     sp,6
;
VLAMBDA_MASK EQU 01h            ; set by gapply with statement: argcnt |= 256;
VLAMBDA0MASK EQU 02h            ; set here if zero formal arguments detected.
;
        public  pushargs_
pushargs_ proc  near
        CLD                     ; Clear the direction flag for STOSW
        MOV     BX,SP           ; Use BX instead of BP as frame-pointer
        MOV     CX,SS:[BX+6]    ; Get arg-count
        TEST    CL,CL           ;Test whether zero formal arguments specified.
        JNZ     skip11          ; If not.
        TEST    CH,CH           ; Test if vlambda...
        JZ      skip11          ; If not.
; So this part is executed only if discipline is vlambda and 0 fargs.
        OR      CH,VLAMBDA0MASK
        MOV     CL,_max_args_   ; If vlambda 0 then use _max_args+1 as
        MOV     AL,CL           ; argument count.
        XOR     AH,AH
        INC     AX              ; Reserve 4 bytes more for endmark
        JMP     SHORT skip22
skip11: ; If not vlambda 0 then just use the argcount given.
        MOV     AX,CX           ; Move count to AX.
        XOR     AH,AH           ; Clear upper byte to get it correct.
;
skip22:
        SHL     AX,1            ; Multiply...
        SHL     AX,1            ;  with 4 (= sizeof(TOB))
        SUB     SP,AX           ; Make space to stack for arg-count TOBs
; (Ret-adr & arguments are overwritten with arguments got from arglist)
        MOV     AX,SS:[BX+2]    ; Load arglist to AX & to...
        MOV     DX,SS:[BX+4]    ;  ...DX. (Low word in AX, High word in DX)
        MOV     BX,SS:[BX]      ; Get return address
        ADD     SP,8            ; (But ret-adr and args take 8 bytes already)
; Note that we should get return address & arglist to registers, before
;  we push anything, because those PUSH:es can overwrite them.
        PUSH    DI              ; Save DI
        MOV     DI,SP           ; Starting address for STOSW
        ADD     DI,2            ; Compensate PUSH DI (it subtracted 2 from SP)
        PUSH    SI              ; Save SI
        PUSH    BX              ; Save return address
        MOV     SI,CX           ; Get vlambda-flag & argcount to SI...
        XOR     CH,CH           ; Clear CH, so that in CX is proper count now
        INC     CX              ; And increment count with 1, because we
; jump directly to the end of loop to test, and that takes one from CX
        JMP     SHORT looptest
loop22:
        TEST    DH,TAGBITS      ; Check whether DX:AX is (still) cons cell.
        JNZ     NOT_A_CONS      ; If not then jump to special place.
;
        PUSH    DX              ; Push argument
        PUSH    AX              ; here for cdr.
; 22-AUG-1991: Call to CAR replaced by "inline car".
; We can be sure at this location that DX:AX is a proper cons cell now,
; and not () (that's checked in the end of loop, where is jumped first).
        MOV     ES,DX           ; Segment is in the conanized high word.
        MOV     BX,AX           ; And offset is in AX.
        MOV     AX,ES:[BX]      ; Get the low word for car-field.
        MOV     DH,ES: BYTE PTR [BX+2]  ; And high byte.
        AND     DX,(SEGTYPEBITS SHL 8)  ; Clear other than seg & typebits
;
        MOV     BX,SS           ; Copy SS
        MOV     ES,BX           ;  to ES so that STOSW can be used.
        STOSW                   ; es:(di)++ = ax
        MOV     AX,DX
        STOSW                   ; es:(di)++ = dx
; Note that cdr shouldn't change CX nor direction flag!!!
        CALL    cdr_            ; using those uncorrupted DX & AX pushed
;                               ;  at the start of loop.
; because cdr returns result in DX,AX then it means that argl = cdr(argl)
        ADD     SP,4            ; Drop arguments of car
looptest:
        MOV     BX,AX           ; Test if (), i.e. DX:AX is long zero.
        OR      BX,DX           ; Result is 0 only if both AX & DX are zero.
        LOOPNZ  loop22          ; Loop so long as DX,AX is not nil & CX not 0
; If put less arguments than specified in arg count, then put the endmark
; after them (doesn't care whether this was called as lambda or vlambda):
        TEST    CX,CX
out_of_loop: ; We can jump directly to here from NOT_A_CONS because DEC CX
; sets also flags, and JMP doesn't change them.
        JNZ     put_endmark     ; CX is not zero, if less args than arg count.
        TEST    SI,(VLAMBDA0MASK*256) ; Test if vlambda & 0 fargs specified.
        JZ      skip33          ; Otherwise if other case, don't put it.
put_endmark: ; But if vlambda 0 then put also endmark after arguments:
        MOV     AX,SS           ; Again necessary, because cdr corrupted
        MOV     ES,AX           ;  ES-register.
        TEST    SI,(VLAMBDA_MASK*256) ; If not vlambda
        JZ      fill_nils       ;  but lambda, then jump to fill nils.
        MOV     AX,0FFFFh       ; Put endmark as last argument
        STOSW                   ;  (endmark = -1L = FFFFFFFFh)
        STOSW
skip33:
        MOV     AX,SI           ; Get argcnt to AX (or _max_args_ if vlambda)
        XOR     AH,AH           ; (Clear vlambda-flag, if it is on)
        SUB     AX,CX           ; And subtract CX to get number of arguments
;                               ;  "pushed" to stack.
        POP     BX              ; Restore the return address
        POP     SI              ; Restore SI
        POP     DI              ; Restore DI
        SUB     SP,6            ; Subtract 6 from SP, because when we return
;                               ;  there is ADD SP,6 after call.
        JMP     BX              ; And return via BX
;
fill_nils:
; Without this saving of CX evalargs would evaluate all these
; filled NIL's too, which is unnecessary.
        PUSH    CX              ; Save the original "missing args count".
        SHL     CX,1            ; Double it.
        XOR     AX,AX           ; So that so many NIL's (= doubleword 0)
        REP     STOSW           ;  are written, with this REP instruction.
        POP     CX              ; Pop the original CX back.
        JMP     skip33
;
NOT_A_CONS: ; Encountered atom which is also put to stack (as last one).
        MOV     BX,SS           ; Copy SS to ES
        MOV     ES,BX           ; so that STOSW puts stuff to correct segment 
        STOSW                   ; es:(di)++ = ax
        MOV     AX,DX
        STOSW                   ; es:(di)++ = dx
        DEC     CX              ; Decrement CX to get correct value.
        JMP     out_of_loop     ; And jump out of the loop.
;
pushargs_ endp
;
; evalargs(n)
; int n;
;
; Evaluate n arguments in stack, starting from topmost, and because topmost
;  was leftmost (= first) in arglist, it means that arguments are
;   evaluated from left to right.
; (Although they were "pushed" from "right to left").
;
        public  evalargs_
evalargs_ proc  near
        MOV     BX,SP
        MOV     CX,SS:[BX+2]    ; Get n
        JCXZ    poistu          ; Don't evaluate anything if n is 0
        ADD     BX,4            ; Skip return address & n
;                               ;  so now BX points to first argument
loop33:
        MOV     AX,SS:[BX]      ; Get low and
        MOV     DX,SS:[BX+2]    ;  high word of argument
        PUSH    CX              ; Eval may corrupt CX
        PUSH    BX              ;  and propably it corrupts BX too.
        PUSH    DX              ; Push high &
        PUSH    AX              ;  low word for eval
        CALL    eval_
        ADD     SP,4            ; Unwind stack (drop those DX & AX)
        POP     BX              ; Get uncorrupted BX &
        POP     CX              ;  CX from stack
        MOV     SS:[BX],AX      ; Replace arg with its
        MOV     SS:[BX+2],DX    ;  evaluated version.
        ADD     BX,4            ; Let BX point to next arg
        LOOP    loop33
poistu:
        RET
evalargs_ endp
;
;
; pushl(x)   /* x is pushed to stack */
; TOB x; /* = double word (= long) */
;
; Called from C-routine:
;        push    high_word_of x
;        push    low_word_of x
;        call    pushl_
;        add     sp,4
;
         public  pushl_
pushl_   proc    near
         ret     -4              ; Pop -4 bytes off the stack (= sub sp,4).
pushl_   endp
;
;
; pushw(x) /* pushes word x to stack */
; int x;
;
         public  pushw_
pushw_   proc    near
         ret     -2              ; Pop -2 bytes off the stack (= sub sp,2).
pushw_   endp
;
;
;
; /* Returns in DX & AX the top-dword of the stack (and drops it): */
; TOB popl()
; Called from C-routine:
;
;        call    popl_
;        mov     hi_word_of_result,dx
;        mov     lo_word_of_result,ax
;
         public  popl_
popl_    proc    near
         mov     bx,sp           ; Use bx as "frame-pointer" (instead of bp),
;                                ;  (because bx doesn't need to be saved).
         mov     ax,ss:[bx+2]    ; Get low word from stack
         mov     dx,ss:[bx+4]    ; Get high word from stack
         ret     4               ; Pop 2 words off the stack (those ^ above)
popl_    endp
;
;
; int popw()
;
         public  popw_
popw_    proc    near
         mov     bx,sp           ; Use bx as "frame-pointer" (instead of bp)
         mov     ax,ss:[bx+2]    ; Get word from stack
         ret     2               ; Pop 2 bytes off the stack (= 1 word)
popw_    endp
;
;
; dropl() /* drops one doubleword from stack (increments sp with 4) */
;
; By the way, same thing would be accomplished with following: (if it worked)
;
; #define dropl()\
; #asm\
; ADD SP,4\
; #endasm
;
;
         public  dropl_
dropl_   proc    near
         ret     4               ; So simple !
dropl_   endp
;
;
;
; This is an obsolete testing function which contains self-modifying code,
; but I think it works only when using single-stepping in debugger, but
; not in actual run-time because of the instruction fetch queue.
; Anyway, it's not used for any purpose, and it's commented out:
;
; dropnbytes(n)
; int n;
;
; Called from C-routine:
;
;        push    n
;        call    dropnbytes_
;        add     sp,2
;
;         public  dropnbytes_
;dropnbytes_ proc near
;         mov     bx,sp           ; Use bx as "frame-pointer" (instead of bp)
;         mov     ax,ss:[bx+2]    ; Get n
;         mov     cs:(addret+1),ax ; and modify ret-instruction with it
;addret   label   word
;         ret     0DADAh          ; DADA is overwritten with ax
;dropnbytes_ endp
;
;
;
;
;
; int get_sp() /* returns stack pointer (before & after the call) */
;
; Called from C-routine:
;        result = get_sp();
; that produces:
;        call    get_sp_
;        mov     result,ax
;
         public  get_sp_
get_sp_  proc    near
         mov     ax,sp           ; Get sp to ax
         add     ax,2            ; And let it point to there where it pointed
;                                ;  before the call (because *sp is ret-addr)
         ret                     ; And return back
get_sp_  endp
;
;
;
; set_sp(x) /* Sets sp to x */
; int x;
;
; Called from C-routine:
;        set_sp(x);
; that produces:
;        push    x
;        call    set_sp_
;        add     sp,2
;
         public  set_sp_
set_sp_  proc    near
         mov     bx,sp           ; Use bx as "frame-pointer"
         mov     ax,ss:[bx]      ; Get return address
         mov     sp,ss:[bx+2]    ; Set new sp
         push    ax              ; Push return address for RET
         ret     -2              ; Pop -2 bytes off the stack (= sub sp,2).
;                                ;  (i.e. compensate the effect of add sp,2)
set_sp_  endp
;
;
;
; TOB pickl(index)
; int index;
; /* Returns longword from stack-segment, i.e: SS:[index] & SS:[index+2] */
;
         public  pickl_
pickl_   proc    near
         mov     bx,sp           ; Use bx as "frame-pointer"
         mov     bx,ss:[bx+2]    ; Get argument to bx
         mov     ax,ss:[bx]
         mov     dx,ss:[bx+2]
         ret
pickl_   endp
;
;
; /* Pokes stuff to SS:[index] & SS:[index+2], returns stuff */
; /* Used by setq to modify locals & arguments of functions,
;     e.g. (setq (pick 1) 'kala)
;  */
; TOB pokeltostack(index,stuff)
; int index;
; TOB stuff;
;
        public  pokeltostack_
pokeltostack_ proc near
        mov     bx,sp
        mov     ax,ss:[bx+4]    ; Get LO of stuff
        mov     dx,ss:[bx+6]    ; Get HI of stuff
        mov     bx,ss:[bx+2]    ; Get index
        mov     ss:[bx],ax
        mov     ss:[bx+2],dx
        ret
pokeltostack_ endp
;
        extrn   car_:near
        extrn   cdr_:near
        extrn   eval_:near
codeseg ends
        END     
