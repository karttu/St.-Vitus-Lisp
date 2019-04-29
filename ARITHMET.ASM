;
;
; ARITHMETIC.ASM  --  Basic arithmetic functions, coded in assembly language.
; By AK at 29. 3. 1989
; Added neg and bit-fiddling functions not, and, or, xor, shl, shr, sar
;  at 3-Jan-1991 (by AK)
; Plus sxt (sign extend) at 6-jan-1991.
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
quot_string db "/"
        db      0
rem_string  db  "%"
        db      0
dataseg ends
        assume  cs:codeseg,ds:dataseg,es:dataseg,ss:dataseg
;
;
; TOB add1(x)
; TOB x;
;
        public  add1_
add1_   proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word
        mov     ax,ss:[bx+2]    ; Get low word
        inc     ax              ;  and increment.
        ret
add1_   endp
;
;
; TOB sub1(x)
; TOB x;
;
        public  sub1_
sub1_   proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word
        mov     ax,ss:[bx+2]    ; Get low word
        dec     ax              ;  and decrement it.
        ret
sub1_   endp
;
;
; TOB plus(x,y)
; TOB x,y;
;
        public  plus_
plus_   proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word of first arg
        mov     ax,ss:[bx+2]    ; Get low word of first arg
        add     ax,ss:[bx+6]    ; And add low word of second arg to it.
        ret
plus_   endp
;
;
;
; TOB difference(x,y)
; TOB x,y;
;
        public  difference_
difference_   proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word of first arg
        mov     ax,ss:[bx+2]    ; Get low word of first arg
        sub     ax,ss:[bx+6]    ; And subtract low word of second arg from it.
        ret
difference_   endp
;
;
;
; TOB times(x,y)
; TOB x,y;
;
        public  times_
times_  proc    near
        mov     bx,sp
        mov     ax,ss:[bx+2]    ; Get low word of first arg.
        mul     ss:[bx+6]       ; And multiply it with low word of second arg.
        mov     dx,ss:[bx+4]    ; Get highword (i.e. tagbits) of first arg.
        ret
times_  endp
;
;
;
; TOB quotient(x,y)
; TOB x,y;
;
        public  quotient_
quotient_  proc    near
        mov     bx,sp
        mov     ax,ss:[bx+2]    ; Get low word of first arg. (Dividend)
        xor     dx,dx           ; Zero DX because div divides DX:AX
        mov     cx,ss:[bx+6]    ; Get low word of second arg (divisor)
        jcxz    quot_ertzu
        div     cx              ; Divide DX:AX with CX (low word of 2nd arg).
        mov     dx,ss:[bx+4]    ; Get highword (i.e. tagbits) of first arg.
        ret
quot_ertzu:                     ; _inv2arg("/",2,x,y);
        push    ss:[bx+8]
        push    ss:[bx+6]
        push    ss:[bx+4]
        push    ss:[bx+2]
        pushi   2
        push    ds
        pushadr quot_string
        call    _inv2arg_
quotient_  endp
;
;
;
; TOB remainder(x,y)
; TOB x,y;
;
        public  remainder_
remainder_  proc    near
        mov     bx,sp
        mov     ax,ss:[bx+2]    ; Get low word of first arg. (Dividend)
        xor     dx,dx           ; Zero DX because div divides DX:AX
        mov     cx,ss:[bx+6]    ; Get low word of second arg (divisor)
        jcxz    rem_ertzu
        div     cx              ; Divide DX:AX with CX (low word of 2nd arg).
        mov     ax,dx           ; Remainder will be placed to DX.
        mov     dx,ss:[bx+4]    ; Get highword (i.e. tagbits) of first arg.
        ret
rem_ertzu:                      ; _inv2arg("/",2,x,y);
        push    ss:[bx+8]
        push    ss:[bx+6]
        push    ss:[bx+4]
        push    ss:[bx+2]
        pushi   2
        push    ds
        pushadr rem_string
        call    _inv2arg_
remainder_  endp
;
;
;
; TOB _neg(x)
; TOB x;
;
        public  _neg_
_neg_   proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word
        mov     ax,ss:[bx+2]    ; Get low word
        neg     ax              ;  and negate it.
        ret
_neg_   endp
;
;
; TOB _sxt(x)
; TOB x;
;
        public  _sxt_
_sxt_   proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word
        mov     ax,ss:[bx+2]    ; Get low word,
        cbw                     ;  sign extend low byte of it. sxt(AL) -> AX
        ret
_sxt_   endp
;
;
; TOB _not(x)
; TOB x;
;
        public  _not_
_not_   proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word
        mov     ax,ss:[bx+2]    ; Get low word
        not     ax              ;  and complement it.
        ret
_not_   endp
;
;
; TOB _and(x,y)
; TOB x,y;
;
        public  _and_
_and_   proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word of first argument. (= type)
        mov     ax,ss:[bx+2]    ; Get low word of first argument.
        and     ax,ss:[bx+6]    ; and it with a low word of second arg.
        ret
_and_   endp
;
;
; TOB _or(x,y)
; TOB x,y;
;
        public  _or_
_or_    proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word of first argument. (= type)
        mov     ax,ss:[bx+2]    ; Get low word of first argument.
        or      ax,ss:[bx+6]    ; or it with a low word of second arg.
        ret
_or_    endp
;
;
; TOB _xor(x,y)
; TOB x,y;
;
        public  _xor_
_xor_   proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word of first argument. (= type)
        mov     ax,ss:[bx+2]    ; Get low word of first argument.
        xor     ax,ss:[bx+6]    ; xor it with a low word of second arg.
        ret
_xor_   endp
;
;
; TOB _shl(x,y) /* Shift Left, logical & arithmetic */
; TOB x,y;
;
        public  _shl_
_shl_   proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word of first argument. (= type)
        mov     ax,ss:[bx+2]    ; Get low word of first argument.
        mov     cx,ss:[bx+6]    ; Get shift count from low word of second arg.
        shl     ax,cl		; Shift AX left.
        ret
_shl_   endp
;
;
; TOB _shr(x,y) /* Shift Right, logical */
; TOB x,y;
;
        public  _shr_
_shr_   proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word of first argument. (= type)
        mov     ax,ss:[bx+2]    ; Get low word of first argument.
        mov     cx,ss:[bx+6]    ; Get shift count from low word of second arg.
        shr     ax,cl		; Shift AX right.
        ret
_shr_   endp
;
;
; TOB _sar(x,y) /* Shift right, arithmetic */
; TOB x,y;
;
        public  _sar_
_sar_   proc    near
        mov     bx,sp
        mov     dx,ss:[bx+4]    ; Get high word of first argument. (= type)
        mov     ax,ss:[bx+2]    ; Get low word of first argument.
        mov     cx,ss:[bx+6]    ; Get shift count from low word of second arg.
        sar     ax,cl		; Shift AX right. (arithmetically).
        ret
_sar_   endp
;
;
        extrn   _inv1arg_:near
        extrn   _inv2arg_:near
codeseg ends
        END   
