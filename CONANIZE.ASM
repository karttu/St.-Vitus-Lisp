;
;   This module belongs to St. Vitus' Lisp which is the Lisp Interpreter
;   for the MS-DOS machines. This is used also by many other programs
;   which use the list-subsystem software package coded by Antti Karttunen,
;   e.g. softwares like KANJIDIC (Electronic Kanji Dictionary) and ODE11
;   (Octal Debugger & Executor for the PDP-11 code).
;   Following text applies to this module and to
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
        include lmacros.h
        include mask-asm.h
;
; /* Convert 32 bit long-pointer (in largedata memory model)
;     to conanized long-pointer.
;    (First it is converted to 20-bit absolute address, then the four highest
;      bits are rorred to proper position).
;  */
; /* As canonized pointer means pointer whose segment part is of the form
;     XXXXh (i.e. anything), and offset is 000Xh (i.e. 0-15),
;    the conanized pointer means pointer whose segment part is of the form
;     X000h, and offset is XXXXh (i.e. anything), so that
;    ((highest nybble of segment * 65536) + offset) = 20 bit absolute address.
;  */
;
; void *conanize(longptr)
; void *longptr;
; 
;
	procdef	conanize,<<loword,word>,<hiword,word>>
	mov	ax,loword
	mov	dx,hiword
	mov	cl,4
	rol	dx,cl
	mov	bx,dx
	and	bx,0fff0h       ; Get to bx bits 11-0 of seg multiplied by 16.
	and	dx,0fh          ; Move bits 15-12 of seg. to bits 3-0 of dx
	add	ax,bx           ; Add bx to offset.
	adc	dx,0            ; Add possible carry to dx.
;                               ; Now dx,ax contains 20-bit absolute address
        ror     dx,cl           ; Move 4 highest bits of abs. addr. back to
;                               ;  bits 15-12 of dx
	pret
	pend	conanize
;
;
; /* This is like conanize, but additionally sets the typebyte */
;
; TOB  t_conanize(longptr,typebyte)
; void *longptr;
; BYTE typebyte;
; 
;
	procdef	t_conanize,<<lo_word,word>,<hi_word,word>,<typebyte,byte>>
	mov	ax,lo_word
	mov	dx,hi_word
	mov	cl,4
	rol	dx,cl
	mov	bx,dx
	and	bx,0fff0h       ; Get to bx bits 11-0 of seg multiplied by 16.
	and	dx,0fh          ; Move bits 15-12 of seg. to bits 3-0 of dx
	add	ax,bx           ; Add bx to offset.
	adc	dx,0            ; Add possible carry to dx.
;                               ; Now dx,ax contains 20-bit absolute address
        ror     dx,cl           ; Move 4 highest bits of abs. addr. back to
;                               ;  bits 15-12 of dx
        or      dh,typebyte     ; Set typebyte
	pret
	pend	t_conanize
;
;
; /* Returns segment part of canonized long pointer in AX register.
;     Used for address-range type-testing.
;  */
;
; UINT canonseg(tob_object)
; TOB tob_object; 
; /* tob_object is conanized long pointer with possible type tag */
;
        procdef canonseg,<<low_word,word>,<high_word,word>>
        mov     ax,low_word
        mov     dx,high_word
        mov     cl,4
        shr     ax,cl           ; Bits 15-4 of offset to bits 11-0 of ax.
; Because this is shift, not roll, it means that bits 15-12 will be zero.
        and     dh,SEGBITS      ; Mask off typebits & others from dh.
        or      ah,dh           ; And put those upper segment bits to ax.
        pret
        pend    canonseg
;
;
        finish
        END   
