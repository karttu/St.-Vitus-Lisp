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
; Functions lsetjmpl and llongjmp for the lisp-functions return, reset, etc.
; Coded by Antti Karttunen once upon a time.
; Only difference to the Aztec-C supplied library-functions is that these
;  transfer 32-bit values instead of 16-bit
;  integers. And lsetjmp returns -1L instead of zero when returns first
;  time after installing the setjmp buffer. (So that NIL's can be returned
;  via this passage).
;
;
	include	lmacros.h
	procdef lsetjmp,<<jmpbuf,ptr>>
	pushds
	ldptr	bx,jmpbuf,ds
	lea	ax,jmpbuf       ; Get sp
	mov	[bx],ax         ; and save it first.
	mov	ax,0[bp]	;get caller's BP
	mov	2[bx],ax
	mov	4[bx],si
	mov	6[bx],di
	mov	ax,2[bp]	;caller's IP
	mov	8[bx],ax
ifdef FARPROC
	mov	ax,4[bp]	;caller's CS
	mov	10[bx],ax
endif
        MOV     AX,0FFFFh       ; Return -1L, i.e. endmark as result,
        MOV     DX,AX           ;  when returning after installation.
	popds
	pret
	pend	lsetjmp
;
	procdef llongjmp,<<jbuf,ptr>,<LO_RETVAL,word>,<HI_RETVAL,word>>
	mov	ax,LO_RETVAL
        MOV     DX,HI_RETVAL
ifndef LONGPTR
	mov	bx,ds
	mov	es,bx
endif
	ldptr	bx,jbuf,es
	mov	sp,es:[bx]
	mov	bp,es:2[bx]
	mov	si,es:4[bx]
	mov	di,es:6[bx]
ifdef FARPROC
	jmp	es:dword ptr 8[bx]
else
	jmp	es:word ptr 8[bx] ; This returns directly to caller of lsetjmp
endif
	pend	llongjmp
	finish
	end
