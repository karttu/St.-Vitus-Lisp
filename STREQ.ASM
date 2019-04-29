;
;   This module belongs to St. Vitus' Lisp which is the Lisp Interpreter
;   for the MS-DOS machines. This is used also by many other programs.
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
; streq  --  coded by AK at 7. March 1989.
; Calling sequence:
;
;       int streq();
;       char *s1,*s2;
;       if(streq(s1,s2)) { ... }
;
; Returns 1 if string s1 is equivalent to beginning of string s2, otherwise 0.
; e.g:  streq("inu","neko")       ==>  0
;       streq("pimp","pimp")      ==>  1
;       streq("pimp","pimpero")   ==>  1
;       streq("pimpero","pimp")   ==>  0
; Note the following conventions:
;       streq("","")              ==>  1
;       streq("","anything")      ==>  0
;
	include	lmacros.h
dataseg segment para public 'data'
	extrn	ctp__:byte	; Flag table for isalpha, isdigit, etc. macros
;                               ;  in ctype.c (macros defined in ctype.h)
dataseg ends
	assume  cs:codeseg,ds:dataseg,es:dataseg,ss:dataseg
;
;
	procdef	streq,<<str1,ptr>,<str2,ptr>>
	mov	cx,7fffh        ; Put the biggest positive int. to count-reg
	cld                     ; Direction is forward
	push	si
	push	di
	pushds
	ldptr	si,str1
ifndef LONGPTR
	mov	di,ds
	mov	es,di
endif
	ldptr	di,str2
cmploop:
	lodsb                   ; MOV AL,DS:[SI++]
	scasb                   ; CMP AL,ES:[DI++]
	jne	notequal
	test	al,al           ; If both strings have ended...
	loopnz	cmploop         ;  then fall through this, and return true.
ret_true:
        mov     ax,1            ; Return true (that is: one)
        jmp     short done
notequal:
        test    al,al           ; If str1 has not ended yet,
        jnz     ret_false       ;  then it is surely false.
        cmp     cx,7fffh        ; True only if str1 has ended, but it is
        jne     ret_true        ;  not "".
ret_false:
        xor     ax,ax           ; Otherwise false
done:
	popds
	pop	di
	pop	si
;
	pret
	pend	streq
;
;
; Like previous but this doesn't care about case, so for example
;  monocase_streq("CyC","cYclOPE") returns 1.
;
	procdef	monocase_streq,<<str_1,ptr>,<str_2,ptr>>
	mov	cx,7fffh        ; Put the biggest positive int. to count-reg
	cld                     ; Direction is forward
	push	si
	push	di
	pushds
	ldptr	si,str_1
ifndef LONGPTR
	mov	di,ds
	mov	es,di
endif
	ldptr	di,str_2
        xor     bh,bh           ; Clear high byte of BX
ifdef LONGPTR
        mov     dx,ds           ; Save segment of source to dx.
endif
cmploop9:
	lodsb                   ; MOV AL,DS:[SI++]
        test    al,al           ; If high bit is on in AL byte, then
        js      not_alpha       ; it's not ascii (table ctp_ is only 128 long)
        mov     bl,al
ifdef LONGPTR          ; If compiled in largedata mode, then pop back original
        pop     ds     ;  DS pointing to data segment, so that flag table ctp_
endif                  ;   can be accessed.
        test    byte ptr [(ctp__+1)+bx],03 ; Tests whether AL is alphabetic
ifdef LONGPTR                   ; i.e. A-Z or a-z (see ctype.h)
        push    ds
        mov     ds,dx           ; Restore segment of source.
endif
        jz      not_alpha
        xor     al,es:[di]	; If xored two same letters with same
        jz      ok9		;  case, then result is zero. But if they
        cmp     al,20h		; are different case, then it is 0x20h.
        jne     ret_false9      ;  If different letters, then result is
ok9:				;   something else.
        inc     di
	dec	cx
        jnz     cmploop9
        jmp     short ret_true9
not_alpha:
	scasb                   ; CMP AL,ES:[DI++]
	jne	notequal9
	test	al,al           ; If both strings have ended...
	loopnz	cmploop9        ;  then fall through this, and return true.
ret_true9:
        mov     ax,1            ; Return true (that is: one)
        jmp     short done9
notequal9:
        test    al,al           ; If str1 has not ended yet,
        jnz     ret_false9      ;  then it is surely false.
        cmp     cx,7fffh        ; True only if str1 has ended, but it is
        jne     ret_true9       ;  not "".
ret_false9:
        xor     ax,ax           ; Otherwise false
done9:
	popds
	pop	di
	pop	si
;
	pret
	pend	monocase_streq
;
;
;/* ======================================================================= */
;
; int monostrequ(s,t)
; char *s,*t;
;
; Test whether strings s and t are equal. However, case is insignificant.
;
	procdef monostrequ,<<a,ptr>,<b,ptr>>
	mov	cx,7fffh
	jmp	short cmpcommon8
	pend	monostrequ
;
	procdef	monostrnequ,<<string1,ptr>,<string2,ptr>,<len,word>>
	mov	cx,len
cmpcommon8:
	cld
	push	si
	push	di
	pushds
	ldptr	si,string1
ifndef LONGPTR
	mov	di,ds
	mov	es,di
endif
	ldptr	di,string2
        xor     bh,bh           ; Clear high byte of BX
ifdef LONGPTR
        mov     dx,ds           ; Save segment of source to dx.
endif
;
;
cmploop8:
	lodsb
        test    al,al           ; If high bit is on in AL byte, then
        js      not_alpha8      ; it's not ascii (table ctp_ is only 128 long)
        mov     bl,al
ifdef LONGPTR          ; If compiled in largedata mode, then pop back original
        pop     ds     ;  DS pointing to data segment, so that flag table ctp_
endif                  ;   can be accessed.
        test    byte ptr [(ctp__+1)+bx],03 ; Tests whether AL is alphabetic
ifdef LONGPTR                   ; i.e. A-Z or a-z (see ctype.h)
        push    ds
        mov     ds,dx           ; Restore segment of source.
endif
        jz      not_alpha8
        xor     al,es:[di]	; If xored two same letters with same
        jz      ok8	        ;  case, then result is zero. But if they
        cmp     al,20h		; are different case, then it is 0x20h.
        jne     notequal8       ;  If different letters, then result is
ok8:				;   something else.
        inc     di
	dec	cx
        jnz     cmploop8
        jmp     short ret_true8
not_alpha8:
	scasb
	jne	notequal8
	test	al,al
	loopnz	cmploop8
ret_true8:
	mov     ax,1
        jmp     short retu8
notequal8:
	xor	ax,ax
retu8:
	popds
	pop	di
	pop	si
	pret
	pend	monostrnequ
;
	finish
;
	END  
