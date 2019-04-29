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
        include mask-asm.h
;
codeseg segment para public 'code'
dataseg segment para public 'data'
	extrn	_T__:word
car_str db      "car"
        db      0
cdr_str db      "cdr"
        db      0
rplaca_str db   "rplaca"
        db      0
rplacd_str db   "rplacd"
        db      0
rplacc_str db   "rplacc"
        db      0
dataseg ends
        assume  cs:codeseg,ds:dataseg,es:dataseg,ss:dataseg
;
; Fundamental list accessing & modifying functions;
;  car,cdr,rplaca,rplacd, etc. Now coded in assembly language.
;
; Modified 22. AUG 1991 so that functions car, cdr, etc. now check
; if any of the tagbits are on, instead of just typebits.
; And car & cdr also make sure that no compact bit or that other
; surplus-bit are not left accidentally on in result. (i.e. they
; are certainly cleared).
; Also L_zerop modified so that it now returns its argument back
; if it's zero integer.
;
; Note that function cdr_ should keep at least register CX and
; direction flag (set by CLD or STD) intact, otherwise pushargs
; will not work! (See file PUSHARGS.ASM)
;
        public  car_
car_    proc    near
        mov     bx,sp
        les     bx,ss:[bx+2]    ; Offset to bx, seg&type-byte to es
        mov     dx,es
        mov     ax,bx
        or      ax,dx           ; If NIL,
        jz      retcar          ;  then return NIL.
;
        test    dh,TAGBITS      ; type of CONS is 0, so if some of the tagbits
        jnz     car_ertzu       ;  is on, then it is not CONS...
;
        cmp     bx,OFFSET_UPLIM ; CHECK FOR SEGMENT WRAP,
	ja	car_ertzu	;  added at 27-11-1989 by A.K.
;
        mov     ax,es:[bx]
        mov     dh,es: byte ptr [bx+2]
;       and     dx,((NOT T_CBIT) SHL 8) ; Clear DL & Compact-bit.
        and     dx,(SEGTYPEBITS SHL 8)  ; Clear other than seg & typebits
retcar:
        ret
car_ertzu: ; _inv1arg("car",arg,get2retadr(),getretadr(),latest_retadr);
        call    getretadr_
	push	ax
	call    get2retadr_
	push	ax
        push    dx
        push    bx
        push    ds
        pushadr car_str
        call    _inv1arg_       ; Never returns, goes to toplevel with setjmp
car_    endp
;
;
;
        public  cdr_
cdr_    proc    near
        mov     bx,sp
        les     bx,ss:[bx+2]    ; Offset to bx, segment to es
        mov     dx,es
        mov     ax,bx
        or      ax,dx           ; If NIL,
        jz      retcdr          ;  then return NIL.
;
        test    dh,TAGBITS      ; type of CONS is 0, so if some of tagbits 
        jnz     cdr_ertzu       ;  is on, then it is not CONS...
;
        cmp     bx,OFFSET_UPLIM ; CHECK FOR SEGMENT WRAP,
	ja	cdr_ertzu	;  added at 27-11-1989 by A.K.
;
        test    es:byte ptr [bx+2],T_CBIT ; Test if compact-bit on in car-part
        jz      normal_cons
        mov     ax,bx
        add     ax,3            ; Just add sizeof internal stuff. (3 bytes)
        ret                     ; This takes less time (than that jmp).
;       jmp     short retcdr
normal_cons:
;
        mov     ax,es:[bx+3]
        mov     dh,es:byte ptr [bx+5]
        and     dx,(SEGTYPEBITS SHL 8)  ; Clear other than seg & typebits
;       xor     dl,dl
retcdr:
        ret
cdr_ertzu: ; _inv1arg("cdr",arg,get2retadr(),getretadr(),latest_retadr);
        call    getretadr_
	push	ax
	call    get2retadr_
	push	ax
        push    dx
        push    bx
        push    ds
        pushadr cdr_str
        call    _inv1arg_       ; Never returns, goes to toplevel with setjmp
cdr_    endp
;
;
; TOB rplaca(cons_cell,stuff_to_car_part)
; TOB cons_cell,stuff_car_part;
;
;       push    hiword_of_stuff       [bp+10] segment&type-byte: [bp+11]
;       push    loword_of_stuff       [bp+8]
;       push    hiword_of_conscell    [bp+6]  segment&type-byte: [bp+7]
;       push    loword_of_conscell    [bp+4]
;       call    rplaca_
;       add     sp,8
;
        public  rplaca_
rplaca_ proc    near
        push    bp
        mov     bp,sp
        les     bx,[bp+4]    ; Offset of 1st arg to bx, hiword to es
        mov     dx,es        ; Copy of hiword to dx
        mov     ax,bx        ;  And low word to ax
        or      ax,dx        ; If NIL then ertzu...
        jz      rplaca_ertzu
;                            ; (because returns as result cons_cell in DX,AX)
;
        test    dh,TAGBITS      ; type of CONS is 0, so if some of tagbits 
        jnz     rplaca_ertzu    ;  is on, then it is not CONS...
;
        cmp     bx,OFFSET_UPLIM ; CHECK FOR SEGMENT WRAP,
	ja	rplaca_ertzu	;  added at 27-11-1989 by A.K.
;
        mov     cx,[bp+8]       ; Replace low word...
        mov     es:[bx],cx      ;  ...of car-part.
        mov     ch,byte ptr [bp+11] ; Get seg&type-byte of stuff
        test    es: byte ptr [bx+2],T_CBIT ; Test cbit of original car
        jz      not_cbit        ; If cbit was on in original car,
        or      ch,T_CBIT       ;  then it must be set for new one too.
not_cbit:
        mov     es: byte ptr [bx+2],ch ; And replace segment&type-byte of car
;       mov     sp,bp           ; I think this is not needed...
        mov     ax,bx           ; Copy of offset to ax. Needed for result.
        pop     bp
        ret
rplaca_ertzu:
; inv2arg("rplaca",1,first_arg,second_arg,get2retadr(),getretadr(),ret_adr);
        mov     bx,bp
        pop     bp              ; Pop away old BP, so ret-adr is now topmost
        call    getretadr_
	push	ax
	call    get2retadr_
	push	ax
        push    ss:[bx+10]
        push    ss:[bx+8]
        push    ss:[bx+6]
        push    ss:[bx+4]
        pushi   1
        push    ds
        pushadr rplaca_str
        call    _inv2arg_
rplaca_ endp
;
;
;
        public  rplacd_
rplacd_ proc    near
        push    bp
        mov     bp,sp
        les     bx,[bp+4]    ; Offset of 1st arg to bx, hiword to es
        mov     dx,es        ; Copy of hiword to dx
        mov     ax,bx        ;  And low word to ax
        or      ax,dx        ; If NIL then ertzu...
        jz      rplacd_ertzu
;
        test    dh,TAGBITS      ; type of CONS is 0, so if some of tagbits 
        jnz     rplacd_ertzu    ;  is on, then it is not CONS...
;
        cmp     bx,OFFSET_UPLIM ; CHECK FOR SEGMENT WRAP,
	ja	rplacd_ertzu	;  added at 27-11-1989 by A.K.
;
        and     es: byte ptr [bx+2],(NOT T_CBIT)  ; Clear cbit from car-part
        mov     ax,[bp+8]
        mov     es: [bx+3],ax   ; Set low-word of cdr.
        mov     al,byte ptr [bp+11]
        mov     es: [bx+5],al   ; Set seg&type-byte of cdr.
        mov     ax,bx           ; Copy of offset to ax. Needed for result.
        pop     bp
        ret
rplacd_ertzu:
; inv2arg("rplacd",1,first_arg,second_arg,get2retadr(),getretadr(),ret_adr);
        mov     bx,bp
        pop     bp              ; Pop away old BP, so ret-adr is now topmost
        call    getretadr_
	push	ax
	call    get2retadr_
	push	ax
        push    ss:[bx+10]
        push    ss:[bx+8]
        push    ss:[bx+6]
        push    ss:[bx+4]
        pushi   1
        push    ds
        pushadr rplacd_str
        call    _inv2arg_
rplacd_ endp
;
; /* Compact-bit of conscell x is set if integer-argument bit is non-zero,
;     and cleared if it is zero. Previous value of compact-bit is returned.
;  */
; int rplacc(x,bit)
; TOB x;
; int bit;
;
        public  rplacc_
rplacc_ proc    near
        push    bp
        mov     bp,sp
        les     bx,[bp+4]    ; Offset of 1st arg to bx, hiword to es
        mov     dx,es        ; Copy of hiword to dx
        mov     ax,bx        ;  And low word to ax
        or      ax,dx        ; If NIL then ertzu...
        jz      rplacc_ertzu
;
        test    dh,TAGBITS      ; type of CONS is 0, so if some of tagbits 
        jnz     rplacc_ertzu    ;  is on, then it is not CONS...
;
        cmp     bx,OFFSET_UPLIM ; CHECK FOR SEGMENT WRAP,
	ja	rplacc_ertzu	;  added at 27-11-1989 by A.K.
;
        xor     ax,ax           ; Result is returned in AX
        test    es: byte ptr [bx+2],T_CBIT ; Test if cbit on...
        jz      cbit_off        ;
        inc     ax              ; Return 1 if previous cbit was on.
cbit_off:
        test    [bp+8],0FFFFh   ; Test if integer-argument non-zero.
        jnz     non_zero
        and     es: byte ptr [bx+2],(NOT T_CBIT)  ; Clear cbit from car-part
        jmp     short pulla
non_zero:
        or      es: byte ptr [bx+2],T_CBIT ; Set cbit
pulla:
        pop     bp
        ret
rplacc_ertzu:
; inv2arg("rplacc",1,first_arg,second_arg,get2retadr(),getretadr(),ret_adr);
        mov     bx,bp
        pop     bp              ; Pop away old BP, so ret-adr is now topmost
        call    getretadr_
	push	ax
	call    get2retadr_
	push	ax
        pushi   (T_INT*256)     ; Push type-mask for integer
        push    ss:[bx+8]
        push    ss:[bx+6]
        push    ss:[bx+4]
        pushi   1
        push    ds
        pushadr rplacc_str
        call    _inv2arg_
rplacc_ endp
;
;
;/* q_car: quick car, no check for NIL, no check for type.
;   Used for accessing value/function-definition field of symbol.
; */
;
        public  q_car_
q_car_  proc    near
        mov     bx,sp
        les     bx,ss:[bx+2]    ; Offset to bx, seg&type-byte to es
        mov     ax,es:[bx]
        mov     dh,es: byte ptr [bx+2]
        and     dx,(SEGTYPEBITS SHL 8)  ; Clear other than seg & typebits
;       and     dx,((NOT T_CBIT) SHL 8) ; Clear DL & Compact-bit.
        ret
q_car_  endp
;
;
;/* q_cdr: quick cdr, no check for NIL, no check for type.
;   Used for accessing property-list field of symbol.
; */
;
        public  q_cdr_
q_cdr_  proc    near
        mov     bx,sp
        les     bx,ss:[bx+2]    ; Offset to bx, seg&type-byte to es
        mov     ax,es:[bx+3]
        mov     dh,es:byte ptr [bx+5]
        and     dx,(SEGTYPEBITS SHL 8)  ; Clear other than seg & typebits
;       xor     dl,dl
        ret
q_cdr_  endp
;
;
;
        public  q_rplaca_
q_rplaca_ proc  near
        push    bp
        mov     bp,sp
        les     bx,[bp+4]    ; Offset of 1st arg to bx, hiword to es
        mov     dx,es        ; Copy of hiword to dx
        mov     ax,bx        ;  And low word to ax
;                            ; (because returns as result cons_cell in DX,AX)
;
        mov     cx,[bp+8]       ; Replace low word...
        mov     es:[bx],cx      ;  ...of car-part.
        mov     ch,byte ptr [bp+11] ; Get seg&type-byte of stuff
        mov     es: byte ptr [bx+2],ch ; And replace segment&type-byte of car
;       mov     sp,bp           ; I think this is not needed...
        pop     bp
        ret
q_rplaca_ endp
;
;
        public  q_rplacd_
q_rplacd_ proc  near
        push    bp
        mov     bp,sp
        les     bx,[bp+4]    ; Offset of 1st arg to bx, hiword to es
        mov     dx,es        ; Copy of hiword to dx
        mov     ax,[bp+8]
        mov     es: [bx+3],ax   ; Set low-word of cdr.
        mov     al,byte ptr [bp+11]
        mov     es: [bx+5],al   ; Set seg&type-byte of cdr.
        mov     ax,bx           ; Copy of offset to ax. Needed for result.
        pop     bp
        ret
q_rplacd_ endp
;
;
; int i_consp(X)
; TOB X;
; This returns 1 if argument is of type cons (list, but not NIL),
;  0 otherwise.
;
        public  i_consp_
i_consp_ proc    near
        mov     bx,sp
        mov     al,byte ptr ss:[bx+5] ; Get the segment & type byte of arg.
        test    al,TYPEBITS
        jnz	not_a_cons
	test    al,al                 ; If seg & type is zero, then arg is
	jnz     ret_true              ;  probably NIL.
        cmp     word ptr ss:[bx+2],0  ; And if low word of arg is zero too,
	je	not_a_cons            ;  then it's NIL almost certainly.
;                                     ; (I don't care to check the bx+4 byte.)
ret_true:
	mov	ax,1
	ret
not_a_cons: ; (Return false)
	xor	ax,ax
	ret	
i_consp_ endp
;
;
;
; TOB L_eq(x,y)
; TOB x,y;
; /* Test whether objects x & y are physically same. If they are integers with
;     different subtypes, the T is still returned.
;  (Macro eq would return false in that case).
;  */
;
        public  L_eq_
L_eq_   proc    near
        mov     bx,sp
        mov     ax,ss:[bx+2]    ; Get low word of first arg.
        cmp     ax,ss:[bx+6]    ; Compare to low word of second arg.
        jne     eq_false
        mov     ax,ss:[bx+4]    ; Get high word of first arg.
        mov     dx,ss:[bx+8]    ; Get high word of second arg.
        cmp     ax,dx
        je      eq_true
        and     ah,TYPEBITS
        cmp     ah,T_INT
        jne     eq_false
        and     dh,TYPEBITS
        cmp     dh,T_INT
        je      eq_true
eq_false:
        xor     ax,ax           ; Clear AX & DX, that is: return NIL.
        cwd
        ret
eq_true:
        mov     ax, word ptr _T__       ; return(_T_);
        mov     dx, word ptr _T__+2
        ret
L_eq_   endp
;
;
; L_zerop modified 22. AUG. 1991 so that it returns x back when it's
; zero instead of returning t as traditional lisps do.
;
; TOB L_zerop(x)
; TOB x;
;
        public  L_zerop_
L_zerop_ proc   near
        mov     bx,sp
        cmp     ss: word ptr [bx+2],0 ; Is there faster way to test if zero ?
        jne     zerop_false
        mov     dh,ss: byte ptr [bx+5] ; Get hi-byte of hi-word (seg&tag-byte)
        and     dh,TYPEBITS
        cmp     dh,T_INT
        jne     zerop_false
;       mov     ax, word ptr _T__       ; return(_T_);
;       mov     dx, word ptr _T__+2
        mov     ax, ss: [bx+2]          ; return(x);
        mov     dx, ss: [bx+4]
        ret
zerop_false:
        xor     ax,ax           ; Clear AX & DX, that is: return NIL.
        cwd
        ret
L_zerop_ endp
;
;
; The C function can use this to get return address to function which
;  has called it:
;
        public  getretadr_
getretadr_ proc   near
        mov     ax,[bp+2]
        ret
getretadr_ endp
;
; This can be used to get the return adress to function which has called
;  a function which has called a function which has called get2retadr
;
        public  get2retadr_
get2retadr_ proc   near
	push	bx
        mov     bx,[bp]		; Get the old BP
        mov     ax,ss:[bx+2]    ; Get the ancient return address.
	pop	bx
        ret
get2retadr_ endp
;
; Like previous but take return address from one step beyond:
;
        public  get3retadr_
get3retadr_ proc   near
	push	bx
        mov     bx,[bp]		; Get the old BP
	mov     bx,ss:[bx]      ; Get the still older BP
        mov     ax,ss:[bx+2]    ; Get the ancient return address.
	pop	bx
        ret
get3retadr_ endp
;
;
;
        extrn   _inv1arg_:near
        extrn   _inv2arg_:near
codeseg ends
        END   
