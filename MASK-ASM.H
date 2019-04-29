;
;   This is the module coded by Antti Karttunen and used by many programs.
;   Following text applies to this module and to all other modules in this
;   package unless otherwise noted:
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
pushi   macro   stuff           ; Push Immediate macro
        mov     ax,stuff
        push    ax
        endm
;
pushadr macro   staff           ; Push Address macro
        lea     ax,staff
        push    ax
        endm
;
;
;/* Type Masks: */
;
TYPEBITS        EQU  000Ch
TAGBITS         EQU  000Fh ; Type-bits plus Compact-bit plus that surplus-bit
SEGBITS         EQU  00F0h
SEGTYPEBITS     EQU  00FCh ; Segment bits plus type bits.
WSEGTYPEBITS    EQU  FC00h ; Segment bits plus type bits.
SIZEOF_INTERNAL EQU  3
SIZEOF_EXTERNAL EQU  4
SIZEOF_CONSCELL EQU  (2 * SIZEOF_INTERNAL)
OFFSET_UPLIM    EQU  0FFE0h     ;/* Offset shouldn't go over this */
;
;                            /* bits 3 2 */
T_CONS          EQU 0000h       ; /* 0 0 */ /* Conscells & NIL */
T_SYMBOL        EQU 0004h       ; /* 0 1 */ /* Symbols */
T_INT           EQU 0008h       ; /* 1 0 */ /* Sixteen bit integers */
T_OTHER         EQU 000Ch       ; /* 1 1 */ /* Others: */
;
T_CBIT          EQU 0002h
T_GCBIT         EQU 0001h
;
