
/*
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
*/

typedef unsigned long int ULI;
typedef unsigned int      UINT;
typedef unsigned char     BYTE;
typedef void             *PTR;

/* Pointer to Function returning Int:
 *  (see K&R pages 114-116,141,195 and two last lines of page 209) */
typedef int (*PFI)();

/* Pointer to function returning ULI */
typedef ULI (*PFULI)();

/* Pointer to Function returning String: (e.g. char *something) */
typedef BYTE *(*PFSTR)();

#define EMPTY_STRING ((BYTE *) "")

/* See "The Compiler" manual of Aztec-C, pages cc. 43-44 */
#ifndef NULLFP
#define NULLFP ((int (*)()) 0)
#endif
 
#ifndef TRUE
#define TRUE  1
#endif
 
#ifndef FALSE
#define FALSE 0
#endif

#define LARGEDATAP (sizeof(void *) == 4)
#define LARGECODEP (sizeof(PFI)    == 4)


#define to_uint(X)  ((UINT) (X))
#define uint(X)     to_uint(X)
#define to_uli(Z)   ((ULI) (Z))                 /* Handle as ULI */
#define uli(Z)      to_uli(Z)
#define lduli(X) (LARGEDATAP ? uli(X) : (X)) /* Handle as ULI only if LD on */

/* "Convert" character pointer to byte pointer */
#define byteptr(X)  ((BYTE *) (X))

/*
These macros add or subtract unsigned 16 bit integer only from low word
 of double word, i.e. from offset part of long pointer. So segment part
 is NOT AFFECTED in anyway.

void *x;  32 bit long pointer
uint y;   unsigned 16 bit integer
*/

#define addtolow(x,y)   plus(x,uli(y))
#define subfromlow(x,y) difference(x,uli(y))

#define inverse_byte(BOORIA)  ((BOORIA) ^ 0xFF)

#define abs(a)   (((a) < 0) ? (-(a)) : (a))
#define min(a,b) (((a) < (b)) ? (a) : (b))
#define max(a,b) (((a) > (b)) ? (a) : (b))
#define LASTCHAR(X) (*((X)+strlen(X)-1))

/* See str_funs.c: */
char *fun_index_gen(),*fun_rindex_gen();
#define fun_index(S,F)       fun_index_gen((S),(F),0)
#define fun_rindex(S,F)      fun_rindex_gen((S),(F),0)
#define fun_index_not(S,F)   fun_index_gen((S),(F),1)
#define fun_rindex_not(S,F)  fun_rindex_gen((S),(F),1)

/* All characters are of type F if finds no character which is not of that
    type: */
#define all_charsp(S,F)      (!fun_index_not(S,F))

/* is  C  '0' - '7' ? */
#define isoctdigit(C) (((C) & ~7) == 060) /* 060 = 0x30 = 48. = '0' */

/* This returns appropriate end-delimiter, when beginning-delimiter character
    is given as argument. I.e. ( ), < >, { }, [ ], all other characters
    are returned intact:
 */
#define get_end_delimiter(c) (((c) == '(') ? ')' :\
 ((((c) == '<') || ((c) == '[') || ((c) == '{')) ? ((c) + 2) : (c)))


/* For some mysterious reason Aztec-C compiler goes haywire when it
    tries to translate these macros. It produces code like MOV ES,ES
     and then assembler won't like that.
   So use only macro LASTCHAR.
#define GETLASTCHAR(X) ((*(X)) ? *((X)+strlen(X)-1) : *(X))
#define SETLASTCHAR(X,Y) ((*(X)) ? (*((X)+strlen(X)-1) = (Y)) : *(X))
 */

/* Moved from lists.h at 6-6-1989: */
#define getlow(X)  ((UINT) ((ULI) (X)))
#define gethigh(X) ((UINT) (((ULI) (X)) >> 16))

#define getlowbyte(X)   ((X) & 0xFF)
#define gethighbyte(X)  (((UINT) (X)) >> 8)

/*
 * Index must be unsigned int so that logical shift is used instead
 *  of arithmetic one, so that vacated bits are filled by zero, and not
 *   by bit-15. (see K&R page 189 7.5  and page 184 6.5)
 * (To make sure it really is handled like unsigned, it is explicitly cast.)
 * New versions for bytesized arrays at 6-6-89. There are two arguments
 *  for macros: base, which is BYTE-array (each element 8 bits), and
 *  index, which is 16-bit index for bits.
 *
 * testbit   tests whether bit is on or off, and returns 0 if it is off,
 *            and non-zero if it is on.
 * getbit    is like testbit except it returns always 1 if bit is on.
 * setbit    sets bit on.
 * clrbit    clears bit (sets it off).
 *
 * Note that zeroth bit is bit-0 of zeroth byte,
 *  1st bit is bit-1 of zeroth byte, 7th bit is bit-7 of zeroth byte
 *  and 8th bit is bit-0 of 1st byte.
 * Example:
 *      BYTE bit_table[8192]; (* Table for 65536 bits *)
 *
 *      setbit(bit_table,12345); (* Sets 12345th bit in table (0th = first) *)
 */

#define poweroftwo(x)  (1 << (x)) /* Returns 2^x (one shifted left x times) */

/* Old ones for sixteen-bit integers: 
#define getbit(base,index)\
         (((*((base)+(((unsigned int) (index)) >> 4))) >> ((index) & 017)) & 1)
Or:
#define getbit(base,index)\
         (!!((*((base)+((index) >> 4))) & (1 << ((index) & 017))))

#define setbit(base,index)\
    ((*((base)+(((unsigned int) (index)) >> 4))) |= poweroftwo((index) & 017))
 */


#define testbit(base,index)\
 ((*((base)+(((UINT) (index)) >> 3))) & poweroftwo((index) & 7))

#define getbit(base,index) (!!testbit(base,index))

#define setbit(base,index)\
 ((*((base)+(((UINT) (index)) >> 3))) |= poweroftwo((index) & 7))

#define clrbit(base,index)\
 ((*((base)+(((UINT) (index)) >> 3))) &= ~poweroftwo((index) & 7))

/* This compelements bit in bit table, and returns the old value of bit.
    Argument aux is for keeping value of bit-mask, so that it is not
     computed two times, because this stupid compiler knows nothing
     about "common subexpression elimination".
 */
#define test_et_complbit(base,ind,aux) (((aux) = poweroftwo((ind) & 7)),\
 (!(((*((base)+(((UINT) (ind)) >> 3))) ^= (aux)) & (aux))))

extern int _hrflag; /* Flag to tell whether HiRes is turned on (my own) */

#ifndef EXITHR
#define EXITHR { mode(2); _hrflag = 0; }
#endif

#ifndef myexit
#define myexit(EC) { if(_hrflag) { getchar(); EXITHR; } exit(EC); }
#endif

#ifndef maybe_static
#define maybe_static      
#endif
