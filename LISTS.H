
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

/* mydefs.h should have been included before this file, so that
 *  type-definitions work.
 */

/*
#define DEFAULT_LINEBUFSIZE 1025
#define SIZEOF_CLISTREADBUF 260
 */
#define DEFAULT_LINEBUFSIZE 1025
#define SIZEOF_CLISTREADBUF 101

#define STATICBUFSIZE 256 /* This is for Lisp */

#define MAXOUTCHARS 1000

#define DEFAULT_OBTAB_SIZE 251  /* This should be a prime !!! */

#define DEFAULT_MAXARGS 16 /* For vlambda */

#define MAXFLAGS 15

typedef ULI TOB; /* Typed Object (just unsigned long, 32 bits, 2 words) */

typedef TOB (*PFTOB)(); /* Pointer to Function returning TOB */

/* Anyway I think that list-system can be only compiled with small code
    memory model. See initlists function in the module LISTS1.C */
#if LARGECODEP
typedef ULI FUN_ADR;
#define ftohex(BUF,ADR) lto2hex((BUF),(ADR),':')
#else
typedef UINT FUN_ADR;
#define ftohex(BUF,ADR) itohex((BUF),(ADR))
#endif


#define NIL  ((TOB) 0)
#define ZERO int_tob(0)

#define SPECMARK (-1L)
#define ENDMARK SPECMARK
/* This can never be value of symbol or in car / cdr,
 *  because this is 0xFFFFFFFF and internal tob is only three bytes.
 * But this can be passed to/from function to signal some special condition.
 *  (because it is certainly different from any legal tob).
 */

/* Note that this macro eq returns false if you test two equivalent integers,
    which have however different printing subtypes: */
#define eq(X,Y) ((X) == (Y))

#define zerop(X) eq((X),ZERO)

#define endmarkp(X) eq((X),ENDMARK)

#define T_OR_NIL(ZYX) ((ZYX) ? _T_ : NIL)


/* If bit 1 ('compact-bit') is on in car-part of cons-cell, then cdr is
 *  gotten implicitly by adding 3 (bytes) to cons-cell. I.e.
 * cdr is that address, not contents of it as ordinarily. Contents of it
 *  is then cadr.
 * And cdr's type is of course CONS in that case.
 */


/*
 * Format of the highest byte (= segtagbyte) in TOB:
 * 1
 * 2 6 3 1
 * 8 4 2 6 8 4 2 1
 *
   7 6 5 4 3 2 1 0
  :       :   : : :
  :       : T :C:G:
  : SEG-  : Y :O:C:
  :  MENT : P :M: :
  :highest: E :P:B:
  :nybble :   :A:I:
  :       :   :C:T:
  :       :   :T: :
  :       :   : : :
 */


/* Type Masks: */


#define TYPEBITS 0x0C
#define SEGBITS  0xF0


#define SIZEOF_INTERNAL 3
#define SIZEOF_EXTERNAL 4
#define SIZEOF_CONSCELL (2 * SIZEOF_INTERNAL)

#define SEG_OFFSET     0xF000FFFFL
#define SEG_BITS       0xF0000000L
#define SEGTYPE_BITS   0xFF000000L /* Segment and Typebits mask */

                  /* bits 3 2 */

#define T_CONS   0x00  /* 0 0 */ /* Conscells & NIL */
#define T_SYMBOL 0x04  /* 0 1 */ /* Symbols */
#define T_INT    0x08  /* 1 0 */ /* Sixteen bit integers */
#define T_OTHER  0x0C  /* 1 1 */ /* Others: */
/* (Filepointers, bcd-function pointers, strings, etc.) */
/* Type of conscell is zero, because then it can be tested faster than other
    types with test-instruction:
        TEST    BYTE PTR seg_et_tag_byte_of_tob,TYPEBITS
        JNZ     not_a_conscell
 */

#define T_CBIT   0x02
#define T_GCBIT  0x01

/* Printing subtypes for integers: */
#define P_DEC       0x00 /* Normal 16 bit decimal integer, e.g. 12345 */
#define P_HEX       0x01 /* Hexadecimal, e.g. 0xFFFF */
#define P_OCT       0x02 /* Octal, e.g. 0177777 */
#define P_CHAR      0x03 /* Character, e.g. `c` */
#define P_BYTEPAIR  0x04 /* Byte pair, e.g. <255,254> */
#define P_PICK      0x05 /* Pick construct */

/* Long versions of T_* masks: */
#define M_CONS     (((ULI) T_CONS)    << 24)
#define M_SYMBOL   (((ULI) T_SYMBOL)  << 24)
#define M_OTHER    (((ULI) T_OTHER)   << 24)
#define M_INT      (((ULI) T_INT)     << 24)

#define M_DEC      (M_INT | (((ULI) P_DEC) << 28))
#define M_HEX      (M_INT | (((ULI) P_HEX) << 28))
#define M_OCT      (M_INT | (((ULI) P_OCT) << 28))
#define M_CHAR     (M_INT | (((ULI) P_CHAR) << 28))
#define M_BYTEPAIR (M_INT | (((ULI) P_BYTEPAIR) << 28))
#define M_PICK     (M_INT | (((ULI) P_PICK) << 28))


/* 'Native type' of NIL is CONS, although it is not proper cons-cell */
#define nilp(X) (!(X))

#define listp(X)          (get_type_bits(X) == M_CONS)
/* consp now coded with assembly language
#define consp(X)          (listp(X) && !nilp(X)) (* NIL is not dotted pair. *)
 */
#define consp(X)	  i_consp(X)
#define nonnilsymbolp(X)  (get_type_bits(X) == M_SYMBOL)
#define symbolp(X)        (nonnilsymbolp(X) || nilp(X))/* NIL is symbol too */
#define otherp(X)         (get_type_bits(X) == M_OTHER)
#define intp(X)           (get_type_bits(X) == M_INT)

#define atom(x)           (!consp(x)) /* All "scalars". (strings too) */

/*
   Because currently only Small-Code memory model is used,
   all binary coded functions are only 16 bits, so segment bits are zero:
   (Take care that you don't create string with segbits zero, like
   pointers to environment variables or command line arguments).
 */
#define _bcdp(x)          (!getsegbits(x))
/* Nowadays bcdp is a function defined in lists1.c:
#define bcdp(x)           (otherp(x) && _bcdp(x))
 */

#define stringp(x)        i_stringp((x))

#define gen_stringp(x)    (nonnilsymbolp(x) || stringp(x))

/* It is assumed that type of x is tested with intp before these are used:
   (these macros), but now they are obsolete:
#define decp(x)           testsegbits(x,P_DEC)
#define hexp(x)           testsegbits(x,P_HEX)
#define octp(x)           testsegbits(x,P_OCT)
#define charp(x)          testsegbits(x,P_CHAR)
#define bytepairp(x)      testsegbits(x,P_BYTEPAIR)
#define pickp(x)          testsegbits(x,P_PICK)
*/

/* Here are the newer versions which test also that x is integer: */
#define decp(x)           teststbits(x,M_DEC)
#define hexp(x)           teststbits(x,M_HEX)
#define octp(x)           teststbits(x,M_OCT)
#define charp(x)          teststbits(x,M_CHAR)
/* This is used when reading and printing lists: */
#define specharp(x)       spec_teststbits(x,M_CHAR)
#define bytepairp(x)      teststbits(x,M_BYTEPAIR)
#define pickp(x)          teststbits(x,M_PICK)

#define compactp(x) (/* consp(x) && */ test_cbit(x))

#define int_tob(x)     (M_INT | ((TOB) ((UINT) (x))))
#define ptr_tob(x)     conanize(x)
#define other_tob(x)   t_conanize(x,T_OTHER)
#define string_tob(x)  t_conanize(x,T_OTHER)
#define fp_tob(x)      t_conanize(x,T_OTHER)
#define fun_tob(x)     (M_OTHER | ((TOB) ((UINT) (x))))
#define sym_tob(x)     t_conanize(x,T_SYMBOL)
#define cons_tob(x)    conanize(x)

#define dec_tob(x)      (M_DEC  | ((TOB) ((UINT) (x))))
#define hex_tob(x)      (M_HEX  | ((TOB) ((UINT) (x))))
#define oct_tob(x)      (M_OCT  | ((TOB) ((UINT) (x))))
#define char_tob(x)     (M_CHAR | ((TOB) ((UINT) (x))))
#define spechar_tob(x)  (M_CHAR | ((TOB) (((BYTE) (x)) << 8)))
#define bytepair_tob(x) (M_BYTEPAIR | ((TOB) ((UINT) (x))))
#define pick_tob(x)     (M_PICK | ((TOB) ((BYTE) (x))))

#define tob_int(x)     ((int) getlow(x))
#define tob_uint(x)    ((UINT) getlow(x))
#define tob_char(x)    ((UINT) getlow(x))
#define tob_spechar(x) (tob_uint(x) >> 8)
#define tob_ptr(x)     ((void *) cleartagbits(x))
#define tob_other(x)   ((void *) cleartagbits(x))
#define tob_string(x)  ((BYTE *) cleartagbits(x))
#define tob_fp(x)      ((FILE *) cleartagbits(x))
#define tob_fun(x)     ((PFTOB) getlow(x))

#define symbol2string(X) string_tob(pname(X))
#define string2symbol(X) sym_tob(tob_string(X))

/*
This is kludge to speed up certain applications (like window system,
 see wputc.c) Added at 20-FEB-1990. X should be cons whose car is of type INT.
This returns pointer to that integer. Of course it works so long as
 location of that list node is not changed.
 */
#define icar_to_intptr(X)  ((int *) (X))

/* This macro tells how manyth cdr Y is from X if they both point
to the same compact list: */
#define clist_diff(X,Y)    (((X) - (Y)) / SIZEOF_INTERNAL)

#define pname(X) tob_string(X)

/* getlow & gethigh moved to mydefs.h at 6-6-89:
#define getlow(X)  ((UINT) (X))
#define gethigh(X) ((UINT) ((X) >> 16))
 */
#define gethighestbyte(X)  ((BYTE) ((X) >> 24))

#define lowbyte(X)  ((X) & 0x00FF)
#define highbyte(X) (((UINT) (X)) >> 8)

#define get_type_bits(X)   ((X) & (((ULI) TYPEBITS) << 24))
#define getsegbits(X)      (gethigh(X) >> 12)
#define cleartagbits(X)    ((X) & SEG_OFFSET)
#define getsegtagbyte(X)   (*(((BYTE *) cleartagbits(X)) + 2))
#define testsegbits(X,Y)   (((X) & SEG_BITS) == (((ULI) (Y)) << 28))
#define teststbits(X,Y)    (((X) & SEGTYPE_BITS) == (Y))
/* Used by spec_charp. Like teststbits but lowest byte must be zero: */
#define spec_teststbits(X,Y)   (((X) & (SEGTYPE_BITS | 0xFFl)) == (Y))

/* New macros added at 6-Jan-1991
int subtype(X)      ; Returns subtype 0-15 
TOB X;              ; X should be TOB-integer.

TOB setsubtype(X,Y) ; Sets subtype of X to be Y
TOB X;              ; X should be TOB-integer.
int Y;              ; Y should be subtype from 0 to 15.

Corresponding Lisp-functions are L_subtype and L_setsubtype in some module.
*/

#define subtype(X)         getsegbits(X)
/* First clear the old subtypebits (= segbits) and then or the new ones: */
#define setsubtype(X,Y)    (((X) & ~SEG_BITS) | (((ULI) (Y)) << 28))

/* test_cbit macro:
   First clear tag bits to get proper address, then handle address as it
   were byte pointer, (i.e. that +2 means bytes, not words or dwords),
   and fetch seg&tag-byte with *-operation, and test with and-operation
   whether compact-bit is on:
   Actually cleartagbits isn't needed because tagbits are in cons-cell
    anyway zero.
   Clear, Eh ?
 */

/* #define test_cbit(x) ((*(((BYTE *) cleartagbits(x)) + 2)) & T_CBIT) */
#define test_cbit(x) ((*(((BYTE *) x) + 2)) & T_CBIT)


#define declare_clist(NAME,LENGTH) char NAME[(LENGTH)*SIZEOF_INTERNAL]
#define declare_conscell(NAME)     char NAME[SIZEOF_CONSCELL]



#define rplacac(x,y) (((rplaca(x,y)) , (rplacc(x,1))))


/* raw quick cdr (for compact lists): */
#define rawqcdr(x)      ((x) + SIZEOF_INTERNAL)
#define rawqnthcdr(n,x) ((x) + (((UINT) (n)) * ((UINT) SIZEOF_INTERNAL)))

/* See LISPcraft by Robert Wilensky, pages 333 & 334, (Hunk functions) */
/*
#define cxr(NTH,CLIST) qnth((NTH),(CLIST))
#define rplacx(INDEX,CLIST,EXPR) rplaca(qnthcdr((INDEX),(CLIST)),(EXPR))
 */
#define cxr(NTH,CLIST) car(rawqnthcdr((NTH),(CLIST)))


#define list1(x)        cons((x),NIL)
#define list2(x,y)      listn((x),(y),ENDMARK)
#define list3(x,y,z)    listn((x),(y),(z),ENDMARK)
#define list4(a,b,c,d)  listn((a),(b),(c),(d),ENDMARK)

#define clist1(x)       list1(x)
#define clist2(x,y)     clistn((x),(y),ENDMARK)
#define clist3(x,y,z)   clistn((x),(y),(z),ENDMARK)
#define clist4(a,b,c,d) clistn((a),(b),(c),(d),ENDMARK)

#define length(X)       tob_int(L_length(X))

/* Is X list, whose length is 1 ? */
#define length1p(X)     (consp(X) && nilp(cdr(X)))



#define new_clist(N) L_new_clist(int_tob(N),NIL)

#define list_insert(X,Y,N) L_list_insert((X),(Y),int_tob((N)))
#define maprplaca(FUN,LISTA) L_maprplaca(fun_tob((FUN)),(LISTA))

#define delnth(N,LISTA) L_delnth((N),(LISTA))

/*
These often (?) used macros are now codes as functions in lists2.c,
 mainly for making code more compact, and also safer in two first cases:


#define rplacx(INDEX,CLIST,EXPR) rplaca(rawqnthcdr((INDEX),(CLIST)),(EXPR))

#define prevcdr(x)      ((x) - SIZEOF_INTERNAL)

#define caar(x)   car(car(x))
#define cadr(x)   car(cdr(x))
#define cdar(x)   cdr(car(x))
#define cddr(x)   cdr(cdr(x))

#define nconc(list1,list2) L_nconc(list1,list2,ENDMARK)

#define terpri(FP)     fprintf((FP),"\n")
#define printexprnl(X,Y)  { printexpr((X),(Y)); terpri((Y)); }

#define print(EXPR)   L_print((EXPR),ENDMARK)
#define eprint(EXPR)  L_print((EXPR),fp_tob(stderr))

 */


#define inv1arg(FUN_NAME,ARG)\
 _inv1arg((FUN_NAME),(ARG),get3retadr(),get2retadr(),getretadr())
#define inv2arg(NAME,NTH,ARG1,ARG2)\
 _inv2arg((NAME),(NTH),(ARG1),(ARG2),get3retadr(),get2retadr(),getretadr())


#define princ(EXPR)   L_princ((EXPR),ENDMARK)
#define eprinc(EXPR)  L_princ((EXPR),fp_tob(stderr))

#define prin1(EXPR)   L_prin1((EXPR),ENDMARK)
#define eprin1(EXPR)  L_prin1((EXPR),fp_tob(stderr))


/* This shit is needed by initlists & prstat: */

#define getheap() (sbrk(0))
/*
#define SDEFAULTSPACE   31000
#define LDEFAULTSPACE   0x00024000L
#define LDEFAULT_UPLIM ((PTR) 0x3D000000L)
 */
#define LDEFAULT_LISTSPACE 150000L

#if LARGEDATAP
#define ptradd(x,y)     (_ptradd((x),((long) (y))))
#define lptrdiff(x,y)   (_ptrdiff(x,y))
#define ptr_below(x,y)  (ptrtoabs(x) < ptrtoabs(y))
#else
#define ptradd(x,y)     (((char *)(x)) + (y))
#define lptrdiff(x,y)   ((long) (((char *)(x)) - ((char *)(y))))
#define ptr_below(x,y)  ((x) < (y))
#endif

void **palloc();
void *galloc();

TOB ptrtoabs(),_ptrdiff();
void *abstoptr(),*_ptradd();
void *sbrk();

#define consalloc()  consfalloc()
#define itoballoc(N) ((I_TOB *) galloc(N,SIZEOF_INTERNAL))


/* ======================================================================= */

/* Type-definitions for functions residing in assembly-language-files: */

/* arithmet.asm: */
TOB add1(),sub1(),plus(),difference(),times(),quotient(),remainder();
/* New bit-fiddling functions of ARITHMET.ASM, added 3-Jan-1991: */
TOB _sxt(),_neg(),_not(),_and(),_or(),_xor(),_shl(),_shr(),_sar();

/* lsetjmp.asm: */
TOB lsetjmp();

#define LJBUFSIZE	(12)
typedef char ljmp_buf[LJBUFSIZE];

/* conanize.asm: */
TOB conanize(),t_conanize();
UINT canonseg();

/* fundamen.asm: */
TOB car(),cdr(),rplaca(),rplacd(),q_car(),q_cdr(),q_rplaca(),q_rplacd();
TOB L_eq(),L_zerop();
int rplacc();
int i_consp(),i_stringp();
FUN_ADR getretadr(),get2retadr(),get3retadr();

/* pushargs.asm: */

UINT pushargs(),popw(),get_sp();
TOB  popl(),pickl(),pokeltostack();
void evalargs(),pushl(),pushw(),dropl(),dropnbytes(),set_sp();


/* Some common functions: */
FILE *fopen(),*myfopen();
BYTE *getenv(),*myfgets();
ULI atol();

#define value2(X)       plist(X)
#define setvalue2(X,Y)  setplist((X),(Y))




/* ======================================================================= */

/* These are from list3-17.h, kanjidic uses some of these (?) */

#define ivalue(X)     tob_int(value(X))
#define pvalue(X)     tob_ptr(value(X))
#define fvalue(X)     tob_fun(value(X))

#define setivalue(X,Y)  setvalue(X,int_tob(Y))
#define setpvalue(X,Y)  setvalue(X,ptr_tob(Y))
#define setfvalue(X,Y)  setvalue(X,fun_tob(Y))



/* Possible values of warningflag: */
#define WDISABLED          0 /* Don't check characters of symbols at all */
#define WENABLED           1 /*Check them, but not until it's set to WALERT */
#define WALERT             2 /* When printing symbols set to this */
#define WDETECTED_STRANGE  3 /* When detected something dubious */
