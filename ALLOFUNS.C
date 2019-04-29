
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


#include "includes.h"


TOB new_string(n)
TOB n;
{
        char *p;
        p = chkmyalloc(tob_int(n)+1);
        *p = '\0';
        return(string_tob(p));
}


TOB clistsave(x)
TOB x;
{
        extern ULI _clistsavecnt;
        register UINT n,nbytes;
        register PTR p;

        n = length(x)+1;
        nbytes = (n * ((UINT) SIZEOF_INTERNAL));
        p = myalloc(nbytes);
        if(!p)
         {
           fprintf(stderr,"\nInternal ERROR in clistsave:\n");
           fprintf(stderr,
            "Received NULL when tried to allocate space for compact list:\n");
           eprint(x);
           fprintf(stderr,"length: %d   x: 0x%lx\n",--n,x);
           prstat(stderr);
           myexit(1);
         }
        _clistsavecnt += n;

/*      movmem(tob_ptr(x),p,nbytes); */
        memcpy(p,tob_ptr(x),nbytes);
        /* I think that movmem is not a Unix function, but memcpy is
           (or is it ?). Only difference is in argument order.
         */

        return(cons_tob(p));
}

TOB tobvec_to_clist(lptr,count)
TOB *lptr;
int count;
{
        extern ULI _clistsavecnt;
        register UINT n,nbytes;
        register PTR p;
        register TOB l,result;

        n = count+1;
        nbytes = (n * ((UINT) SIZEOF_INTERNAL));
        p = myalloc(nbytes);
        if(!p)
         {
           fprintf(stderr,"\nInternal ERROR in tobvec_to_clist:\n");
           fprintf(stderr,
            "Received NULL when tried to allocate space for compact list");
           fprintf(stderr," of length: %d   lptr: 0x%lx\n",count,lptr);
           prstat(stderr);
           myexit(1);
         }
        _clistsavecnt += n;

        l = result = cons_tob(p);
        n -= 2; /* n is now number of elements - 1 */
        while(n--) { rplacac(l,*lptr++); l = rawqcdr(l); }
/* compact bits of two last NIL's should be cleared: */
        rplaca(l,*lptr); rplacc(l,0); l = rawqcdr(l);
        rplaca(l,NIL); rplacc(l,0);
        return(result);
}


/* Allocates new compact list of length n (actually n+1, (for ending nil)),
 *  and initializes each element to inielem, or if called with only one arg
 * (i.e. inielem is endmark) then they are initialized to NIL.
 * I.e. (new-clist 3) => (() () ())
 */
TOB L_new_clist(sompa,inielem) /* vlambda */
TOB sompa,inielem;
{
        register PTR p;
        register TOB l,result;
        UINT n,nbytes;

        if(!intp(sompa)) { inv2arg("new_clist",1,sompa,inielem); }
        n = tob_int(sompa);
        if(!n) { return(NIL); }
        ++n;
        nbytes = (n * ((UINT) SIZEOF_INTERNAL));

        if(endmarkp(inielem)) { inielem = NIL; }

        p = myalloc(nbytes);
        if(!p)
         {
           fprintf(stderr,"\nInternal ERROR in newclist:\n");
           fprintf(stderr,
"Received NULL when tried to allocate space for compact list of length %d.\n",
             --n);
           prstat(stderr);
           myexit(1);
         }
        _clistsavecnt += n;
        l = result = cons_tob(p);
        n -= 2; /* n is now number of elements - 1 */
        while(n--) { rplacac(l,inielem); l = rawqcdr(l); }
/* compact bits of two last NIL's should be cleared: */
        rplaca(l,inielem); rplacc(l,0); l = rawqcdr(l);
        rplaca(l,NIL); rplacc(l,0);
        return(result);
}


TOB symbolsave(s)
register BYTE *s;
{
        extern ULI _strsavecnt;
        register BYTE *p;
        register TOB result;
        register int n;

        n = (strlen(s)+1);
        n += SIZEOF_INTERNAL; /* Reserve space before print name for value */
        if(*plist_flag) /* For plist too if flag tells so */
         { n += SIZEOF_INTERNAL; }

        p = ((BYTE *) myalloc(n));
        if(!p) { saverr("symbolsave",s); }
        _strsavecnt += n;

        *(p+2) = 0; /* Clear the type byte of the plist/value location */
        p += SIZEOF_INTERNAL; /* Skip the storage reserved for value */
        if(*plist_flag) /* Skip the storage for plist too if flag tells so */
         { *(p+2) = 0; p += SIZEOF_INTERNAL; } /* Clear also this type byte */
        strcpy(p,s);      /* Copy the string */
        result = sym_tob(p);
/* Initialize symbol's value to the value contained in _NEWSYMBVALUE_
    except if it's still () then () is used: */
        setvalue(result,
         (!nilp(_NEWSYMBVALUE_) ? value(_NEWSYMBVALUE_) : NIL));
/*
        if(lispmode) { setvalue(result,_UNBOUND_); }
        else { setvalue(result,NIL); }   Initialize value to NIL */
        /* Propertylist is anyway initialized to NIL: */
        if(*plist_flag) { setplist(result,NIL); }

        return(result);
}

maybe_static saverr(fun_name,s)
BYTE *fun_name,*s;
{
        fprintf(stderr,"\n**ERROR in %s:\n",fun_name);
        fprintf(stderr,
         "Received NULL when tried to allocate space for string: %s\n",s);
        fprintf(stderr,
         "(which is %d characters long).\n",
           strlen(s));
        prstat(stderr);
        myexit(1);
}


/* See K/R page 97 */
/*
BYTE *myalloc(n) (* Allocate n bytes *)
UINT n;
{
        register TOB oldp;

        (* Make sure that object of length of n bytes
            doesn't cross 0xX000FFFF boundary: *)
        oldp = conanize(_allocp);
(* If "conanized offset" plus n is greater than 0xFFFF, then we must 
    allocate object from next "conanized segment" with offset 0000. *)
        if((((ULI) getlow(oldp)) + n) > 65535L)
         {
(* Because abstoptr wants 20-bit absolute address as long integer, this
    means same as metanotation abstoptr(0x<X+1>0000000): *)
           oldp = ((TOB) abstoptr(0,(getsegbits(((ULI) oldp))+1)));
         }

        _allocp = ptradd(oldp,n);

        if(ptr_below(_allocp,_uplim))
         {
           return((BYTE *) oldp);
         }
        else
         {
           _allocp = ((PTR) oldp); (* So that prstat prints correct value *)
           return(NULL);
         }
}
*/

/* Don't allocate anything over this in any ?000 segment: */
#define OFFSET_UPLIM 0x0000FFE0L


BYTE *myalloc(n) /* Allocate n bytes */
UINT n;
{
        register TOB oldp;

#ifdef DEBUGGING
	BYTE smaidaflag;
	BYTE buf[12],buf2[12],buf3[12];

        smaidaflag = 0;
#endif

        /* Make sure that object of length of n bytes
            doesn't cross 0xX000FFFF boundary: */
        oldp = conanize(_allocp);
/* If "conanized offset" plus n is greater than 0xFFE0, then we must 
      allocate object from next "conanized segment" with offset 0020.
   32 bytes at both sides of segment boundary are because there once
    were certain obscure bugs.
 */
        if((((ULI) getlow(oldp)) + n) > OFFSET_UPLIM)
         {
#ifdef DEBUGGING
	   if(*debug_flag)
            {
              fprintf(stderr,
        "\nlinebuf=%s\nstrlen(linebuf): %d\n",linebuf,strlen(linebuf));
              fprintf(stderr,
        "lineptr=%s\nstrlen(lineptr): %d\n",lineptr,strlen(lineptr));
              fprintf(stderr,
               "\nn: %d   _allocp: %s   oldp: %s   ",
                 n,lto2hex(buf,_allocp,':'),lto2hex(buf2,oldp,':'));
	      smaidaflag = 1;
            }
#endif
           chainresiduememory(oldp);
           oldp &= 0xFFFF0000L; /* mask off offset */
   /* aloita seuraava segmentti vasta 32:n byten p{{st{ varmuuden vuoksi: */
	   oldp += 0x10000020L;
         }

        _allocp = ((BYTE *) (oldp + n));

        if(ptr_below(_allocp,_uplim))
         {
#ifdef DEBUGGING
           if(smaidaflag)
            {
              fprintf(stderr,"\noldp: %s  _allocp: %s   result: %s",
                lto2hex(buf,oldp,':'),lto2hex(buf2,_allocp,':'),
                 lto2hex(buf3,((BYTE *) oldp),':'));
            }
#endif
           return((BYTE *) oldp);
         }
        else
         {
           _allocp = ((PTR) oldp); /* So that prstat prints correct value */
           return(NULL);
         }
}


BYTE *chkmyalloc(n)
UINT n;
{
        register PTR p;

        if(p = myalloc(n)) { return(p); }
        else
         {
           fprintf(stderr,"\nERROR in chkmyalloc:\n");
           fprintf(stderr,
            "\nReceived NULL when tried to allocate space for %d bytes.\n",
                n);
           prstat(stderr);
           myexit(1);
         }
}


TOB consfalloc()
{
        if(nilp(_freelist))
         { _consalloc_cnt++; return(cons_tob(chkmyalloc(SIZEOF_CONSCELL))); }
        else
         {
           /* register */ TOB z;
           z = _freelist;
           _freelist = cdr(_freelist);
           return(z);
         }
}


/* This puts a memory at upper fringe of any ?000 segment (what is
    left over by myalloc) to _freelist so that it can be used at least
    for consing:
 */
chainresiduememory(ptr)
TOB ptr; /* This is understood as cons, i.e. no tagbits */
{
        TOB next;

        do {
             next = (ptr + SIZEOF_CONSCELL);
             if(((ULI) getlow(next)) > OFFSET_UPLIM) { break; }
             rplaca(ptr,NIL);
             rplacc(ptr,0); /* Zero compact bit */
/*           rplaca((ptr+SIZEOF_INTERNAL),NIL);    Unnecessary !
             rplacc((ptr+SIZEOF_INTERNAL),0);
 */
             _freelist = rplacd(ptr,_freelist);
             _residueconscnt++;
             ptr = next;
           } while(1);
}

