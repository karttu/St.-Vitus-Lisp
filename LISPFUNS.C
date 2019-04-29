
/*
;
;   This module belongs to St. Vitus' Lisp which is the Lisp Interpreter
;   for the MS-DOS machines.
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
*/

#include "linclude.h"

/* Obsolete 
TOB L_length(lista)
TOB lista;
{
        return(int_tob(length(lista)));
}
 */


TOB switch_compact_bit(x,y)
TOB x,y;
{
        rplacc(x,!nilp(y));
        return(x);
}

/* This is unnecessary now: (use setsubtype function instead!)
TOB xbytepair(x)
TOB x;
{
        return(bytepair_tob(tob_int(x)));
}
 */

/* This returns subtype of the x which should be the TOB integer */
TOB L_subtype(x)
TOB x;
{
        return(int_tob(subtype(x)));
}


TOB L_setsubtype(x,y)
TOB x,y;
{
        return(setsubtype(x,tob_uint(y)));
/* Old bullshit:
        TOB z;

        z = x;
        if(nonnilsymbolp(x)) { z = value(x); }

        if(!intp(z)) { inv2arg("setsubtype",1,x,y); }
        y = setsubtype(z,tob_uint(y));
        if(x != z) { setvalue(x,y); }
        return(y);
 */
}


TOB pick(n)
TOB n;
{
        return(pickl(_framepointer+(tob_uint(n) << 2)));
}


/* ------------------------------------------------------------------------ */
/* arithmetic functions: */

/* These are now in ARITHMET.ASM: (commented out) */
/*

TOB add1(x)
TOB x;
{
        return(int_tob(tob_int(x)+1));
}

TOB sub1(x)
TOB x;
{
        return(int_tob(tob_int(x)-1));
}

TOB plus(x,y)
TOB x,y;
{
        return(int_tob(tob_int(x) + tob_int(y)));
}

TOB difference(x,y)
TOB x,y;
{
        return(int_tob(tob_int(x) - tob_int(y)));
}

TOB times(x,y)
TOB x,y;
{
        return(int_tob(tob_int(x) * tob_int(y)));
}

TOB quotient(x,y)
TOB x,y;
{
        return(int_tob(tob_int(x) / tob_int(y)));
}

TOB remainder(x,y)
TOB x,y;
{
        return(int_tob(tob_int(x) % tob_int(y)));
}

*/


TOB vplus(args) /* vlambda */
TOB args;
{
        register TOB *argp;
        register UINT result;

        result = 0;
        argp = &args;

        while(!endmarkp(*argp))
         {
           result += tob_uint(*argp);
           argp++;
         }
        return(int_tob(result));
}


TOB vtimes(args) /* vlambda */
TOB args;
{
        register TOB *argp;
        register UINT result;

        result = 1;
        argp = &args;

        while(!endmarkp(*argp))
         {
           result *= tob_uint(*argp);
           argp++;
         }
        return(int_tob(result));
}



/* ------------------------------------------------------------------------ */
/*  Predicates: */

/* Most of these were modified 17. AUG 1991 so that they return their
   argument intact when it matches to that type class (or other condition),
   and () when it doesn't. (Old way, and usually a standard lisp way, is to
   return t or () always).
   Of course this applies only to those type classes which doesn't contain
   () in them. E.g. listp and symbolp must return () or t because they
   match to () also.
 */

TOB L_atom(x)
TOB x;
{
        return(T_OR_NIL(atom(x)));
}

/* Is this correct ? Hmm.... */
TOB L_boundp(x)
register TOB x;
{
        return(T_OR_NIL(nonnilsymbolp(x) && !eq(value(x),_UNBOUND_)));
}


TOB L_compactp(x)
TOB x;
{
/*      return(T_OR_NIL(compactp(x))); */
        return(compactp(x) ? x : NIL);
}


TOB L_consp(x)
TOB x;
{
/*      return(T_OR_NIL(consp(x))); */
        return(consp(x) ? x : NIL);
}


TOB L_endmarkp(x)
TOB x;
{
        return(T_OR_NIL(endmarkp(x)));
}

/* In FUNDAMEN.ASM:
TOB L_eq(x,y)
TOB x,y;
{
        return(T_OR_NIL(eq(x,y)));
}
 */

TOB L_equal(x,y)
TOB x,y;
{
        return(T_OR_NIL(equal(x,y)));
}


/* Now handles x & y as unsigned integers */
TOB L_greaterp(x,y)
TOB x,y;
{
/*      return(T_OR_NIL(tob_uint(x) > tob_uint(y))); */
        return((tob_uint(x) > tob_uint(y)) ? x : NIL);
}


/* Now handles x & y as unsigned integers: */
TOB L_lessp(x,y)
TOB x,y;
{
/*      return(T_OR_NIL(tob_uint(x) < tob_uint(y))); */
        return((tob_uint(x) < tob_uint(y)) ? x : NIL);
}


TOB L_intp(x)
TOB x;
{
/*      return(T_OR_NIL(intp(x))); */
        return(intp(x) ? x : NIL);
}



/*
If x is negative integer (bit-15 on), then return it, otherwise return NIL:
 */
TOB L_minusp(x)
TOB x;
{
        return((intp(x) && (tob_int(x) < 0)) ? x : NIL);
}



/*
If x is positive integer (bit-15 zero), then return it, otherwise return NIL:
 */
TOB L_plusp(x)
TOB x;
{
        return((intp(x) && (tob_int(x) >= 0)) ? x : NIL);
}


TOB L_listp(x)
TOB x;
{
        return(T_OR_NIL(listp(x)));
}

TOB L_neq(x,y)
TOB x,y;
{
        return(T_OR_NIL(nilp(L_eq(x,y))));
}


TOB L_nilp(x) /* nilp, not, null */
TOB x;
{
        return(T_OR_NIL(nilp(x)));
}


TOB L_nonnilsymbolp(x)
TOB x;
{
/*      return(T_OR_NIL(nonnilsymbolp(x))); */
        return(nonnilsymbolp(x) ? x : NIL);
}

TOB L_otherp(x)
TOB x;
{
/*      return(T_OR_NIL(otherp(x))); */
        return(otherp(x) ? x : NIL);
}

TOB L_bcdp(x)
TOB x;
{
        return(bcdp(x) ? x : NIL);
}

TOB L_portp(x)
TOB x;
{
        return(portp(x) ? x : NIL);
}


TOB L_symbolp(x)
TOB x;
{
        return(T_OR_NIL(symbolp(x)));
}

TOB L_stringp(x)
TOB x;
{
/*      return(T_OR_NIL(stringp(x))); */
        return(stringp(x) ? x : NIL);
}

/* Nonnilsymbol or string: */
TOB L_gen_stringp(x)
TOB x;
{
/*      return(T_OR_NIL(gen_stringp(x))); */
        return(gen_stringp(x) ? x : NIL);
}

TOB get_endmark()
{
        return(ENDMARK);
}


/* In FUNDAMEN.ASM:
TOB L_zerop(x)
TOB x;
{
        return(T_OR_NIL(eq(x,ZERO)));
}
 */


/* ------------------------------------------------------------------------ */



/* Tarkistaa onko item listassa, ja jos on niin palauta numero
 *  joka osoittaa kuinka mones se on listassa (0 = eka, 1 = toka, etc.)
 * Jos taas item ei ole listassa niin palautetaan NIL.
 * Esim:
 *  (memqnth 'a '(a b c d e))  --> 0
 *  (memqnth 'd '(a b c d e))  --> 3
 *  (memqnth 'Z '(a b c d e))  --> ()
 */

/*
   This is unnecessary, because there is identical function nthmemq
   in module lists2.c ! Use that.
TOB memqnth(item,lista)
TOB item;
register TOB lista;
{
        int num;

        num = 0;

        while(!nilp(lista))
         {
           if(eq(car(lista),item)) { return(int_tob(num)); }
           lista = cdr(lista);
           num++;
         }

        return(NIL);
}
*/

/* Seconds since 00:00 or 12:00, because there is 86400 seconds in the day,
    and that doesn't fit to sixteen bits. */
UINT seconds_of_day()
{
        struct tm buf;
        UINT z;

        dostime(&buf);
        
        z = (buf.tm_hour);
        if(z >= 12) { z -= 12; }
        z *= 3600; /* 3600 seconds in one hour */
        z += ((60 * (buf.tm_min)) + (buf.tm_sec));
        return(z);
}

/* Seconds since beginning of this year */
ULI seconds_of_year()
{
        struct tm buf;

        dostime(&buf);
        
        return((buf.tm_yday * 86400L) + (buf.tm_hour * 3600L)
             + (buf.tm_min * 60) + (buf.tm_sec));
}

/* Hundredths of seconds since beginning of this year */
ULI hsecs_of_year()
{
        struct tm buf;

        dostime(&buf);
        
        return((uli((buf.tm_yday * 86400L) + (buf.tm_hour * 3600L)
             + (buf.tm_min * 60) + (buf.tm_sec)) * 100) + buf.tm_hsec);
}

TOB L_hsecs()
{
    return(int_tob(getlow(hsecs_of_year())));
}

TOB L_seconds_of_day()
{
    return(int_tob(seconds_of_day()));
}

TOB L_daytime()
{
/*      BYTE *asctime(); */ /* Defined already in time.h */
        struct tm buf;

        dostime(&buf);
        strcpy(tob_string(_STATICBUF_),asctime(&buf));
        LASTCHAR(tob_string(_STATICBUF_)) = '\0';
        return(_STATICBUF_);
}


/* These functions L_intran & L_setseed are adapted from functions
    rand & srand from the page 46 of K/R second edition:
 */

ULI next = 1;

/* Return random integer from range 0 - n-1, and from 0 to 65535 if n is zero:
 */
TOB L_intran(n)
TOB n;
{
    next = ((next * 1103515245) + 12345);

/*
 Original formula was like this, returning pseudo-random integer on 0...32767
    return (unsigned int)(next/65536) % 32768;
 I hope that this works:
 */ /* If argument is 0 then result is from range 0-65535: */
    return(zerop(n) ? int_tob(gethigh(next))
    /* Otherwise take modulo with argument: */
                    : int_tob(gethigh(next) % tob_uint(n)));
}

TOB L_setseed(seed)
TOB seed;
{
    if(!intp(seed)) { next = uli(seconds_of_day()); }
    else { next = uli(tob_uint(seed)); }

    return(int_tob(getlow(next)));
}


/* Returns random integer between 0 - n-1 */
/* This bullshit commented out: */
/*
TOB L_intran(n)
TOB n;
{
        UINT intran();

        return(int_tob(intran(tob_int(n))));
}


(* Sets seed number of the random generator.
   If no argument at all or if it's not a cons cell then uses current time,
   otherwise 32-bit integer represented in the longcell.
   Returns as result the longcell containing the 32-bit seed number,
   so that car is low word and cdr is high word.
 *)
TOB L_setseed(cell)
TOB cell;
{
    ULI setseed(),randomize();
    ULI result;

    if(!consp(cell)) { result = randomize(); }
    else
     {
       result =
         setseed(tob_uint(car(cell)) + (((ULI) tob_uint(cdr(cell))) << 16));
     }

    return(make_longcell(result,cell));
}
*/



TOB L_intern(sym_or_str)
TOB sym_or_str;
{
        return(intern(tob_string(sym_or_str)));
}


TOB L_lookup(sym_or_str)
TOB sym_or_str;
{
        return(lookup(tob_string(sym_or_str)));
}


TOB mem_free(cell)
TOB cell;
{
    return(make_longcell(lptrdiff(_uplim,_allocp),cell));
}

TOB mem_used(cell)
TOB cell;
{
    return(make_longcell(lptrdiff(_allocp,_lowlim),cell));
}


/*
   This hairy function is coded just for the hubafish.lsp.
   It works as follows:
    If y is not integer, then return NIL immediately.
    x must be integer, or name of symbol whose value is integer,
    or pick-expression whose value is integer.
    y is subtracted from the value x represents, and if x is symbol
    or pick-expression, then that is set to be a new value of x.
    If difference was positive, i.e. zero or more, then NIL is returned,
    otherwise is returned that negative result.
 */
TOB sub_while_plusp(x,y)
TOB x,y;
{
        register TOB xx;

        if(!intp(y)) { return(NIL); }

        if(!intp(xx = eval(x))) { inv2arg("sub_while_plusp",1,x,y); }

        xx = difference(xx,y);

        if(pickp(x))
         {
           return(pokeltostack((_framepointer+(tob_uint(x))),xx));
         }
        else if(nonnilsymbolp(x)) { setvalue(x,xx); }

        return((tob_int(xx) < 0) ? xx : NIL);
}


/* This returns list of the all filenames matched to pattern, if pattern
    is not string or symbol (e.g. endmark when called with no args), then
    return all the filenames in the current directory:
   (Modify this in future, so that uses wildcard matching function, and
     not the scabious matching of the MS-DOS
   And don't take directories and volume labels to the list!).
 */
TOB dir(pattern)
TOB pattern;
{
    BYTE *scdir();
    BYTE *x;
    TOB z=NIL;

    while(x = scdir(gen_stringp(pattern) ? pname(pattern) : byteptr("*.*")))
     {
       z = cons(intern(x),z);
     }

    return(z);
}


/*
   Note about functions L_argv and L_getenv !
   If lisp.exe is started so that there's no much other stuff in memory
   then it's possible and probable that both command line arguments
   and environment variables are in the first 64 K of RAM, i.e.
   their segment part is zero in conanized form. That means that
   they couldnt be distinguished from the internal function pointers
   (= bcd's), because they belong also to typeclass other, and they
   are recognized just by the fact that their segment part is zero,
   and the offset-part is direct address to function in CS.
   Of course it would be possible to use them also as conanized
   pointers, but that would slow the execution because of those
   far jumps needed.
   So I have instead edited these two functions so that they make
   the safe copy of their results with L_strdup. Note that L_strdup
   shouldn't contain any checks about the argument type !
   (or check just that it with otherp, but not with stringp !)
   Of course this means that user cannot "hack" the environment
   or command line args from the lisp.
 */

/* Returns copy of the command line argument: */
TOB L_argv(n)
TOB n;
{
    if(!intp(n)) { inv1arg("argv",n); }

/* If n is negative return argc, otherwise the requested argument: */
    if(tob_int(n) < 0) { return(int_tob(G_argc)); }
    else
     {
       register BYTE *p;

       p = *(G_argv+tob_uint(n));
  /* If p is NULL then return (), otherwise it's copied to _STATICBUF_
     except if it's longer than STATICBUFSIZE, in that case safe copy
     is made with L_strdup function: */
       if(p)
        {
          if(strlen(p) > STATICBUFSIZE) { return(L_strdup(string_tob(p))); }
          else { strcpy(tob_string(_STATICBUF_),p); return(_STATICBUF_); }
        }
       else { return(NIL); }
     }
}


/* Returns copy of the environment variable: */
TOB L_getenv(name)
TOB name;
{
    register BYTE *p;

    if(!gen_stringp(name)) { inv1arg("getenv",name); }
    p = getenv(pname(name));

  /* If p is NULL then return (), otherwise it's copied to _STATICBUF_
     except if it's longer than STATICBUFSIZE, in that case safe copy
     is made with L_strdup function: */
    if(p)
     {
       if(strlen(p) > STATICBUFSIZE) { return(L_strdup(string_tob(p))); }
       else { strcpy(tob_string(_STATICBUF_),p); return(_STATICBUF_); }
     }
    else { return(NIL); }
}



TOB shell(cmd)
TOB cmd;
{
  return(system(gen_stringp(cmd) ? pname(cmd) : getenv("COMSPEC"))
          ? NIL : _T_);
}

TOB L_sleep(seconds)
TOB seconds;
{
    register ULI start_time;

/* Replace this by sleep(tob_uint(seconds)) if you have that in
   your C's library (like in Turbo-C). But Aztec-C doesn't have
   sleep function so we must use this clumsy code.
   Note that if executed (sleep something) just before New Year
   midnight, then "sleeping" stops just at midnight because
   then seconds_of_year returns zero and zero - start_time
   produces a quite big unsigned long integer.
 */
    start_time = seconds_of_year();
    while((seconds_of_year() - start_time) < tob_uint(seconds)) { }
    return(int_tob(seconds_of_day()));
}


#define buf1len 78
#define margin  24 /* Column where values of the symbols are printed */


TOB apropos(argu)
TOB argu;
{
  TOB findsymbols(); /* If this is not defined anywhere else ? */
  int wildcard(); /* String matching function */
  register TOB lista;
  BYTE buf1[buf1len+3];

  if(endmarkp(argu))
   {
     fprintf(stderr,"\nApropos: Enter search pattern (e.g. set*): ");
     myfgets(buf1,buf1len,stdin);
   }
  else if(!gen_stringp(argu))
   {
     fprintf(stderr,
"\nApropos: argument must be a string or symbol, or no argument at all !\n");
     reset();
   }

  argu = lista =
   findsymbols((endmarkp(argu) ? buf1 : tob_string(argu)),wildcard);

  buf1[buf1len] = '\0';
 
  while(!nilp(lista))
   {
     register unsigned int len;
     TOB x;

     *buf1 = '\0'; /* Is this necessary ? */
     len = sprintnexpr((x = car(lista)),buf1,buf1len);
/* Put spaces until margin (but at least one), and keep track of length: */
     do { strcat(buf1," "); } while(++len <= margin);
     if(stringp(value(x)))
      { /* Don't show the values of strings because they can contain all
           kind of garbage characters which mess up the output: */
        strncat(buf1,"is a string.",buf1len);
      }
     else { sprintnexpr(value(x),(buf1+len),(buf1len-len)); }
 
     fputs(buf1,stdout);
     fprintf(stdout,"\n");

     lista = cdr(lista);
   }

  lista = L_length(argu); /* Get the length of the list */

  free_list(argu); /* Free the original list returned by findsymbols */

  return(lista); /* Return the number of matched symbols */
}


/* Some rudimentary help... */
TOB help()
{
  fprintf(stderr,
"\nSome useful functions:\n");
  fprintf(stderr,
"(exit)                              Exit to DOS.\n");
  fprintf(stderr,
"(shell)                             Jump to shell if there's enough memory.\n");
  fprintf(stderr,
"(shell \"some command\")              Run some command in command.com\n");
  fprintf(stderr,
"(load 'filename)                    Load a lisp program named filename.lsp\n");
  fprintf(stderr,
"(memstat)                           Show information about memory allocation.\n"
         );
/*
  fprintf(stderr,
"(deb)                               Show some debugging information.\n");
 */
/*
  fprintf(stderr,
"Set environment variable MAXMEM to the suitable value, e.g. SET MAXMEM=330000\n");
 */
  fprintf(stderr,
"(apro) or (apro \"wildcard-expr\")    Show matching symbols from *oblist*\n");
  fprintf(stderr,
"For example:\n");
  fprintf(stderr,
" (apro \"set*\")    Show all symbols (i.e. function names) beginning with set\n");
  fprintf(stderr,
" (apro \"<*>*<*>\") Show all symbols beginning and ending with asterisk,\n");
  fprintf(stderr,
"                   i.e. global variables.\n");
  fprintf(stderr,
"\nBrowse LISP.DOC for more information !\n");
  fprintf(stderr,
"St. Vitus' Lisp comes with ABSOLUTELY NO WARRANTY; for details type GPL.TXT\n\n");
  return(NIL);
}

