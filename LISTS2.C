
/*
;
;   This module belongs to St. Vitus' Lisp which is the Lisp Interpreter
;   for the MS-DOS machines. (And is also used by other programs...)
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

/* Abridged lists2.c for kanjidic. Some functions not needed by kanjidic
    removed to lists2lp.c at 17-sep-1989
 */

#include "includes.h"

/* Some macros from lists.h, now coded as functions: */

/*
#define rplacx(INDEX,CLIST,EXPR) rplaca(rawqnthcdr((INDEX),(CLIST)),(EXPR))
 */
TOB rplacx(index,clist,expr)
UINT index;
TOB clist,expr;
{
        return(rplaca(qnthcdr(index,clist),expr));
}


/* #define prevcdr(x)      ((x) - SIZEOF_INTERNAL) */
TOB prevcdr(x)
register TOB x;
{
        TOB y;
        char *lto2hex();
        char buf[11];

        if(!consp(x)) { goto ertzu; }
        y = ((x) - SIZEOF_INTERNAL);
        /* This may catch majority of errors caused when prevcdr is
           accidentally used to go to "below" the start of compact list:
         */
        if(!(compactp(y)))
         {
           fprintf(stderr,
"**prevcdr: car of result has not compact bit on: %s\n",
            lto2hex(buf,car(y),':'));
ertzu:
           inv1arg("prevcdr",x);
         }
        return(y);
}



/* #define caar(x)   car(car(x)) */
TOB caar(x)
TOB x;
{ return(car(car(x))); }

/* #define cadr(x)   car(cdr(x)) */
TOB cadr(x)
TOB x;
{ return(car(cdr(x))); }

/* #define cdar(x)   cdr(car(x)) */
TOB cdar(x)
TOB x;
{ return(cdr(car(x))); }

/* #define cddr(x)   cdr(cdr(x)) */
TOB cddr(x)
TOB x;
{ return(cdr(cdr(x))); }


/* This just saves the sequence: MOV DX,-1    MOV AX,-1    PUSH DX    PUSH AX
 */
/* #define nconc(list1,list2) L_nconc(list1,list2,ENDMARK) */
TOB nconc(list_1,list_2)
TOB list_1,list_2;
{ return(L_nconc(list_1,list_2,ENDMARK)); }


/* When used as function there will be needed only one "\n" string instead
    of N+1 ones.
 */
/* #define terpri(FP)     fprintf((FP),"\n") */
terpri(fp)
FILE *fp;
{ fprintf(fp,"\n"); }

/* #define printexprnl(X,Y)  { printexpr((X),(Y)); terpri((Y)); } */
printexprnl(expr,fp)
TOB expr;
FILE *fp;
{
        int r;

        r = printexpr(expr,fp);
        terpri(fp);
        return(r);
}


/* #define print(EXPR)   L_print((EXPR),ENDMARK) */
TOB print(expr)
TOB expr;
{ return(L_print(expr,ENDMARK)); }


/* #define eprint(EXPR)  L_print((EXPR),fp_tob(stderr)) */
TOB eprint(expr)
TOB expr;
{ return(L_print(expr,fp_tob(stderr))); }



/* ====================================================================== */



/* Return length of lista (0 if NIL).   --   A.D. 1988 15th August */
TOB L_length(lista)
/* register */ TOB lista;
{
        TOB origlista;
        register UINT count;
        /* register */ TOB next;

        origlista = lista;
        
        if(nilp(lista)) { return(ZERO); }
        count = 0;

/*      do { */

loop:

/* If count is 0 after increasing, then it has gone from 0 to 65535 and to
    zero again, so list is obviously too long to be anything else than
    circular list. (Note that this limits length of lists to be below 65536).
 */
             if(!++count) { goto ertzu; }
             next = cdr(lista);
 /* if next is not a cons (proper), then this is the last one: */
             if(!consp(next)) { return(int_tob(count)); }
             lista = next;    /* loop to next */

             goto loop;

/*         } while(1);   Aztec-C produces so bad code for this, that I prefer
                          the old-fashioned but fast goto loop instead.
 */

ertzu:
        fprintf(stderr,"\n**length: argument is probably circular !");
	inv1arg("L_length",origlista);
}



TOB last(q)
/* register */ TOB q;
{
        TOB origlista;
        register UINT count;
        /* register */ TOB w;
        
        if(nilp(q)) return(q); /* if NIL, return NIL */

        origlista = q;
        count = 0;

        do {
             if(!++count) { goto ertzu; }
             w = cdr(q);
             /* if next is not a cons (proper), then this is the last one: */
             if(!consp(w)) { return(q); }
             q = w;    /* loop to next */
           } while(1);
ertzu:
        fprintf(stderr,"\n**last: argument is probably circular !");
	inv1arg("last",origlista);
}


/* 0th is first */
TOB nthcdr(number,q)
register int number;
register TOB q;
{
        while(number--)
              {
                if(nilp(q)) return(NIL);
                q = cdr(q);
              }
        return(q);
}



TOB nth(number,lista)
int number;
TOB lista;
{
        return(car(nthcdr(number,lista)));
}

TOB qnthcdr(number,q)
register int number;
register TOB q;
{
        if(!(consp(q) && compactp(q)))
         { inv2arg("qnthcdr",2,int_tob(number),q); }
        return(rawqnthcdr(number,q));
}


TOB dsubst(to,from,expr)
TOB to,from,expr;
{
    i_dsubst(to,from,expr);
    return(expr);
}

TOB idsubst(to,from,expr)
TOB to,from,expr;
{
    return(int_tob(i_dsubst(to,from,expr)));
}

/* Destructively changes all occurrences of "from" to "to" in all levels
    of expr. Returns the number of changes made.
 */
int i_dsubst(to,from,expr)
TOB to,from;
register TOB expr;
{
        TOB car_of_expr,cdr_of_expr;

/*
        if(equal(expr,from)) { return(to); }
        else
 */
        if(atom(expr)) { return(0); }

        car_of_expr = car(expr);
        cdr_of_expr = cdr(expr);

        if(equal(car_of_expr,from))
         {
           rplaca(expr,to);
           return(i_dsubst(to,from,cdr_of_expr)+1);
         }
/* This is little bit "misfeaturous" part, commented out!
        else if(equal(cdr_of_expr,from))
         {
           rplacd(expr,to);
           return(i_dsubst(to,from,car_of_expr)+1);
         }
 */
        else
         {
           UINT z,y;

           z = i_dsubst(to,from,car_of_expr);
           y = i_dsubst(to,from,cdr_of_expr);
           /* Returns the sum of substitutions made in lower levels: */
           return(z + y);
         }
}




TOB qnth(number,q)
register int number;
register TOB q;
{
        return(car(qnthcdr(number,q)));
}




/* Muista tehd{ vastaava vlambda-append sitten kun jaksat ! */
TOB L_nconc(args) /* vlambda */
TOB args;
{
        TOB *argptr;
        register TOB last_cell;
/*
        TOB root;

    This is source of potential bug, if rootspace is lying at segment boundary
    area, then cdr & rplacd don't work !
        declare_conscell(rootspace);

        last_cell = root = cons_tob(rootspace);
 */

        last_cell = lists2sparecons;
        /* If there is no arguments then make sure that NIL is returned: */
        rplacd(last_cell,NIL);
        argptr = &args;

        while(!endmarkp(*argptr))
         {
           rplacd(last_cell,*argptr);
/* If some of the arguments are NIL's they are just ignored:
           if(!nilp(*argptr)) { last_cell = last(*argptr); } */
/* Now all the non-cons-cell are ignored, except the last one: */
           if(consp(*argptr)) { last_cell = last(*argptr); }
           argptr++;
         }

        /* Return cdr of the "root": */
        return(cdr(lists2sparecons));
}



/*
TOB nconc(lista1,lista2)
TOB lista1,lista2;
{
        if(nilp(lista1)) { return(lista2); }
        else
         {
           rplacd(last(lista1),lista2);
           return(lista1);
         }
}
 */


TOB append(lista1,lista2)
TOB lista1,lista2;
{
        return(nconc(topcopy(lista1),lista2));
}


TOB putlast(lista,item)
TOB lista,item;
{
        return(nconc(lista,cons(item,NIL)));
}




TOB memq(atom,lista)
/* register */ TOB atom;
register TOB lista;
{
        while(lista)
         {
           if(eq(car(lista),atom)) { return(lista); }
           else { lista = cdr(lista); }
         }
        return(NIL);
}


/* This makes (plain) list of n arguments (0 or more), and returns that.
    Last argument should be ENDMARK.
 */
TOB listn(args)
TOB args;
{
        register TOB *argptr;
        TOB start;
        register TOB lista;

        argptr = &args;

        /* Well, somebody can call from lisp with (list),
           and that should return NIL:
        */
        if(endmarkp(*argptr)) { return(NIL); }

        start = lista = cons(*argptr,NIL); /* first element ... */

        while(!endmarkp(*++argptr))
         {
           rplacd(lista,cons(*argptr,NIL));
           lista = cdr(lista);
         }

        return(start);
}


/* This is like listn, but makes compact list. */
TOB clistn(args)
TOB args;
{
        register TOB *argptr;
        TOB start;
        register int count;
        register TOB lista;

        argptr = &args; /* Set argument pointer to start of arguments. */
        count = 0;

        /* Get count of arguments: */
        while(!endmarkp(*argptr++)) { count++; }
        /* Well, somebody can call from lisp with no arguments,
            as (clist), and that should return NIL:             */
        if(!count) { return(NIL); }

        argptr = &args; /* Reset argptr again */
        /* Allocate new compact list for count items: */
        start = lista = new_clist(count);
        /* Move arguments to the compact list: */
        while(!endmarkp(*argptr))
         {
           rplaca(lista,*argptr++);
           lista = cdr(lista);
         }

        return(start);
}


TOB topcopy(lista)
register TOB lista;
{
        TOB start,new,next;

        if(nilp(lista)) { return(NIL); }

        start = new = list1(car(lista));
        lista = cdr(lista);
        /* If supplied dotted pair, e.g. (a . b) then copy it: */
        if(!consp(lista)) { return(rplacd(start,lista)); }

        do {
             next = cdr(lista);
             rplacd(new,list1(car(lista)));
             new = cdr(new);
 /* if next is not a cons (proper), then this is the last one: */
             if(!consp(next))
              {/* If list ends in dotted pair, then copy that cdr part too: */
                rplacd(new,next);
                return(start);
              }
             lista = next;    /* loop to next */
           } while(1);
/* Old algorithm: (tuskaa ja hirvitysta, lujaa ihan itsestaan).
        while(lista)
         {
           rplacd(new,list1(car(lista)));
           lista = cdr(lista);
           new = cdr(new);
         }
        return(start);
 */
}


/* Make compact list copy of lista, and return that.
    If (whole) lista were compact list, then there would be quicker
    way to do this with myalloc & memcpy. (but not much)
 */
TOB ctopcopy(lista)
register TOB lista;
{
        TOB z,result,next;

        /* Allocate new compact list of same length: */
        z = result = new_clist(length(lista));

        /* Transfer atoms of lista to compact list: */
        do {
             rplaca(z,car(lista));
             next = cdr(lista);
 /* if next is not a cons (proper), then this is the last one: */
             if(!consp(next))
              {
                rplacd(z,next);
                return(result);
              }
             lista = next;    /* loop to next */
             z = cdr(z);
           } while(1);
/*
        while(!nilp(lista))
         {
           rplaca(z,car(lista));
           lista = cdr(lista);
           z = cdr(z);
         }
        return(result);
 */

}


/* This is analogous to strcpy function:
   Copy items from list src to list dst (i.e. overwrite car's of dst)
   Return dst.
 */
TOB movelist(dst,src)
register TOB dst,src;
{
        TOB orig_dst;

        orig_dst = dst;

        while(!nilp(dst) && !nilp(src))
         {
           rplaca(dst,car(src));
           src = cdr(src);
           dst = cdr(dst);
         }

        return(orig_dst);
}




/* delnth(n,lista):
 *
 * Deletes destructively n:th element from lista (which can be compact-list).
 *  0th is first.
 *  Example: delnth(2,'(a b c d e)) -> (a b d e)
 * In cases delnth(anything,()) and delnth(0,'(a)) returns NIL
 *  but DOESN'T change list physically !
 */
TOB L_delnth(num,lista)
/* register */ TOB num;
/* register */ TOB lista;
{
        TOB start;
        register UINT n;

        n = tob_uint(num);
        start = lista;

        if(nilp(lista)) { return(NIL); }

        if(!n) /* Special case, first element removed. */
         {
           if(nilp(cdr(lista))) { return(NIL); }
           rplaca(lista,cadr(lista));
           rplacd(lista,cddr(lista));
           return(lista);
         }

        while(--n && consp(lista)) { lista = cdr(lista); }
        if(!consp(lista)) { return(start); }
        rplacd(lista,cddr(lista));
        return(start);
}



/* This is buggy code, if element to be deleted is first one in
   lista or in one of its sublists, then the remaining elements
   in that (sub-)list are not examined at all.
 */
TOB deepdel(item,lista)
register TOB item,lista;
{
        TOB deepdel();
        register TOB previous,tmp,start;

        if(nilp(lista) || equal(item,lista)) { return(NIL); }

        if(equal(car(lista),item))
         { /* Special case, first element removed. */
           if(nilp(cdr(lista))) { return(NIL); }
           rplaca(lista,cadr(lista));
           rplacd(lista,cddr(lista));
           return(lista);
         }

        start = previous = lista;
        lista = cdr(lista);
        while(lista)
         {
           tmp = car(lista);
           if(equal(tmp,item))
            { /* Delete item, i.e. leave it out from the lista: */
              lista = cdr(lista);
              rplacd(previous,lista);
              continue;
            }
           else if(consp(tmp))
            { rplaca(lista,deepdel(item,tmp)); }
           previous = lista;
           lista = cdr(lista);
         }

        return(start);
}


/* Loop version would be probably faster: (What is a loop version ???) */
/* Now also recognizes physically different, but alphabetically equal
   strings as equivalent.
 */
int equal(x,y)
register TOB x,y;
{
        int equal();

        if(eq(x,y)) { return(1); } /* Should we use L_eq ? */
        if(stringp(x) && stringp(y) &&
           !strcmp(tob_string(x),tob_string(y))) { return(1); }
        if(consp(x) && consp(y))
         {
           return(equal(car(x),car(y)) && equal(cdr(x),cdr(y)));
         }
        else { return(0); }
}


/* Reverses list 'in a place', i.e. physically reverses cdr-pointers: */
TOB nreverse(lista)
register TOB lista;
{
        register TOB rev,qcu;

        rev = NIL;

        while(!nilp(lista))
         {
           qcu = cdr(lista); /* Save the cdr */
           rplacd(lista,rev);
           rev = lista;
           lista = qcu;
         }
        return(rev);
}


TOB reverse(lista)
register TOB lista;
{
        TOB result;

        result = NIL;

        while(!nilp(lista))
         {
           result = cons(car(lista),result);
           lista = cdr(lista);
         }

        return(result);
}



TOB attach(elem,lista)
TOB elem,lista;
{
        /* If lista is nil, then can't make physical attachment, so
            we must construct just single-element list from elem: */
        if(nilp(lista)) { return(cons(elem,NIL)); }

        if(compactp(lista))
         { /* compact-lists must be handled in special way */
           rplacd(lista,cons(car(lista),cons(cadr(lista),cddr(lista))));
         } /* Else it is a normal list: */
        else { rplacd(lista,cons(car(lista),cdr(lista))); }
        rplaca(lista,elem);
}


/* This function replaces all CAR's of lista with result of function fun
    applied with original CAR.
   So fun must be of form:
    TOB fun(arg)
    TOB arg;
 */
TOB L_maprplaca(fun_arg,lista)
TOB fun_arg,lista;
{
        PFTOB fun;
        register TOB l;

        fun = tob_fun(fun_arg);

        for(l = lista; !nilp(l); l = cdr(l))
         {
           rplaca(l,((*fun)(car(l))));
         }

        return(lista);
}


/* This function searches first item in lista which, given as argument
    to predicatefun, produces true-value (non-zero). Remaining list,
    with that item at head of it, is returned. (as member usually does).
   If no item which matches with predicatefun is found, then NIL is returned.
   predicatefun must be of form:
    int predicatefun(arg)        Returns zero or non-zero
    TOB arg;
 */

TOB mempred(predicatefun,lista)
register PFI predicatefun;
TOB lista;
{
        register TOB l;

        for(l = lista; !nilp(l); l = cdr(l))
         {
           if((*predicatefun)(car(l))) { return(l); }
         }

        return(NIL);
}


/* nconcs list_item into lista before n:th element. If n is zero,
    then list_item is attached to the front of lista.
 */
TOB L_list_insert(list_item,lista,n_arg)
TOB list_item,lista,n_arg;
{
        UINT n;

        n = tob_int(n_arg);

        /* If list_item is NIL then return lista intact: */
        if(nilp(list_item)) { return(lista); }

        /* If it is other atom then make list from it: */
        if(!consp(list_item)) { list_item = list1(list_item); }

        if(!n)
         {
/* Special case: attach list_item before lista, so make some hairy
    replacements. Replace first item of lista with first of list_item,
    and put rest of list_item plus first of list_item between
    first item and the rest of lista. Clear, eh ?
   I hope that this works also when lista and list_item are only one
    element long...
 */
           n_arg = car(list_item); /* Save the first of list_item to this */
           list_item =
            nconc(cdr(list_item),
                   cons(car(lista),
                         (compactp(lista) ? cons(cadr(lista),cddr(lista))
                                          : cdr(lista))
                       )
                 );
           /* Replace the first elem of lista with first elem of list_item: */
           rplaca(lista,n_arg);
           /* And after that the rest of list_item plus original first of
               lista plus the rest of lista: */
           rplacd(lista,list_item);
           return(lista);
         }

        n_arg = lista; /* Save the beginning of lista here */
        while(--n) { lista = cdr(lista); }

        if(compactp(lista))
         {
           rplacd(lista,
            nconc(list_item,cons(cadr(lista),cddr(lista))));
         }
        else { rplacd(lista,nconc(list_item,cdr(lista))); }
        return(n_arg); /* Return from the original beginning of lista */
}


/* Returns numerical index of first occurrence of item in lista, and NIL
    if it is not found at all from lista.
   0 = first, 1 = second, and so on.
 */
TOB nthmemq(item,lista)
TOB item;
register TOB lista;
{
        TOB origlista;
        register UINT count;

        origlista = lista;
        count = 0;

        while(!nilp(lista))
         {
           if(eq(item,car(lista))) { return(int_tob(count)); }
           lista = cdr(lista);
           if(!++count) { goto ertzu; }
         }

        return(NIL);
ertzu:
        fprintf(stderr,"\n**nthmemq: argument is probably circular !");
	inv2arg("nthmemq",2,item,origlista);
}


