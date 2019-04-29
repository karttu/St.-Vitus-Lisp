
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

/* list-functions specially for Lisp, not needed in kanjidic: */


TOB L_nthcdr(num,lista)
TOB num,lista;
{
        return(nthcdr(tob_int(num),lista));
}

TOB L_nth(num,lista)
TOB num,lista;
{
        return(nth(tob_int(num),lista));
}



TOB L_qnthcdr(num,clist)
TOB num,clist;
{
        return(rawqnthcdr(tob_int(num),clist));
}

TOB L_qnth(num,clist)
TOB num,clist;
{
        return(cxr(tob_int(num),clist));
}

TOB L_rplacx(index,clist,stuff)
TOB index,clist,stuff;
{
        rplacx(tob_int(index),clist,stuff);
        /* What rplacx returns in Franz Lisp ??? */
        return(clist);
}


TOB mcl(item) /* Make circular list */
register TOB item;
{
        item = cons(item,NIL); /* Make one element list from item */
        return(rplacd(item,item)); /* And circulate it */
}


/* There is three calling formats for this member:

(member item lista)  ; Normal form, uses equal, so item can be a list also.

(member item lista :TEST testfun)
                     ; This uses testfun for testing "equality". There can
                       any non-nil expression in place of :TEST. Only NIL
                       and :TEST-NOT keyword reverse the truth value,
                       so that lista is checked to first item that _doesn't_
                       satisfy the test.

(member item testfun) ; This is same as (member item lista :TEST testfun)

*/

TOB member(elem,lista,keyword,testfun) /* vlambda */
register TOB elem;
register TOB lista;
TOB keyword,testfun;
{
        if(endmarkp(keyword)) /* Normal member */
         {
           while(!nilp(lista))
            {
              if(equal(car(lista),elem)) { return(lista); }
              else { lista = cdr(lista); }
            }
         }
        else
         {
           register UINT negate_flag;
           register TOB tmplist,cdr_of_tmplist;

           if(endmarkp(testfun)) { negate_flag = 0; testfun = keyword; }
           else { negate_flag = !!(eq(keyword,_TEST_NOT_) || nilp(keyword)); }

           /* construct tmplist only once for apply,
               and after that just rplaca second element in loop: */
           tmplist = list2(elem,car(lista));
           /* it's better to call cdr here than in loop: */
           cdr_of_tmplist = cdr(tmplist);

           while(!nilp(lista))
            { /* Utilize xor-operator (^),
                  and fact that !! returns always 0 or 1: */
              if((!!apply(testfun,tmplist)) ^ negate_flag)
               { break; }
              else { lista = cdr(lista); }
              rplaca(cdr_of_tmplist,car(lista));
            }
           free_list(tmplist);
         }
        return(lista);
}


/* Auxiliary function for get & putprop.
   Returns pointer to property list, beginning with prop.
   If there is no prop in property list, then return the
   last cons cell of that.
 */
TOB getproploc(sym,prop)
TOB sym,prop;
{
        register TOB proplist;

        /* handle disembodied property-lists too: (see Franz) */
        if(consp(sym)) { proplist = cdr(sym); }
        else { proplist = plist(sym); sym = NIL; }

        while(!nilp(proplist))
         {
           if(eq(car(proplist),prop)) { return(proplist); }
           sym = cdr(proplist);
           proplist = cdr(sym);
         }

 /* Return the last cons cell of the proplist, or NIL if sym was symbol
    and its plist was NIL: */
        return(sym);
}


TOB get(sym,prop)
TOB sym,prop;
{
    return(cadr(getproploc(sym,prop)));
}


TOB putprop(sym,propval,prop) /* Mac- & Franzlisp order */
TOB sym,propval,prop;
{
        register TOB poro,peura;

        poro = getproploc(sym,prop);
        peura = cdr(poro);
        if(!nilp(peura)) /* If there is already that property */
         {
           rplaca(peura,propval); /* Then just change it */
         }
        else /* Add a new property, i.e. concatenate
                 clist (prop propval) to the end. */
         {
           prop = clist2(prop,propval);
           if(nilp(poro)) { setplist(sym,prop); }
           else { rplacd(poro,prop); }
         }
        return(propval);
}

TOB remprop(sym,prop)
TOB sym,prop;
{
        register TOB proplist,previous;

        /* handle disembodied property-lists too: (see Franz) */
        if(consp(sym)) { previous = sym; proplist = cdr(sym); }
        else { previous = NIL; proplist = plist(sym); }

        while(!nilp(proplist))
         {
           if(eq(car(proplist),prop))
            { /* If previous is nil, then this is not disembodied one and
                  property to be removed is first one, so handle it
                   in special way: */
              if(nilp(previous))
               { setplist(sym,cddr(proplist)); }
              else { rplacd(previous,cddr(proplist)); }
              /* Palautetaan nyt jotain, katso LISPcraft, sivu 335: */
              return(cddr(proplist));
            }
           else /* Advance previous and proplist lists by two nodes: */
            {
              previous = cdr(proplist);
              proplist = cdr(previous);
            }
         }

        return(NIL);
}



TOB L_delete(exp,lista,number)
TOB exp,lista,number;
{
    return(general_delete(0,fun_tob(L_equal),exp,lista,number));
}

TOB L_delq(exp,lista,number)
TOB exp,lista,number;
{
    return(general_delete(0,fun_tob(L_eq),exp,lista,number));
}

TOB L_delete_if(testfun,lista,number)
TOB testfun,lista,number;
{
    return(general_delete(0,testfun,NIL,lista,number));
}

TOB L_delete_if_not(testfun,lista,number)
TOB testfun,lista,number;
{
    return(general_delete(1,testfun,NIL,lista,number));
}


/*
    If testfun is of typeclass other, i.e. probably bcd given
    from L_delete or L_delq (equal or L_eq) then
    deletes from list lista first number occurrences of exp,
    or all of them if exp is not given or it's NIL.

    However if testfun is something else, then it's assumed
    that it's lambda-form and it's applied to every car of
    lista at step, and if that produces non-nil answer,
    the corresponding element is removed. However, only first
    number elements are deleted, except if it's zero, then all
    of them are deleted.
    If negate_flag is 1 then semantics are negated. (When called
    by L_delete_if_not).
    If number is a normal integer or endmark then deleted list nodes
    are not freed, but if it's some special integer like hex or octal,
    or something else, then they are freed.
 */

TOB general_delete(negate_flag,testfun,exp,lista,number)
UINT negate_flag;
TOB testfun,exp,lista,number;
{
    TOB orig_lista,previous;
    TOB tmplist;
    register UINT num,free_flag;

    if(intp(number))
     {
       num = tob_uint(number);
       if(!num) { return(lista); }
       free_flag = subtype(number);
     }
    else { free_flag = !endmarkp(number); num = 0; }

    previous = NIL;
    orig_lista = lista;
 
    if(!otherp(testfun))
     {
       tmplist = cons(NIL,NIL);
     }
    else { tmplist = NIL; }

    while(!nilp(lista))
     {
       if(otherp(testfun))
        { 
           if((!!((*tob_fun(testfun))(exp,car(lista)))) ^ negate_flag)
            { goto ehto; }
        }
       else
        {
          rplaca(tmplist,car(lista));
          if((!!apply(testfun,tmplist)) ^ negate_flag)
           {
ehto:
 /* If previous is not () then just rplacd it by the cdr of lista: */
             if(!nilp(previous))
              {
 /* If free_flag is on then call free_cons(lista) otherwise just cdr(lista) */
                rplacd(previous,
                       (lista = ((free_flag ? free_cons : cdr))(lista)));
              }
 /* If previous is () and the cdr of lista is also, then it means that lista
    contained just one element, which is to be removed, so () is returned: */
             else if(nilp(cdr(lista)))
              { /* I'm not sure if freeing here is safe, so commented out: */
             /* if(free_flag) { free_cons(lista); } */
                orig_lista = NIL;
                break;
              }
             else /* Prev. was (), i.e. this is the first element in lista */
              {
                rplaca(lista,cadr(lista));
                rplacd(lista,cddr(lista));
                if(free_flag) { free_cons(cdr(lista)); }
              }

/* If num is not zero, but it comes to zero (i.e. it was 1) then stop
   deleting, because the count is fulfilled now: */
             if(num && !--num) { break; }
             else { continue; }
           }
        }

       previous = lista;
       lista = cdr(lista);
     }

    if(!nilp(tmplist)) { free_list(tmplist); }
    return(orig_lista);
}
