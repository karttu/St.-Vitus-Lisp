
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

/* See strsave-function in K/R, page 103 */
/* Obsolete now:
TOB stringsave(s)
register char *s;
{
        extern ULI _strsavecnt;
        register char *p;
        register int n;

        n = (strlen(s)+1); (* +1 for ending '\0' *)
        p = ((char *) myalloc(n)); (* Try to allocate space *)
        if(!p) { saverr("stringsave",s); }
        _strsavecnt += n;
        strcpy(p,s);      (* Copy the string to allocated space *)
        return(string_tob(p));
}
*/


TOB stringsave(s)
char *s;
{ return(_string_save(s,1)); }

TOB temp_stringsave(s)
char *s;
{ return(_string_save(s,0)); }



TOB _string_save(s,perm_flag)
char *s;
BYTE perm_flag;
{
        UINT len;
        register TOB lista;
        TOB previous_one;

        len = strlen(s);
        previous_one = NIL;

        for((lista = _free_strings_list); !nilp(lista); (lista = cdr(lista)))
         {
           if(tob_int(value(car(lista))) >= len) { break; }
           previous_one = lista;
         }

/* There wasn't any string longer or equal to s in _free_strings_list,
    so we must allocate new string: */
        if(nilp(lista))
         {
           TOB x;
           BYTE save_plist_flag;
           /* Don't allocate plist fields for strings in any case: */
           save_plist_flag = *plist_flag; *plist_flag = 0;
           x = symbolsave(s);
           *plist_flag = save_plist_flag;
           if(perm_flag) { setvalue(x,NIL); }
           else          { setvalue(x,int_tob(len)); }
           return(symbol2string(x));
         }
        else /* Otherwise use the old string: */
         { /* Delete this string from _free_strings_list: */
           if(nilp(previous_one)) /* If this was first one */
            { _free_strings_list = cdr(_free_strings_list); }
           else { rplacd(previous_one,cdr(lista)); } /* Not first one */

           /* Copy new string upon old string: */
           strcpy(pname(car(lista)),s);
           if(perm_flag) { setvalue(car(lista),NIL); }
           return(symbol2string(car(lista)));
         }
}


/* Maybe this could be used for symbols too. But it must be noticed that
    symbols maybe allocated with plist field too (if plist flag is on),
    but strings are always allocated just with value field, so there
    should probably be another list for free'ed symbols.
 */
/* Puts string s to _free_strings_list, so that free'ed strings are
   in ascending order sorted by maximum length they have ever been.
   Notice that strings are kept in list as symbols, and are only converted
   back to strings when returned to function calling stringsave functions.
 */
TOB free_string(s)
TOB s;
{
        register TOB lista;
        TOB x,y;
        UINT len;

        if(!/*gen_*/stringp(s)) /* Don't accept symbols currently. */
         { inv1arg("free_string",s); }

        y = string2symbol(s); /* Convert string to symbol */

/* Don't free permanent strings: (they have their value field NIL) */
        if(!intp(x = value(y)))
         {
           if(nilp(x)) { return(NIL); } /* Permanent */
           else /* Invalid */
            {
              char *lto2hex();
              char buf[41];
              fprintf(stderr,"**free_string: value of y is invalid: %s\n",
               lto2hex(buf,x,':'));
              inv1arg("free_string",y);
            }
         }

        len = tob_int(x); /* Get maximum length of this string */

        /* Search _free_strings_list until string which have longer
            or equal max.length is found: */
        for((lista = _free_strings_list); !nilp(lista); (lista = cdr(lista)))
         {
           if(tob_int(value(car(lista))) >= len)
            { /* Put this string before the first longer or equilength one: */
              attach(y,lista);
              return(y);
            }
         }

        /* Else _free_strings_list was NIL or this is the longest string
            free'ed so far, so put it to last of _free_strings_list: */
/*      _free_strings_list = nconc(_free_strings_list,list1(y)); */
        _free_strings_list = putlast(_free_strings_list,y);

        return(s);
}


