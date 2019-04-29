
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
 
 
#include "stdio.h"
#include "ctype.h"
#include "mydefs.h"

/* Search string s2 from string s1 and return pointer to that place where
 *  s2 starts. If s2 is not found from s1, then return NULL.
 * (this is like index, but second argument is string, not single char)
 * E.g:
 *      veba = "ankerias";
 *      sindex(veba,"eri")   -> returns veba + 3
 */

char *sindex(s1,s2)
char *s1,*s2;
{
        int streq(); /* Coded in assembly */
        char *index();
        char c;
 
        c = *s2++; /* first char of string searched for */
        if(isalpha(c)) { c = _tolower(c); goto alphaloop; }

        while(s1 = index(s1,c))
         {
           if(streq(s2,++s1)) { return(--s1); }
         }
        return(NULL);

alphaloop:

        while(*s1)
         {
           if((_tolower(*s1) == c) && isalpha(*s1))
            { if(streq(s2,++s1)) { return(--s1); } }
           else { s1++; }
         }
        return(NULL);
}

