
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



/* Converts every character of s with convfun, for example:
    convert_string(buf,toupper);
    changes every alphabetic character in buf to upper case.
 */
char *convert_string(s,convfun)
register char *s;
PFI convfun;
{
        char *orig_s;

        orig_s = s;
        while(*s) { *s = ((*convfun)(*s)); s++; }
        return(orig_s);
}


/* Gets pointer to first character in s which, when applied to testfun,
    returns non-zero. For example   fun_index(buf,f_isdigit)
    returns pointer to first digit in buf, NULL if there is
    no digits at all in buf.
 */
char *fun_index_gen(s,testfun,negate_flag)
register char *s;
register PFI testfun;
register UINT negate_flag;
{
        while(*s)
         {
           if(negate_flag ^ (!!((*testfun)(*s)))) { return(s); }
           s++;
         }
        return(NULL);
}

/* This is like previous, but start searching from the end. */
char *fun_rindex_gen(s,testfun,negate_flag)
register char *s;
register PFI testfun;
register UINT negate_flag;
{
        char *orig_s;

/*      if(!s) { return(s); } */ /* If s is NULL, return NULL immediately */
        if(!*s) { return(NULL); } /* Nope ! If s is empty, then return NULL */
        orig_s = s; /* Save the beginning of s */
        s += (strlen(s)-1); /* Get pointer to last character */

        while(1)
         {
           if(negate_flag ^ (!!((*testfun)(*s)))) { return(s); }
           if(s == orig_s) { break; }
           --s;
         }
        return(NULL);
}



/* Returns 1 if ALL the characters of s fullfil the condition for testfun.
   For example, all_charsp(buf,f_isxdigit) returns 1 if buf contains
   only hexadecimal digits, otherwise 0.
 */
/* Unnecessary because !fun_index_not(s,testfun) is effectively the same.
    (defined as macro in mydefs.h)

int all_charsp(s,testfun)
register char *s;
PFI testfun;
{
        while(*s)
         {
           if(!((*testfun)(*s))) { return(0); }
           s++;
         }
        return(1);
}

*/


/* Look if there's any a in s, if is then substitute it with b and put
    result into z.
 */
int subst_string(z,s,a,b)
char *z,*a,*b;
register char *s;
{
	int cases=0;

        while(*s)
         {
           if(streq(a,s))
            {
              strcpy(z,b);
              s += strlen(a);
              z += strlen(b);
              cases++;
            }
           else { *z++ = *s++; }
         }

        *z = '\0'; /* Put the final zero. */

        return(cases);
}


/* Substitute every c1 from s to c2: */
int substchars(s,c1,c2)
register char *s,c1,c2;
{
        int cases=0;

        if(!c2) { return(delchars(s,c1)); }

        while(*s) { if(*s == c1) { *s = c2; cases++; } s++; }
        return(cases);
}


/* Delete every c1 from s: */
int delchars(s,c1)
register char *s,c1;
{
        int cases=0;

        while(*s)
         {
           if(*s == c1) { strcpy(s,(s+1)); cases++; }
           else { s++; }
         }

        return(cases);
}


/* Function versions of character testing macros in ctype.h.
   First check in all that c is seven bits, because ctp_ table is
   only 128 long, and if indexed with something bigger then
   unexpected truth values may result.
 */

int f_isalpha(c)
int c;
{ return(isascii(c) && isalpha(c)); }

int f_isupper(c)
int c;
{ return(isascii(c) && isupper(c)); }

int f_islower(c)
int c;
{ return(isascii(c) && islower(c)); }

int f_isdigit(c)
int c;
{ return(isascii(c) && isdigit(c)); }

int f_isxdigit(c)
int c;
{ return(isascii(c) && isxdigit(c)); }

int f_isalnum(c)
int c;
{ return(isascii(c) && isalnum(c)); }

int f_isspace(c)
int c;
{ return(isascii(c) && isspace(c)); }

int f_ispunct(c)
int c;
{ return(isascii(c) && ispunct(c)); }

int f_iscntrl(c)
int c;
{ return(isascii(c) && iscntrl(c)); }

int f_isprint(c)
int c;
{ return(isascii(c) && isprint(c)); }

int f_isgraph(c)
int c;
{ return(isascii(c) && isgraph(c)); }

int f_isascii(c)
int c;
{ return(isascii(c)); }

int f_toascii(c)
int c;
{ return(toascii(c)); }


#ifndef isoctdigit
#define isoctdigit(C) (((C) & ~7) == 060) /* 060 = 0x30 = 48. = '0' */
#endif

int f_isoctdigit(c)
int c;
{ return(isoctdigit(c)); }

