
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
#include "mydefs.h"

static char *_allerrmsg =
"\nERROR in %salloc: received NULL when tried to allocate %d items of size %d\n";
static char *_moremsg =
"Implication: Space exhausted, compile the program with the large data option.\n";


PFI _addr_of_prstat=NULLFP;

#define call_prstat (_addr_of_prstat ? ((*_addr_of_prstat)(stderr)) : 7)

/* Total count of chars (= bytes) allocated with strsave: */
ULI _strsavecnt = 0L;

/* allocate n characters, and return pointer to first of them */
char *challoc(n)
register int n;
{
        char *calloc();
        register char *z;
        
        if(z = ((char *) calloc(n,sizeof(char))))
           { return(z); }
        else
           {
             fprintf(stderr,_allerrmsg,"ch",n,sizeof(char));
             fputs(_moremsg,stderr);
             call_prstat;
             myexit(1);
           }
}

/* K/R  page 103 (little modified)   --   (added 27-Sep-1988) */
char *strsave(s)
char *s;
{
        char *calloc();
        register char *p;
        register int n;
        
        n = strlen(s);
        n++;
        p = ((char *) calloc(n,sizeof(char)));
        _strsavecnt += n;
        if(!p)
         {
           fprintf(stderr,"\nInternal ERROR in strsave:\n");
           fprintf(stderr,
            "Received NULL when tried to allocate space for string: %s\n",s);
           fprintf(stderr,
            "(which is %d characters long). _strsavecnt: %ld. / 0x%lx\n",
              --n,_strsavecnt,_strsavecnt);
           fprintf(stderr,"%s",_moremsg);
           call_prstat;
           myexit(1);
         }
        strcpy(p,s);
        return(p);
}
        

/* allocate n integers, and return pointer to first of them */
int *ialloc(n)
register int n;
{
        char *calloc();
        register int *z;
        
        if(z = ((int *) calloc(n,sizeof(int)))) { return(z); }
        else
           {
             fprintf(stderr,_allerrmsg,"i",n,sizeof(int));
             fputs(_moremsg,stderr);
             call_prstat;
             myexit(1);
           }
}
        

/* allocate n general pointers, and return pointer to first of them */
/* example:
 *           void **palloc();
 *           int **p1;
 *           p1 = (int **) palloc(6);
 */
void **palloc(n)
register int n;
{
        char *calloc();
        register void *z;
        
        if(z = ((void **) calloc(n,sizeof(void *)))) { return(z); }
        else
           {
             fprintf(stderr,_allerrmsg,"p",n,sizeof(void *));
             fputs(_moremsg,stderr);
             call_prstat;
             myexit(1);
           }
}




/* allocate n general items of size of variable size,
 *  and return pointer to first of them.
 * This is like calloc but with check.
 */
void *galloc(n,size)
register int n;
int size;
{
        char *calloc();
        register void *z;
        
        if(z = ((void *) calloc(n,size)))
           { return(z); }
        else
           {
             fprintf(stderr,_allerrmsg,"g",n,size);
             fputs(_moremsg,stderr);
             call_prstat;
             myexit(1);
           }
}

