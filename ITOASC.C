
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


/* Convert integer x to (always-) three-digit-long ascii octal-number */
/* x should be in range 0-511, i.e. octal 000-777 */
/* Example:
 *          char *itoct3();
 *          char some_space[4];
 *          int octopus = 511;
 *          printf("octopus is now: %s\n",itoct3(some_space,octopus));
 */
char *itoct3(z,x)
char *z;
register int x;
{
	int strlen();
        char tmpspc[6];
        
        /* If still bits on when masked off 9 lowermost: */
        if(x & ~0777)
           {
             fprintf(stderr,
              "\nERROR in itoct3: x is %oQ (= %d.)\n",
              x,x);
             strcpy(z,"ERR");
             return(z);
           }
        
        sprintf(tmpspc,"00%o",x);
        
        strcpy(z,&tmpspc[(strlen(tmpspc)-3)]);
        
        return(z);
}


static char *tab = "0123456789ABCDEF";

char *btohex(z,x)
register char *z;
register unsigned int x;
{
        register int j=2;
        /* Only two hex-digits, because arg is (should be !) in range 0-255 */
        
        /* Generate digits in reverse order: */
        z  +=    j;
        *z  =  '\0'; /* Put the ending zero */
        while(j--) { (*--z) = *(tab+(x & 017)); (x >>= 4); }

        return(z);
}

char *itohex(z,x)
register char *z;
register unsigned int x;
{
        register int j=4; /* 4 hex-digits for 16 bits */
        
        /* Generate digits in reverse order: */
        z  +=    j;
        *z  =  '\0'; /* Put the ending zero */
        while(j--) { (*--z) = *(tab+(x & 017)); (x >>= 4); }

        return(z);
}

char *ltohex(z,x)
register char *z;
register ULI x;
{
        register int j=8; /* Eight hex-digits for long (= 32 bits = dword) */
        
        /* Generate digits in reverse order: */
        z  +=    j;
        *z  =  '\0'; /* Put the ending zero */
        while(j--) { (*--z) = *(tab+(x & ((ULI) 017))); (x >>= 4); }

        return(z);
}


/* lto2hex: Like ltohex, but puts char. separator between high and low word,
    e.g. lto2hex(buf,-2L,':')  ->  "FFFF:FFFE"
   However, if separator is '\0', then this is just like ltohex.
 */
/* char *lto2hex(z,x,separator) */
char *lto2hex(z,low_of_x,high_of_x,separator)
register char *z;
/* register ULI x; */
UINT low_of_x,high_of_x;
char separator;
{
        itohex(z,high_of_x);
        *(z+4) = separator;
        itohex((z + 4 + !(separator == '\0')),low_of_x);
/*      itohex((*(z+4) ? (z+5) : (z+4)),low_of_x); */ /* This would go too */
        return(z);
}


/* General to hex, argument is doubleword (= long), and j lowest hex-digits
    are converted to buffer z.
 */
char *gtohex(z,x,j)
register char *z;
register ULI x;
register int j;
{
        /* Generate digits in reverse order: */
        z  +=    j;
        *z  =  '\0'; /* Put the ending zero */
        while(j--) { (*--z) = *(tab+(x & ((ULI) 017))); (x >>= 4); }

        return(z);
}




/* Convert to percents a ratio x/y */
/* Result is of the form: ddd.d e.g. it is 5 characters long */
char *itoperc(z,x,y)
char *z;
int x,y;
{
        char tmpspc[10];
        char *tmpstr,*zz;
        char c;
        int  l;
        long int tmp;

        /* We don't want any division by zero errors
           (Or worse: halting of the whole PuuCee !) */
        if(!y) { strcpy(z,"000.0"); return(z); }

        /* tmp = ((((x * 10000)/y)+5)/10); */
        tmp =  (((long int) x) * 10000);
        tmp /= y;
        tmp += 5;
        tmp /= 10;

	zz = z;
        tmpspc[0] = '\0'; /* Put a marker to the start of string */
        tmpstr = &tmpspc[1];
        zz = z + 5;
        
        sprintf(tmpstr,"%ld",tmp);
        l = strlen(tmpstr);
        /* Set tmpstr to point to last char of tmpstr: */
        tmpstr = (tmpstr+(l-1));
        *zz-- = '\0'; /* Put the ending zero to result string */
        *zz-- = *tmpstr--; /* Xfer the last digit from tmpstr to result str. */
        *zz-- = '.'; /* Put the decimal point */
        /* If the ratio was zero, i.e. 0 percents, then result is "  0.0": */
        if(!(c = *tmpstr--)) { *zz-- = '0'; goto jolu; }
        else { *zz-- = c; }
        /* Continue copying digits backward, until the beginning-marker '\0'
         *  is found:
         */
        while(c = *tmpstr--) { *zz-- = c; }
jolu:   /* Pad the rest (at left) with blanko */
	while(zz >= z) { *zz-- = ' '; }
	return(z);
}


