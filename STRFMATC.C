
#include "stdio.h"
#include "ctype.h"
#include "mydefs.h"
#include "lists.h"


/* Functions for fuzzy match, previously called splatch */

static UINT max_diffs;


TOB strfmatchp(arg1,arg2,overridemax)
TOB arg1,arg2;
/* If specified, then used as a value for maximum differences allowed: */
TOB overridemax;
{
	UINT dfmatch();
	register int d,s1l; /* s1l = s1's length */
        char *s1,*s2;

        s1 = tob_string(arg1);
        s2 = tob_string(arg2);
	
	d = ((s1l = strlen(s1)) - strlen(s2));
        if(intp(overridemax)) { max_diffs = tob_int(overridemax); }
	else 
	 { switch(s1l)
	       {
	         case 0: case 1: case 2:
		      max_diffs = 0;
		      break;
		 case 3: case 4:
		      max_diffs = 1;
		      break;
		 case 5: case 6: case 7: case 8:
		      max_diffs = 2;
		      break;
		 default:
		      max_diffs = 3;
		      break;
	       }
         }
	if(abs(d) > max_diffs) return(NIL);
	if((d = dfmatch(s1,s2,0)) > max_diffs) { return(NIL); }
	else { return(int_tob(d)); }

}


UINT dfmatch(s1,s2,diffs)
register BYTE *s1,*s2;
register UINT diffs;
{
    UINT dfmatch();

loop:

    /* If more differences than allowed: */
    if(diffs > max_diffs)  return(diffs);
    if(!*s1 && !*s2) return(diffs); /* Both at the end */
    if(!*s1) { s2++; diffs++; goto loop; } /* s1 in the end */
    if(!*s2) { s1++; diffs++; goto loop; } /* s2 in the end */
    if(*s1 == *s2) { s1++; s2++; goto loop; } /* same chars */
    else
       { register UINT d1,d2,d3,m;
         d1 = (dfmatch(s1+1,s2+1,diffs+1)); /* different chars */
         d2 = (dfmatch(s1+1,s2,diffs+1));   /* extra character in s1 */
         d3 = (dfmatch(s1,s2+1,diffs+1));   /* extra character in s2 */
         m = min(d1,d2); /* min is macro */
         return(min(m,d3));
       }
}

