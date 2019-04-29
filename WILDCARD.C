
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

/*
   Pattern can contain following characters:

    ?   Matches one character which can be anything.
    *   Matches zero or more of any characters.

    <   Start of the "group-expression", which contains some chars,
         and must end in >
        If first char after < is ^ then its semantics are negated.
        If first char after < or ^ is > then it's not understood yet as end
         delimiter.
    Examples:
        <abc>          Matches any of the letters a, b and c.
        <^0123456789>  Matches anything, except digits.
        <>>            Matches >
        <^>>           Matches anything except >

    @   Matches character last matched to ? or group-expression.
         For example ?*@ matches to all strings which begin with same
         character they end.
        However, if pattern starts with @ then it sets the group
         start & end characters, e.g. pattern: @{tuu<ba{123}pasuuna
         matches to anything which begins tuu<ba then after that is
         1, 2 or 3 and after that pasuuna.

     Any other characters match just to themselves.

   Note that unix-like [0-9] (corresponding to <0123456789>) is not
   implemented yet.
*/


#include "mydefs.h" /* For abs and min */

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE  !FALSE
#endif


char GROUP_BEG_CHAR = '<';
char GROUP_END_CHAR = '>';

unsigned char *string_org;
unsigned char last_matched=0;


/* Check whether pattern and string match, and returns 0 if they not.
   If they match then return integer whose high byte is the last character
   matched to ? or <> expression (if any), and whose low byte is
   index +2 to string, to that point which matched to the first
   non-* and non-? expression.

   Note that value of last_matched is updated in double-recursive
   function, although it is static variable.
   Intuitively, this should cause some problems, but I haven't yet
   discovered any pattern which would match incorrectly.
 */
int wildcard(pattern,string)
unsigned char *pattern,*string;
{
    UINT wild_aux();
    UINT i;

    last_matched = 0;
    string_org = string;
    if(*pattern == '@') /* Set group-expression delimiter characters. */
     { /* Set GROUP_BEG_CHAR to be char after @, and if it is not '\0' */
       if(GROUP_BEG_CHAR = *++pattern) { pattern++; } /* then skip it also. */
       GROUP_END_CHAR = get_end_delimiter(GROUP_BEG_CHAR);
     }

    i = wild_aux(pattern,string,1);
    return(i ? ((last_matched << 8)|i) : 0);

}


UINT wild_aux(pattern,string,fnwc)
register unsigned char *pattern,*string;
UINT fnwc; /* First Non-WildCard index */
{

loop:

    if(!*pattern && !*string) /* if BOTH are in the end */
     { return(fnwc); }
    /* Asterisk may match emptiness at the end of string: */
    if((*pattern == '*') && !*string)
     { pattern++; goto loop; }
    if(!*pattern || !*string) return(FALSE); /* If only OTHER is in the end */
    if(*pattern == GROUP_BEG_CHAR)
     {
       if(group_match(&pattern,&string)) { goto jalava; }
       else { return(FALSE); }
     }
    if(*pattern == '?') /* Question-mark in pattern ? */
     { pattern++; last_matched = *string++; goto loop; }
    if((*pattern == '@') && last_matched)
     {
       if(*string == last_matched) { goto silava; }
       else { goto sulava; }
     }
    if(*pattern == '*')
     {
       UINT muu,kuu;
       /* Save the value of last_matched at this level... */
       kuu = last_matched; /* if next level of recursion fucks it up. */
       if(muu = wild_aux(pattern,string+1,fnwc)) { return(muu); }
       last_matched = kuu; /* Restore value of last_matched at this level */ 
       return(wild_aux(pattern+1,string,fnwc));
#ifdef VANHA_PASKA
 /* It (*) matches several chars?: */
       return(wild_aux(pattern,string+1,fnwc)
               || /* Matches one character: (not really necessary)
              wild_aux(pattern+1,string+1,fnwc)
               ||  */             /* Or it matches 0 characters? */
              wild_aux(pattern+1,string,fnwc));
#endif
     }
    else sulava: if(*pattern == *string) /* Same characters ? */
     {
silava:
       pattern++; string++;
jalava:
       if(fnwc == 1) { fnwc = ((string - string_org)+1); }
       goto loop;
     }
    else { return(FALSE); }
}


int group_match(pat_adr,str_adr)
unsigned char **pat_adr,**str_adr;
{
        unsigned char *index();
        register unsigned char *pat;
        register unsigned char c,positive_flag;

        /* Take current char. from string, and advance string by one: */
        c = *(*str_adr)++;
        pat = (*pat_adr)+1; /* Skip group beginning char */

/* positive_flag is on if there is no negation-sign (^) in the beginning: */
        if(*pat == '^') { positive_flag = 0; pat++; }
        else { positive_flag = 1; }

        while(*pat)
         {
           if(*pat == c) /* If found c from the pattern. */
            { /* If group ending char not found, then return false: */
              if(!(pat = index((pat+1),GROUP_END_CHAR))) { return(FALSE); }
              else
               {
                 /* Set pattern to point one after group_end_char: */
nakki:           *pat_adr = (pat+1);
                 if(positive_flag) /* Set last_matched char. */
                  { last_matched = c; }
                 return(positive_flag);
               }
            }
           if(*++pat == GROUP_END_CHAR)
            {
/* If there was negation-sign (^) in the beginning, meaning that
    positive_flag was 0, then set it to 1, and jump to nakki
    to return true result. Because we are here it means that
    c doesn't match to group, so if ^ in the beginning, then
    return true.
 */           /* He he hee hee hee, some sick code again: */
              positive_flag = !positive_flag;
              goto nakki;
            }
         }

        return(FALSE); /* If no group_ending_character */
}

