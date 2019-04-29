
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


TOB fetch_char(string,index) /* vlambda */
TOB string,index;
{
        return(char_tob(*(tob_string(string) +
                (endmarkp(index) ? 0 : tob_int(index)))));
}

TOB set_char(string,character,index)
TOB string,character,index;
{
        BYTE *s;
        UINT i;

        s = tob_string(string);
        i = (endmarkp(index) ? 0 : tob_uint(index));

        *(s+i) = tob_uint(character);
        return(string);
}


TOB L_strcpy(s1,s2)
TOB s1,s2;
{
        strcpy(tob_string(s1),tob_string(s2));
        return(s1);
}

TOB L_strncpy(s1,s2,n)
TOB s1,s2,n;
{
        strncpy(tob_string(s1),tob_string(s2),tob_uint(n));
        return(s1);
}


TOB L_strcat(s1,s2)
TOB s1,s2;
{
        strcat(tob_string(s1),tob_string(s2));
        return(s1);
}

TOB L_strncat(s1,s2,n)
TOB s1,s2,n;
{
        strncat(tob_string(s1),tob_string(s2),tob_uint(n));
        return(s1);
}


TOB L_strcmp(s1,s2)
TOB s1,s2;
{
        return(int_tob(strcmp(tob_string(s1),tob_string(s2))));
}

TOB L_strncmp(s1,s2,n)
TOB s1,s2,n;
{
        return(int_tob(strncmp(tob_string(s1),tob_string(s2),tob_uint(n))));
}

TOB L_strchr(s,c) /* strchr is ANSI-C name */
TOB s,c;
{
    BYTE *veba;
    BYTE *index(); /* index is the old name of the same function */

    veba = index(tob_string(s),tob_char(c));
    return(veba ? string_tob(veba) : NIL);
}

TOB L_strrchr(s,c) /* strrchr is ANSI-C name */
TOB s,c;
{
    BYTE *veba;
    BYTE *rindex(); /* rindex is the old name of the same function */

    veba = rindex(tob_string(s),tob_char(c));
    return(veba ? string_tob(veba) : NIL);
}


TOB L_strlen(s1)
TOB s1;
{
        return(int_tob(strlen(tob_string(s1))));
}

/* Note! Don't put here any stringp check for the argument !
   (perhaps otherp check can be used, however).
   See the note for functions L_argv & L_getenv in the file LISPFUNS.C
 */
TOB L_strdup(s1)
TOB s1;
{
        return(stringsave(tob_string(s1)));
}

TOB L_strequ(s1,s2)
TOB s1,s2;
{
        return(T_OR_NIL(!strcmp(tob_string(s1),tob_string(s2))));
}

TOB L_monostrequ(s1,s2) /* *strequ */
TOB s1,s2;
{
        return(T_OR_NIL(monostrequ(tob_string(s1),tob_string(s2))));
}


TOB L_streq(s1,s2)
TOB s1,s2;
{
        return(streq(tob_string(s1),tob_string(s2)) ? s2 : NIL);
}


TOB L_monostreq(s1,s2) /* *streq */
TOB s1,s2;
{
        return(monocase_streq(tob_string(s1),tob_string(s2)) ? s2 : NIL);
}


/* (strmatchp string pattern)  Returns pattern if matches. */
TOB strmatchp(str,pat)
TOB str,pat;
{
    if(!gen_stringp(str) || !gen_stringp(pat)) { return(NIL); }

    return(wildcard(tob_string(pat),tob_string(str)) ? pat : NIL);
}


/* (*strmatchp pattern string)  Returns string if matches. */
TOB strmatchp2(pat,str)
TOB pat,str;
{
    if(!gen_stringp(str) || !gen_stringp(pat)) { return(NIL); }

    return(wildcard(tob_string(pat),tob_string(str)) ? str : NIL);
}


/* (istrmatchp string pattern)  Returns index information if matches. */
TOB istrmatchp(str,pat)
TOB str,pat;
{
    register UINT i;

    if(!gen_stringp(str) || !gen_stringp(pat)) { return(NIL); }

    return((i = wildcard(tob_string(pat),tob_string(str)))
             ? int_tob((getlowbyte(i) == 1) ? (i-1) : (i-2))
             : NIL);
}



TOB L_convert_string(string,convfun)
TOB string;
TOB convfun;
{
        register BYTE *s;
        TOB orig_s;

        orig_s = string;
        s = tob_string(string);
        while(*s) { *s = tob_char(apply(convfun,char_tob(*s))); s++; }
        return(orig_s);
}

TOB L_substchars(s,c1,c2)
TOB s,c1,c2;
{
    return(int_tob(substchars(tob_string(s),tob_char(c1),tob_char(c2))));
}

TOB L_delchars(s,c)
TOB s,c;
{
    return(int_tob(delchars(tob_string(s),tob_char(c))));
}


TOB isalphap(c)
TOB c;
{
  return((intp(c) && (tob_int(c) < 128) && isalpha(tob_int(c))) ? c : NIL);
}


TOB isupperp(c)
TOB c;
{
  return((intp(c) && (tob_int(c) < 128) && isupper(tob_int(c))) ? c : NIL);
}

TOB islowerp(c)
TOB c;
{
  return((intp(c) && (tob_int(c) < 128) && islower(tob_int(c))) ? c : NIL);
}

TOB isdigitp(c)
TOB c;
{
  return((intp(c) && (tob_int(c) < 128) && isdigit(tob_int(c))) ? c : NIL);
}

TOB isxdigitp(c)
TOB c;
{
  return((intp(c) && (tob_int(c) < 128) && isxdigit(tob_int(c))) ? c : NIL);
}

TOB isalnump(c)
TOB c;
{
  return((intp(c) && (tob_int(c) < 128) && isalnum(tob_int(c))) ? c : NIL);
}

TOB isspacep(c)
TOB c;
{
  return((intp(c) && (tob_int(c) < 128) && isspace(tob_int(c))) ? c : NIL);
}

TOB ispunctp(c)
TOB c;
{
  return((intp(c) && (tob_int(c) < 128) && ispunct(tob_int(c))) ? c : NIL);
}

TOB iscntrlp(c)
TOB c;
{
  return((intp(c) && (tob_int(c) < 128) && iscntrl(tob_int(c))) ? c : NIL);
}

TOB isprintp(c)
TOB c;
{
  return((intp(c) && (tob_int(c) < 128) && isprint(tob_int(c))) ? c : NIL);
}

TOB isgraphp(c)
TOB c;
{
  return((intp(c) && (tob_int(c) < 128) && isgraph(tob_int(c))) ? c : NIL);
}

TOB isasciip(c)
TOB c;
{
  return((intp(c) && (tob_int(c) < 128)) ? c : NIL);
}

TOB isoctdigitp(c)
TOB c;
{
  return((intp(c) && isoctdigit(tob_int(c))) ? c : NIL);
}


TOB L_tolower(c)
TOB c;
{
  return(isupper(tob_int(c)) ? char_tob(_tolower(tob_int(c))) : c);
}


TOB L_toupper(c)
TOB c;
{
  return(islower(tob_int(c)) ? char_tob(_toupper(tob_int(c))) : c);
}

/* ibm_scand2asc & asc_scand2ibm are defined in parsechr.c
   Sorry, names are somewhat inconsistent...
 */
TOB L_ibm2sevenbit(c) /* Convert ibm-scandis to 7-bit-scandis. */
TOB c;
{
  return(int_tob(ibm_scand2asc(tob_char(c))));
}

TOB L_sevenbit2ibm(c) /* And vice versa... */
TOB c;
{
  return(int_tob(asc_scand2ibm(tob_char(c))));
}



/* Default bits in {,},|,[,] and \ characters in ctp_ table */
/* This belongs to same group as those other: */
#define SCAND_DEFAULT ctp_['_'+1]
#define UAVAL         ctp_['A'+1] /* Ascii Uppercase Value */
#define LAVAL         ctp_['a'+1] /* lowercase value */

/* This sets scandinavian mode if argument is non-nil, otherwise clears it.
   Returns as result the previous mode, i.e. NIL or T
   That is, it sets bits of the some special characters (which are used
    as diacritic letters in some european languages) so that "issomething"
    macros of C, defined in ctype.h, think that they are normal letters.
 */
TOB setscandmode(flag)
TOB flag;
{
        int result;

        result = (ctp_['|'+1] != SCAND_DEFAULT);

        if(!nilp(flag)) /* Set scandinavian mode */
         {
           /* Uppercase 'special' letters: */
           ctp_['@'+1]  = UAVAL; /* Some kind of French E with accent */
           ctp_['['+1]  = UAVAL; /* A with dots */
           ctp_[']'+1]  = UAVAL; /* A with one circle (= swedish O) */
           ctp_['\\'+1] = UAVAL; /* O with dots */
           /* U with dots. This is for Germans & Estonians: */
           ctp_['^'+1]  = UAVAL;

           /* Lowercase 'special' letters: */
           ctp_['`'+1]  = LAVAL; /* Some kind of French e */
           ctp_['{'+1]  = LAVAL; /* a with dots */
           ctp_['}'+1]  = LAVAL; /* a with circle */
           ctp_['|'+1]  = LAVAL; /* o with dots */
           ctp_['~'+1]  = LAVAL; /* u with dots. */
         }
        else /* Remove scandinavian mode */
         {
           ctp_['@'+1]  = SCAND_DEFAULT;
           ctp_['['+1]  = SCAND_DEFAULT;
           ctp_[']'+1]  = SCAND_DEFAULT;
           ctp_['\\'+1] = SCAND_DEFAULT;
           ctp_['^'+1]  = SCAND_DEFAULT;

           ctp_['`'+1]  = SCAND_DEFAULT;
           ctp_['{'+1]  = SCAND_DEFAULT;
           ctp_['}'+1]  = SCAND_DEFAULT;
           ctp_['|'+1]  = SCAND_DEFAULT;
           ctp_['~'+1]  = SCAND_DEFAULT;
         }

        return(T_OR_NIL(result));
}

