
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

/* Warning! This module contains some weird kludges for KANJIDIC ! */


int strcmp();
static PFI cmpfun=strcmp;

int monocase_streq(),prefix_streq();
PFI abrevmatchfun = prefix_streq;


#define save_it(S)    symbolsave(S)

/* Symbol table scheme: (by AK, meditated at winter 1989)
 * There is two lists: _oblist, which is normal list (not compact)
 *   and _obtable which is compact-list of length _OBTAB_SIZE_ 
 *  (i.e. one-dimensional array).
 * The _obtable contains _OBTAB_SIZE_ buckets (which are initially all nils)
 * for
 *  hash-values 0 to (_OBTAB_SIZE_-1).
 * But instead of conventional system where each bucket is separate list,
 *  elements of _obtable are just pointers to _oblist, to that location
 *   where resides first symbol with corresponding hash-value.
 * And _oblist is conventional list, its elements being all the symbols
 *  read so far. (In "hashabetical" (not alphabetical) order.)
 *  (That is, elements of _oblist are symbols, not list of symbols.)
 * If H:th bucket in _obtable is nil that means that symbols with
 *  hash-value H has not yet been encountered.
 */



/* hashpjw:
 *  hash function from "Compilers: Principles, Technique and Tools", page 436.
 *   ("the Dragon Book" by Aho, Sethi, Ullman)
 * (Originally from P.J. Weinberger's C compiler)
 */

/*
int hashpjw(s)
register BYTE *s;
{
        register ULI h,g;
 
        h = 0L;

        while(*s)
         {
           h  <<=  4;  (* h = (h << 4) + (*s); *)
           h  +=  *s;
           if(g = (h & 0xf0000000L))
            {
              h ^= (g >> 24);
              h ^= g;
            }
           s++;
         }

        return(h % _OBTAB_SIZE_);
}
 */


/* This is from the page 144 of Kernighan/Ritchie, second edition.
   (I have added _toupper so that user can change symbols from lowercase
   to uppercase or vice versa, and they are still found from the
   correct location in *oblist*  Be sure that macro _toupper doesn't
   use its argument twice!). What's the magic in the number 31 ?
   At least it's prime and 2^5 - 1...
   Hashpjw may has a better algorithm but this simple version runs
   faster in 16-bit machines. At least I think so.
 */
UINT hash(s)
register BYTE *s;
{
        register UINT hashval;

        for(hashval = 0; *s;)
         { hashval = ((31 * hashval) + ((UINT) _toupper(*s++))); }
        return(hashval % _OBTAB_SIZE_);
}




TOB getnextbucket(hashval)
int hashval;
{
        register TOB l;
 
        for(l = cdr(qnthcdr(hashval,_obtable)); !nilp(l); l = cdr(l))
         {
           if(!nilp(car(l))) { return(car(l)); }
         }
        return(NIL);
}


/* Unnecessary:
changenextbucket(hashval,newbucket)
int hashval;
TOB newbucket;
{
        register TOB l;
 
        for(l = cdr(qnthcdr(hashval,_obtable)); !nilp(l); l = cdr(l))
         {
           if(!nilp(car(l))) { rplaca(l,newbucket); break; }
         }
}
 */


/* Tries to find symbol with printname s from _oblist, and if finds
    then return that symbol, otherwise return NIL.
   String s may contain various prefix & abbreviation characters.
   (which affect how it is read)
 */
TOB lookup(s)
BYTE *s;
{
        int tolower(),toupper();
        TOB handle_abbr_et_wildcards();
        TOB getnextbucket();
        register TOB l;
        TOB stopper;
        int hashval;
        BYTE buf[81];

        if((*abrev_char) && (LASTCHAR(s) == *abrev_char) &&
             (*s != *abrev_char))
         { return(handle_abbr_et_wildcards(s)); }

/* If some prefix character is used, and string s doesn't start with it
    already, then recursively try to find symbol with same print name,
     but starting with prefix character.
 */
        if(*prefix_char && (*s != *prefix_char))
         { /* However, if last char is uppercase, then don't try to find
               prefixed version. (and make string lowercase again)
            */
           if(f_isupper(LASTCHAR(s)))
            { convert_string(s,tolower); }
           else
            {
              *buf = *prefix_char;
              strcpy((buf+1),s);
              if(!nilp(l = lookup(buf))) { return(l); }
              /* If didn't find, then just continue in normal way,
                i.e. search without prefix char. in beginning. */
            }
         }

        /* If string starts with one of the "makeuppercase prefixes",
            then make string uppercase before looking it up:
         (This kludge used by KANJIDIC should be replaced with read macros !)
        if(!nilp(_TOUPPER_PREFIXES_) &&
            !nilp(memq(char_tob(*s),value(_TOUPPER_PREFIXES_))))
         { convert_string(s,toupper); }
         */

        hashval = hash(s);

        /* search the first point of _oblist where is symbol with
         *  different (i.e. bigger) hash-value:
         */
        stopper = getnextbucket(hashval);

        /* Get the first point of _oblist where is symbol
         *  with hash-value hashval: */
        l = qnth(hashval,_obtable);
        /* (If bucket (= l) is nil, then return nil) */

        while((!nilp(l)) && (l != stopper))
         { /* Check if same print name: */
           if(!strcmp(s,pname(car(l)))) { return(car(l)); }
           l = cdr(l);
         }
        return(NIL);
}



/* If there is abreviation character at the end of s, then try
    to find symbol from oblist matching it, and return that.
   If there is none or more than one matching to it, then return
    NIL or list of those ambiguous cases.
    (However, single abreviation character is understood just as
    ordinary symbol, (if symbol starts with abreviation char.))
 */
TOB handle_abbr_et_wildcards(s)
BYTE *s;
{
        char *index();
        int wildcard(),strinstring();
	TOB l;
        PFI matchfun;
        BYTE space_for_z[83];
        BYTE *z=space_for_z;

        strncpy(z,s,80);
        LASTCHAR(z) = '\0'; /* Overwrite the abbrev.char. in the end */
/* If there is wildcard characters (* or ?) in string, then use appropriate
    matching function: */
        if(index(z,'*') || index(z,'?'))
         {
/* If string is like *something* then use strinstring. Note however
    that there cannot be ?'s or *'s in between, i.e. in "something"
 */
           if((*z == '*') && (LASTCHAR(z) == '*') && (strlen(z) > 2) &&
               !index(z,'?') && !*(index((z+1),'*')+1))
/* The last cryptic expression means that if there is asterisk (*) in
    z (from second letter onward) then it should be that last asterisk. */
            {
              LASTCHAR(z) = '\0';
              ++z;
              matchfun = strinstring;
            }
           else { matchfun = wildcard; }
         }
        else { matchfun = abrevmatchfun; }

        l = findsymbols(z,matchfun);
    /* If matched just one (as it should !) then return symbol, not list: */
        if(length1p(l)) { free_cons(l); l = car(l); }
        return(l);
}


TOB intern(s)
BYTE *s;
{
        TOB lookup();
        TOB getnextbucket();
/* work is just work variable (means different things at different times): */
        register TOB work;
        TOB newsym;
        int hashval;

        if(work = lookup(s)) { return(work); } /* Symbol is already present */
        else if(*dins_flag) /* If Don't Intern New Symbols flag is on */
         { return(NIL); } /* Then don't intern it, just return () */
        else
          if(*abrev_char && (LASTCHAR(s) == *abrev_char)
                         && (*s != *abrev_char))
         /* If lookup didn't find any symbols to match this abbreviation: */
         { return(NIL); }
        else /* New entry must be created */
         {
           interncount++;
           hashval = hash(s);
           newsym = save_it(s); /* Create new symbol with print name s */
           work = qnth(hashval,_obtable); /* Get bucket (corresponding to s)*/

           if(!nilp(work))
            { /* If there is non-nil bucket then attach newsym to beginning */
              rplacd(work,cons(car(work),cdr(work))); /* ...of that bucket */
              rplaca(work,newsym);
            }
           else /* bucket is nil, so this is chronologically first */
            {  /* instance of symbol with hash-index hashval */
              work = getnextbucket(hashval);
              if(nilp(work)) /* If there is no nonnil bucket after that... */
               {          /* Then put newsym last of oblist */
                 work = list1(newsym);
                 _oblist = nconc(_oblist,work);
                 rplacx(hashval,_obtable,work); /* set that bucket */
               }
              else /* There is some nonnil bucket after that */
               { /* So 'rob' the location of first elem. of that bucket for
                  * newsym, and allocate new cons-node for that first element.
                  * (of that next bucket)
                  *  (And update (make corrections to) _obtable)
                  */
                 TOB qcu;
                 /* Allocate new cons-node to 'between': */
                 qcu = cons(car(work),cdr(work));
                 rplaca(work,newsym); /* "Rob" the location for newsym */
                 rplacd(work,qcu); /* ompele lista kokoon taas */
                 rplacx(hashval,_obtable,work);
                 rplacx(hash(pname(car(qcu))),_obtable,qcu);
                 /* Clear, eh ? */
               }
            }
           return(newsym);
         }
}



/* Returns list of those symbols of _oblist which match string s,
 *  compared with function cmpfun.
 * (which should return 0 if doesn't match, non-zero otherwise).
 * E.g: cmpfun can be wildcard or streq or some form of splatch.
 *  (It should have two arguments, which both are strings)
 */
TOB findsymbols(s,cmpfun)
BYTE *s;
PFI cmpfun;
{
        register TOB l,z;

        l = _oblist;
        z = NIL; /* Matched symbols are collected to this list */

        while(l)
         {
           if((*cmpfun)(s,pname(car(l)))) { z = cons(car(l),z); }
           l = cdr(l);
         }
        return(z);
}


int prefix_streq(s1,s2)
BYTE *s1,*s2;
{
        if(*prefix_char && 
           (*s1 != *prefix_char) &&
           (*s2 == *prefix_char))
         { s2++; }
        return(monocase_streq(s1,s2));
}


int strinstring(s1,s2)
BYTE *s1,*s2;
{
        char *sindex();

        return(sindex(s2,s1) != NULL);
}



/* Just debugging ! */
/*
dump_qps()
{
        register int i;
        register TOB tmp;

        i = 0xDADA; (* Marker for debugger *)

        fprintf(stdout,"\nlength(_oblist): %d   length(_obtable): %d\n",
                 length(_oblist),length(_obtable));

        for(i=0; i < _OBTAB_SIZE_; i++)
         {
           tmp = qnth(i,_obtable);
           fprintf(stdout,"%3d: %6d   ",i,length(tmp));
           print(car(tmp));
         }
        fprintf(stdout,"\n\ni = %d\n\n",i);
}
*/




