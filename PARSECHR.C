
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


#define hexdigtoi(C) (isdigit(C) ? ((C) - '0') : (_toupper(C) - ('A' - 10)))

UINT hextoi();

BYTE *parse_char(res_ptr,string)
UINT *res_ptr;
BYTE *string;
{

        if(*string != '\\')
         {
           *res_ptr = *string;
           return(string+1);
         }

/* New escapes added 23.AUG.1991 \a for bell, and \v for vertical tab
    as specified in ANSI C standard. Also now recognizes hexadecimal
    character constants beginning with \x Note that \e for escape
    doesn't belong to standard.
 */
        switch(*++string)
         {
           case 'a': { *res_ptr = '\7'; break; }    /* BEL audible alert */
           case 'b': { *res_ptr = '\b'; break; }    /* BS  backspace */
           case 'e': { *res_ptr = '\033'; break; }  /* ESC escape */
           case 'f': { *res_ptr = '\f'; break; }    /* FF  form feed */
           case 'n': { *res_ptr = '\n'; break; }    /* NL (LF) newline */
           case 'r': { *res_ptr = '\r'; break; }    /* CR  carriage return */
           case 't': { *res_ptr = '\t'; break; }    /* HT  horizontal tab */
           case 'v': { *res_ptr = '\013'; break; }  /* VT  vertical tab */
           case 'x': /* There's a hexadecimal char constant \xhh */
           case 'X':
            {
              *res_ptr = hextoi(&string,++string);
              return(string);
              break;
            }
           case '^': /* AK's special control-character escapes */
/* E.g. \^A or \^a is CTRL-A (= \1)  and \^? is 63-64 = -1 = 255 */
            {
              BYTE veba;

              veba = ((BYTE) toupper(*++string));
              *res_ptr = ((BYTE) (veba - ((BYTE) 64)));
              break;
            }
           default:
            {
              UINT i,z;

/* This commented out: (i.e. if string ends to \ then nothing is
    printed for that:
              if(!*string)
               { *res_ptr = '\0'; return(string); }
   And replaced by this at 15.08.1991:
 */
              if(!*string || (*string > 126))
               { *res_ptr = *(string-1); return(string); }
 
              if(!f_isoctdigit(*string)) { *res_ptr = *string; break; }

              z = 0; i = 3;
              while(f_isoctdigit(*string) && (i--))
               {
                  z = ((z << 3) + (*string++ - '0'));
               }
              *res_ptr = z;
              return(string);
            }
         }
        return(string+1);
}


UINT hextoi(rest_ptr,string)
BYTE **rest_ptr;
register BYTE *string;
{
        register UINT x;

        x = 0;
        while(f_isxdigit(*string))
         {
           x = ((x << 4) + hexdigtoi(*string));
           string++;
         }
        *rest_ptr = string;
        return(x);
}


UINT octoi(rest_ptr,string)
BYTE **rest_ptr;
register BYTE *string;
{
        register UINT o;

        o = 0;
        while(f_isoctdigit(*string))
         {
           o = ((o << 3) + (*string++ - '0'));
         }
        *rest_ptr = string;
        return(o);
}


/* This converts some 8-bit letters with diacritic marks to normal ascii,
    where they are respresented as braces & brackets, and so on:
 */
UINT ibm_scand2asc(c)
UINT c;
{
        c = (c & 0xFF); /* Clear upper byte, if it has been sign extended. */

        switch(c)
         {
           case 129: { return('~'); }  /* u with dots */
           case 132: { return('{'); }  /* a with dots */
           case 134: { return('}'); }  /* a with circle */
           case 142: { return('['); }  /* A with dots */
           case 143: { return(']'); }  /* A with circle */
           case 148: { return('|'); }  /* o with dots */
           case 153: { return('\\'); } /* O with dots */
           case 154: { return('^'); }  /* U with dots */
           default:  { return(c); }
         }
}

/* This converts to other direction: */
UINT asc_scand2ibm(c) /* Ascii scandis to ibm-pc scandis: */
UINT c;
{
        switch(c)
         {
           case '~':  { return(129); }  /* u with dots */
           case '{':  { return(132); }  /* a with dots */
           case '}':  { return(134); }  /* a with circle */
           case '[':  { return(142); }  /* A with dots */
           case ']':  { return(143); }  /* A with circle */
           case '|':  { return(148); }  /* o with dots */
           case '\\': { return(153); }  /* O with dots */
           case '^':  { return(154); }  /* U with dots */
           default:   { return(c); }
         }
}


