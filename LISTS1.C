
/*
;
;   This module belongs to St. Vitus' Lisp which is the Lisp Interpreter
;   for the MS-DOS machines. This is used also by many other programs
;   which use the list-subsystem software package coded by Antti Karttunen,
;   e.g. softwares like KANJIDIC (Electronic Kanji Dictionary) and ODE11
;   (Octal Debugger & Executor for the PDP-11 code).
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

/* LISTS1.C -- Copyright (C) 1988, 1989, 1990 & 1991  Antti Karttunen
    Writing of this file started at 7. & 8. October 1988.
    This file contains most of the list-reading and printing
    functions and the lots of other miscellaneous stuff.
    Memory allocation functions, symbol table
    (*oblist*) functions intern & lookup are now in separate files.
    (car, cdr & so on are in fundamen.asm).
    This module is used by many programs, mainly by:
    LISP.EXE (St. Vitus' Lisp), compile with -DSVL
    KANJIDIC.EXE (Electronic Kanji-Dictionary) and (-DKANJIDIC)
    ODE11.EXE (Octal Debugger & Executor for the PDP-11 code).
    All coded by (C) Antti Karttunen.
 */

/* List concept fundamentally changed at 26-DEC-88. Typed Objects, TOB's */
/* List structure changed again at 26-MAR-89. "Conanized pointers" are used
    instead of 20-bit absolute addresses. (Almost two times faster !)
   Abridged for kanjidic.c, some Lisp-I/O-functions removed at 17-Sep-1989
   to their own module.

   Read macros added at 3. & 4. of January of 1991, by AK.

   Actually this whole module requires cleaning up, i.e. removing of all the
    awful kludges and replacing them with more uniform stuff.
   More control should be given to lisp interpreter by means of global
    system variables (and maybe read macros), and character syntax tables
    (something like ctp_[]). (And possibility to define own integer printtypes
    from lisp interpreter ?) (but that's done already!)
   And maybe function names and syntax should be made more Franz lisp
    compatible. 

   _IBASE_ & _INTEGER_PRINTTYPES_ compactlist-vector  6-Jan-1991

   Monenkin jutun sis{{nlukemisen implementointi "sis{isin{ read-macroina"
    olisi miettimisen arvoinen juttu.

   Esim. '   quote (lisp)
         #'  function (lisp)
         /*  C-style comment
         ;   comment (zapline_rm ?)
         "   string
         `   character or something else
         0   Octal numbers (done already, maybe lousily)
         1-9 Decimal numbers
         (   read list ?
         < { and [ read special list ?
   
   24.AUG.1991 All above done ! genauxread is much cleaner now.

   10-AUG-1991. Some minor modifications made by AK.
   Some issomething -macros changed to f_issomething so that
   reading-process doesn't go haywire when it encounters
   8-bit letters, e.g. IBM- or ISO-scandis or Shift-JIS kanji-characters.
   Function skipwhitespaces now recognizes also Shift-JIS blankos, (hex. 8140).

 */

#include "includes.h"

#define LPAR          '(' /* +1 */
#define RPAR          ')'
 
/* BEGHAR is some list-starting char,
   returns corresponding list-ending char:
   I.e. if it's ( then returns ), and for any other character that
   character + 2, so that for example following pairs work: { } [ ] < >
 */
#define getlendchar(BEGHAR) ((BEGHAR == LPAR) ? (BEGHAR + 1) : (BEGHAR + 2))

#define C 1

/* Error flags for signerror: */
#define LUNBALANCED       257
#define RUNBALANCED       258 
#define UNMATQUOTE        259
#define COMMENTNEVERENDS  260
#define INV_DTPR          261 /* Invalid Dotted Pair */
#define INV_QUOTEXPR      262 /* Invalid Quoted Expression */
#define INV_QUOTCHAR      263 /* Invalid Quoted Character */
#define INV_READMACRO     264 /* Invalid Readmacro */
#define INV_IBASE         265 /* Invalid ibase (not 8 nor 10) */

TOB findsymbols();

BYTE *_EMPTY_STRING_ = byteptr("");
/* This is 'dynamic' pointer, it floats through line: */
/* First time (in program execution) in getnextchar forces to read new line: */
/* maybe_static */ BYTE *lineptr = NULL;
/* Remember that whenever lineptr is set to NULL, it should be immediately
    set to _EMPTY_STRING_ so that getnextchar works at the next time */
/* maybe_static */ BYTE *_save_lineptr=NULL;

static PFSTR getfun;
/* This is 'static' pointer, i.e. it always points to start of line */
maybe_static BYTE *linebuf = byteptr("<VIRGIN>");
static int maxline;
static void *source;
static long linecount=0;
static int visitcount=0;

BYTE *lto2hex();
BYTE *parse_char();
BYTE *itohex();

static PFTOB internfun=intern;

 
/* Returns the next character. Zero if End Of File encountered. */
/* Remember that lineptr points to the next character of linebuf
 *  after getnextchar()
 *   (i.e. related to that character which getnextchar returned)
 */
/* maybe_static */ UINT getnextchar()
{
    UINT getnextchar();

    /* If there is still something in this line, then return it: */
    if(*lineptr) { return(*lineptr++); }
    else /* if lineptr is in the end, then... */
     { /* ...there is reason to read a new line in */
/* This is for the read-macro checking code in genauxread, so that
    it knows when the new line has been read in: */
       _save_lineptr = NULL;
       lineptr = ((*getfun)(linebuf,maxline,source));
       /* if got NULL, then return 0 as sign of EOF: */
       if(!lineptr) { lineptr = _EMPTY_STRING_; return(0); }
       else { if(linecount != -1) { linecount++; } }
       /* If in sentence mode then convert ibm-scandis to seven bit ones: */
       if(*sent_flag) { convert_string(lineptr,ibm_scand2asc); }

       return(getnextchar()); /* And try again... */
     }
}
 
/* Now also handles Shift-JIS blanks (umlaut-u and @ hex: 8140): */
           
maybe_static int skipwhitespaces()
{
        UINT getnextchar();
        register UINT c;
 
leguaani:
        while(f_isspace(c = getnextchar())) { }
        if((c == 0x81) && (*lineptr == '@')) /* If Shift-JIS blank */
         { lineptr++; goto leguaani; } /* Skip the miau, and loop back */
        return(c); /* return first non white space character. */
}


/* sreadexpr: Reads expression from the string given as argument.
    If there is nothing in string (i.e. it's "" or contains just white spaces)
     then ENDMARK is returned.
   Modified at 15. March 1988 to allow multiple reads from same string,
    so that rest of string after returned expression is copied to start.

   For example: if string called string is initially as below:
           BYTE *string = "AKU (1 2) REPE";
    then if sreadexpr is called many times, following happens:
1st time:  sreadexpr(string) returns symbol AKU, string is " (1 2) REPE"
2nd time:  sreadexpr(string) returns list (1 2), string is " REPE"
3rd time:  sreadexpr(string) returns symbol REPE, string is ""
4th & last time:
          sreadexpr(string) returns ENDMARK, because string ended.
   Of course argument of sreadexpr can be constant-string too, like:
   sreadexpr("TAVI (6 9) PETO") and it works equally well
   when calling many times, although it is a little bit perverse idea
   to modify constant-strings.
 */
TOB sreadexpr(line)
BYTE *line;
{
        TOB genread();
        TOB e;
        BYTE *getoneline();

        PFSTR save_getfun;
        BYTE *save_lineptr,*save_linebuf;
        void *save_source;
        int save_maxline,save_visitcount;

/* Save (& Restore) essential read variables, so that ,,environ_var_name
    doesn't mess up the things:
 */
        save_getfun	= getfun;
        save_lineptr	= lineptr;
	save_linebuf 	= linebuf;
	save_source	= source;
	save_maxline	= maxline;
	save_visitcount = visitcount;
        
        visitcount = 0;
/*      linecount  = -1; */
        lineptr    = _EMPTY_STRING_;
         /* ==> so that first time in getnextchar assigns
                                 lineptr to linebuf (via getoneline) */

        e = genread(getoneline,line,0,NULL);
        /* Copy the rest of string to the start of line, so that
            caller can call sreadexpr another time to read more expressions
             from line, if there is any.
         */
        strcpy(line,lineptr);

        getfun	   = save_getfun;
        lineptr	   = save_lineptr;
	linebuf    = save_linebuf;
	source	   = save_source;
	maxline	   = save_maxline;
	visitcount = save_visitcount;

        return(e);
}

BYTE *getoneline(line,turha,turha2)
BYTE *line;
int turha;
void *turha2;
{
        if(!visitcount) { visitcount++; return(line); }
        else { return(NULL); }
}


/* Returns next expression from input stream fp
 * and type of expression in status.
 * If () is encountered, returns NIL, (of course)
 * If EOF is encountered, returns ENDMARK.
 */
TOB readexpr(fp) /* Read Expression (from file-stream fp) */
FILE *fp;
{
        TOB genread();
        BYTE *myfgets(); /* fgets changed to myfgets */

/* shouldn't do lineptr = ""; here because there can still be unread stuff
    in linebuf after previous call to readexpr (lineptr is pointing there)
 */

        return(genread(myfgets,(tob_string(_LINEBUF_)+3),_LINEBUFSIZE_,fp));
}


TOB genread(_getfun,_linebuf,_maxline,_source)
PFSTR _getfun;
BYTE *_linebuf;
int _maxline;
void *_source;
{
        TOB genauxread();
        register TOB p;
        UINT lendchar;
        
        getfun  = _getfun;
        linebuf = _linebuf;
        maxline = _maxline;
        source  = _source;

/* See Aztec-C manual, System Independent Functions, pages lib.57 & 58 */
        if(setjmp(readerrbuf))
         { /* If encountered read-error: (coming from signerror) */
           zapline_rm(); /* Clear the line from other rubbish */
           return(ENDMARK);
         }

        p = genauxread();
        /* If we have encountered ending-parentheses ')', for example,
           then it is surely unbalanced expression: */
        if(specharp(p) && (lendchar = tob_spechar(p)))
         { signerror(LUNBALANCED,lendchar); }
        else { return(p); }
}


/* This is the function which scans the line buffer, skips the white spaces,
    then calls the corresponding read macro if that has been defined for
    that character, otherwise returns symbol composed of those character(s).
 */
TOB genauxread()
{
        TOB result;
        register BYTE c;

loopus:
        c = skipwhitespaces();
        if(!c) { return(ENDMARK); }
         

/* Check whether a read macro has been defined
    for this char. Do it here so that meaning of all other characters
    than '\0', tab, and spaces can be overridden by user:
 */
        if(!nilp(result = cxr(c,_READ_MACROS_)))
         {
           result = call_readmacro(c,result);

/* If read macro returned endmark then ignore the character and continue
    reading. (Like splicing read macro returning NIL in Franz lisp): */
           if(endmarkp(result))
            {
              if(_save_lineptr != lineptr) { goto loopus; }
/* If read macro returned ENDMARK and left lineptr to point to the
    character which triggered it, then don't go back to loopus,
    because that would result an idiot loop, when same character
    would trigger the same read macro again and again. Instead
    continue from this point:
 */           c = skipwhitespaces();
              if(!c) { return(ENDMARK); }
/* Otherwise fall through to the bottom, where symbol_rm is called */
            }
           else { return(result); }
         }

     /* else */ /* If not anything else, then it must me some symbol, */
         {     /* e.g: kala   *   or   krapulapieru */
              /* Symbol parsing function is stored as "read macro" for
                character '\0' which is never encountered on real input: */
           result = call_readmacro(0,car(_READ_MACROS_));
/* If user defined read macro returned ENDMARK (default internal read
   macros never return that) then go back to the beginning of loop.) */   
           if(endmarkp(result)) { goto loopus; }
           /* else */ return(result);
         }
}


TOB call_readmacro(c,aurinko)
BYTE c;
register TOB aurinko; /* Aurinko contains various things at various times */
{
/* Set _save_lineptr to point the character which triggered this read macro:
 */
    _save_lineptr = lineptr-1;

    if(nonnilsymbolp(aurinko)) { aurinko = value(aurinko); }

#ifdef SVL /* If compiled for Lisp */

    /* If read macro defined in Lisp: */
    if(consp(aurinko)) /* || nonnilsymbolp(aurinko) */
     {
       TOB apply();

    /* Nowadays apply accepts also single atom for arglist,
       so this line is used: */
       aurinko = apply(aurinko,string_tob(_save_lineptr));
     }
    else
#endif
    if(bcdp(aurinko))
     { aurinko = ((*(tob_fun(aurinko)))(string_tob(_save_lineptr))); }
    else /* Illegal read macro definition in _READ_MACROS_ */
     { signerror(INV_READMACRO,c); }

    return(aurinko);
}

 
maybe_static UINT isnumber(rest,s)
register BYTE **rest,*s;
{
        UINT default_base;

        *rest = s;

/* If ibase is 0, then don't read numbers at all: (intern them as symbols) */
        if(!(default_base = tob_uint(value(_IBASE_)))) { return(0); }

        /* single + or - is not a valid number: */
        if(!f_isdigit(*s) && !f_isdigit(*(s+1))) { return(0); }

        while(f_isdigit(*++s)) { }
/* If number ends in dot, and after that dot is some non-continuous characters
    then it's decimal anyway, regardless of ibase: */
        if((*s == '.') && !iscontinuous(s+1))
         { *rest = s+1; return(10); }
        else if(iscontinuous(s)) { return(0); }
        else
         {
           *rest = s;
           return(default_base);
         }
}


/* This should be assigned to characters + and - and digits 1-9 and
   also to 0 if that's not assigned to octhex_rm
 */
TOB number_rm(line)
TOB line;
{
    ULI atol();
    ULI lum; /* Beware of Lum ! */
    int num;

    UINT base;
    BYTE *piece;

    /* Set lineptr back to the triggering character */
    set_lineptr(line);

    /* If not a valid number (or ibase = 0) then leave lineptr point
       to the triggering character and return endmark to indicate that
       this should be read in normal way (as a symbol):
     */
    if(!(base = isnumber(&piece,lineptr)))
     { return(ENDMARK); }
    else
     {
       if(base == 8)
        {
          sscanf((lineptr + (*lineptr == '-')),"%lo",&lum);
        }
       else if(base == 10) /* Decimal number */
        {
          /* Take the absolute long value from lineptr: (get it ?) */
          lum = atol(lineptr + (*lineptr == '-'));
        }
       else { signerror(INV_IBASE,base); }

       if(*lineptr == '-') /* If negative value... */
        { /* -32768 is the lowest negative 16-bit integer: */
          if(lum > 32768)
           { lum = -lum; goto longcell; }
          else { num = lum; num = -num; }
        }
       else /* it's positive */
        {
          if(lum > 65535) { goto longcell; }
          else { num = lum; }
        }

       lineptr = piece; /* assign lineptr to the rest of line */

       return((base == 8) ? oct_tob(num) : dec_tob(num));
     }

/* Numbers which don't fit to sixteen bits (-32768 -- 65535) are
    returned as longcells:  */
longcell:
    lineptr = piece;
    return(make_longcell(lum,ENDMARK));
}



/* read macro for reading octal & hex numbers, with leading zero.
   This should be assigned to the character '0'
   If there is some non-octal characters (other than spaces and such)
   then number_rm read macro is used for them.
     I think that this should also return longcells if magnitude goes
   above 65535 but I don't care to write it now.
 */
TOB octhex_rm(line)
TOB line;
{
    register UINT z;
    BYTE *s;

    /* If ibase is zero then don't read even octal numbers: */
    if(!(tob_uint(value(_IBASE_)))) { return(set_lineptr(line)); }

    s = tob_string(line);

    /* At this point s should point to the triggering character
       (i.e. 0) and lineptr to character just after that */

/* If C-style hex number: */
    if(*s == '0' && (*lineptr == 'x') && f_isxdigit(*(lineptr+1)))
     {
       return(hex_tob(hextoi(&lineptr,++lineptr)));
     }

/* Start converting from first digit (i.e. c) if it's accidentally
    something else than '0', e.g. if user has assigned this read macro
    to other digits too:
   (s is assigned to point to first non-octal-digit character)
 */
    z = octoi(&s,s);

 /* If immediately after octal digits is "alnum" digit or dot then
    try with number_rm whether it's decimal number:
 */
    if((iscontinuous(s)) || (*s == '.'))
     {
       return(number_rm(line));
/* This is commented out:
       lineptr--;       (* Set lineptr point back to that zero *)
       return(ENDMARK);
 */
     }

/* If s is still pointing to lineptr, i.e. there was no octal digits
    after zero, then return zero with decimal subtype, not octal: */
    if(s == lineptr) { return(int_tob(z)); }
    else { lineptr = s; return(oct_tob(z)); }
}

#ifdef SVL

TOB quote_rm(line)
TOB line;
{
    UINT c;

    c = *tob_string(line);

    line = genauxread();
    if(endmarkp(line)) { signerror(INV_QUOTEXPR,c); }
    return(list2(_QUOTE_,line));
}

/* This converts #'expression in input to (function expression)
   This function should be assigned to character #
 */
TOB function_rm(line)
TOB line;
{
    UINT c;

    /* If the second character is not single quote (') then
       set lineptr back to # and read it in the normal way: */
    if(*lineptr != '\'')
     {
       lineptr--;
       return(ENDMARK);
     }

    c = *(tob_string(line));

    getnextchar(); /* "read away" that single-quote */
    line = genauxread();
    if(endmarkp(line)) { signerror(INV_QUOTEXPR,c); }
    return(list2(_FUNCTION_,line));

}

#endif

TOB comment_rm(line)
TOB line;
{
    register UINT endchar,c;

    /* If the second character is not an asterisk (*) then
       set lineptr back to triggering character (usually slash)
       and read it in the normal way:
     */
    if(*lineptr != '*')
     {
       lineptr--;
       return(ENDMARK);
     }

    /* Get the triggering character to endchar: */
    endchar = *(tob_string(line));
    lineptr++; /* Skip the asterisk */

Mikki_Hiiri_Korvat:
    while((c = getnextchar()) && (c != '*')) { }
    if(!c) { signerror(COMMENTNEVERENDS,0); }
    if(*lineptr != endchar) { goto Mikki_Hiiri_Korvat; }
    getnextchar(); /* "read away" that endchar */
    return(ENDMARK);
}

/* This is normally assigned to semicolon (;) */
TOB zapline_rm()
{
    while(*lineptr) { lineptr++; }
    return(ENDMARK);
}


/* This is a read macro for character quote: */
TOB charquote_rm(line)
TOB line;
{
    BYTE c;
    UINT rezult,rezult2;

    /* Put to c the character which triggered this read macro: */
    c = *(tob_string(line));

    lineptr = parse_char(&rezult, lineptr);

    /* If there's ending quote after first char or something like \X8140 */
    if((*lineptr == c) || (rezult > 255))
     { /* If latter case then check that there's ending quote, and skip it: */
       if(*lineptr++ != c) { signerror(INV_QUOTCHAR,c); }
       return(char_tob(rezult)); /* And return the result */
     }
    else /* There's a double char constant */
     { /* Read the second one: */
       lineptr = parse_char(&rezult2, lineptr);
       if(*lineptr++ != c) { signerror(INV_QUOTCHAR,c); }
       /* First char to high byte, second one to low byte: */
       return(char_tob((rezult << 8) + rezult2));
     }
}


/* This is a read macro for string quote, usually a doublequote: */
TOB stringquote_rm(line)
TOB line;
{
    /* Read string in from that triggering quote onward: */
    return(getnextstring(&lineptr,tob_string(line)));
}



/* This is for kanjidic: #ifdef KANJIDIC */

TOB value_rm(line)
TOB line;
{
    TOB result;
    BYTE c;

    /* Put to c the character which triggered this read macro: */
    c = *(tob_string(line));

 /* If there is two that kind of characters one after another then
     get the value of an environment variable: */
    if((*lineptr == c))
     {
       BYTE *getenv();
       BYTE save_DINS_flag;

       save_DINS_flag = *dins_flag;
       *dins_flag = 0; /* Intern new symbols */

       getnextchar(); /* "read away" that second getvaluechar */
       result = genauxread();

       if(endmarkp(line))
        {
          *dins_flag = save_DINS_flag;
          signerror(INV_QUOTEXPR,c);
        }

       if(gen_stringp(result))
        {
          BYTE *s;

          if(!(s = getenv(pname(result)))) { result = NIL; }
          else
           {
             BYTE buf[165];

             /* Make safe copy of s because sreadexpr corrupts it: */
             strncpy(buf,s,163);
             result = sreadexpr(buf);
           }
        }

       *dins_flag = save_DINS_flag;

       return(result);
     }

 /* Else it's read macro for getting value of symbol:
    (only one getvalue char): */
     else
      {
        result = genauxread();
        if(endmarkp(result)) { signerror(INV_QUOTEXPR,c); }
        else { return(nonnilsymbolp(result) ? value(result) : result); }
      }
}

/* #endif */


TOB listend_rm(line)
TOB line;
{
    return(spechar_tob(*(tob_string(line))));
}


TOB listbegin_rm(line)
TOB line;
{
        TOB genauxread();
        TOB item; /* returned by genauxread */
        UINT lbegchar;
        UINT lendchar;
        UINT infovar;
        BYTE dots_encountered=0;
        TOB start,lista;

        lbegchar = *tob_string(line);
        lendchar = getlendchar(lbegchar);

        lista = start = cons(NIL,NIL);

        if(lbegchar != LPAR)
         { /* If not that ordinary parentheses () list, then put lbegchar
            *  to first member of list:
            */
           rplacd(lista,cons(spechar_tob(lbegchar),NIL));
           lista = cdr(lista);
         }

        /* Continue so long as item returned is not ENDMARK. */
        while((item = genauxread()) != ENDMARK)
         { /* And not spechar, i.e. list ending character: */
           if(specharp(item) && (infovar = tob_spechar(item)))
            { break; }
           rplacd(lista,cons(item,NIL));
           lista = cdr(lista);
           if(eq(item,_DOT_) && *dtpr_flag) { dots_encountered++; }
         }

        if(endmarkp(item))
         { /* Too much on the left, something is lacking on the right */
           signerror(RUNBALANCED,lendchar);
         }
        else 
         {
           if(infovar == lendchar) /* this particular (correct one) */
            {
              rplacd(lista,NIL); /* Complete the list */
              lista = free_cons(start); /* Free start and get cdr of it */
              if(dots_encountered)
               {
                 TOB naarasvompatti;
                 naarasvompatti = lista; /* Save lista */
                 lista = make_dtpr(dots_encountered,lista);
                 free_list(naarasvompatti); /* Free the old list */
               }
              return(lista);
            }
           else /* too much on the right, something lacks on the left side.
                 *  (because we got wrong kind of list-end-character) */
            { signerror(LUNBALANCED,infovar); }
	 }

}


/*  New compact_read_the_list by AK at 27-11-1989.
    (Name changed to clistbegin_rm).
    Old scheme didn't always work because compact list was allocated to
    stack, and in some occassions it could cross "segment boundary"
    (i.e. 0x?000:FFFF), and that would confuse cdr et other list
    handling primitives. So now stuff is still read into stack,
    but to tob vector instead of compact list
    (each item takes four bytes, not three).
 */
TOB clistbegin_rm(line)
TOB line;
{
        TOB genauxread(),c_make_dtpr(),tobvec_to_clist();
        TOB item; /* returned by genauxread */
        UINT lbegchar;
        UINT lendchar;
        UINT infovar,count=0;
        BYTE dots_encountered=0;
	TOB *lptr;
        TOB read_buf[SIZEOF_CLISTREADBUF+3];

        lbegchar = *tob_string(line);
        lendchar = getlendchar(lbegchar);

        lptr = read_buf;

        if(lbegchar != LPAR)
         { /* If not that ordinary parentheses () list, then put lbegchar
            *  to first member of list:
            */
           *lptr = spechar_tob(lbegchar);
	   lptr++;
           count++;
         }

        /* Continue so long as item returned is not ENDMARK. */
        while((item = genauxread()) != ENDMARK)
         { /* And not spechar, i.e. list ending character: */
           if(specharp(item) && (infovar = tob_spechar(item)))
            { break; }
           if(count > SIZEOF_CLISTREADBUF)
            {
              fprintf(stderr,"\n**ERROR in compact_read_the_list:\n");
              fprintf(stderr,
"count is greater than SIZEOF_CLISTREADBUF (%d > %d). Compact list abridged.\n",
                count,SIZEOF_CLISTREADBUF);
              prstat(stderr);
              infovar = lendchar;
              count--;
              break;
/*
              myexit(1);
 */
            }
	   *lptr = item;
	   lptr++;
           count++;
           if(eq(item,_DOT_) && *dtpr_flag) { dots_encountered++; }
         }
        if(endmarkp(item))
         {  /* Too much on the left, something is lacking on the right */
           signerror(RUNBALANCED,lendchar);
         }
        else
         {
           if(infovar == lendchar) /* this particular (correct one) */
            {
              if(!count) { return(NIL); } /* If read 0 elements */
	      *lptr = NIL; /* Put the ending NIL to vector (not necessary ?)*/
              if(dots_encountered)
               {
                 return(c_make_dtpr(dots_encountered,read_buf,count));
               } /* it's normal list: */
              else { return(tobvec_to_clist(read_buf,count)); }
            }
           else /* too much on the right, something lacks on the left side.
                 *  (because we got wrong kind of list-end-character) */
            { signerror(LUNBALANCED,infovar); }
	 }
}



/*
TOB compact_read_the_list(lbegchar)
int lbegchar;
{
        TOB genauxread();
        TOB item; (* returned by genauxread *)
        int lendchar;
        int infovar,count=0;
        BYTE dots_encountered=0;
        TOB start,lista;
        declare_clist(read_buf,(SIZEOF_CLISTREADBUF+2));

        lista = start = cons_tob(read_buf);
        rplacc(start,1); (* set compact bit for zeroth element *)

        lendchar = getlendchar(lbegchar);

        if(lbegchar != LPAR)
         { (* If not that ordinary parentheses () list, then put lbegchar
            *  to first member of list:
            *)
           BYTE smallbuf[2];
           smallbuf[0] = lbegchar;
           smallbuf[1] = '\0';
           lista = cdr(lista);
           rplaca(lista,((*internfun)(smallbuf)));
           rplacc(lista,1);
           count++;
         }

        (* Continue so long as item returned is not ENDMARK. *)
        while((item = genauxread()) != ENDMARK)
         {
           if(count > SIZEOF_CLISTREADBUF)
            {
              fprintf(stderr,"\nERROR in compact_read_the_list:\n");
              fprintf(stderr,"count is greater than SIZEOF_CLISTREADBUF (%d > %d)\n",
                count,SIZEOF_CLISTREADBUF);
              prstat(stderr);
              myexit(1);
            }
           lista = cdr(lista);
           rplacac(lista,item);
           count++;
           if(eq(item,_DOT_) && *dtpr_flag) { dots_encountered++; }
         }
        if(infovar == EOF)
         { (* Too much on the left, something lacks on the right *)
           signerror(RUNBALANCED,lendchar);
         }
        else if((*islendcharfun)(infovar))
         {
           if(infovar == lendchar) (* this particular (correct one) *)
            {
              if(lista == start) { return(NIL); } (* If read just () *)
              rplacc(lista,0); (* clear c-bit from last elem (necessary ?) *)
              rplacd(lista,NIL); (* Complete the list *)
              if(dots_encountered)
               {
                 return(make_dtpr(dots_encountered,cdr(start)));
               }
              else { return(clistsave(cdr(start))); } (* it's normal list *)
            }
           else (* too much on the right, something lacks on the left side.
                 *  (because we got wrong kind of list-end-character) *)
            { signerror(LUNBALANCED,infovar); }
	 }
}
*/




/* Auxiliary function for plain_read_the_list: */
TOB make_dtpr(dots_encountered,lista)
register BYTE dots_encountered;
register TOB lista;
{
        if((length(lista) != 3)      ||    /* If length is not three */
           (dots_encountered != 1)   ||   /* Or more than one . met */
           (!eq(cadr(lista),_DOT_)))     /* Or middle one is not dot */
         { signerror(INV_DTPR,'.'); }   /* Then it is invalid dotted pair. */
        else /* it's correct, of the form (something . something) */
         {
           return(cons(car(lista),car(cddr(lista))));
         }
}


/* Auxiliary function for compact_read_the_list: */
TOB c_make_dtpr(dots_encountered,tobptr,count)
register BYTE dots_encountered;
register TOB *tobptr;
int count;
{
        if((count != 3)      ||            /* If length is not three */
           (dots_encountered != 1)   ||   /* Or more than one . met */
           (!eq(*(tobptr+1),_DOT_)))     /* Or middle one is not dot */
         { signerror(INV_DTPR,'.'); }   /* Then it is invalid dotted pair. */
        else /* it's correct, of the form (something . something) */
         {
           return(cons(*tobptr,*(tobptr+2)));
         }
}


/*
   Creates (interns) symbol from all the continuous characters from
   line onward, and sets lineptr point to the first following non-continuous
   character on line. If first char of line is non-continuous, then
   single-char symbol interned from that is returned.
 */
TOB symbol_rm(line)
TOB line;
{
        TOB result;
        register BYTE *string;
        BYTE *start;
        BYTE save;

        string = tob_string(line);
        start = string; /* mark the start of the string */
        /* It is guaranteed that string is never "", (i.e. empty)
            so following line is unnecessary: */
/*      if(!*string) { return(NULL); } */

/* If first char is noncontinuous then symbol is made from that
    single character: */
        if(!(iscontinuous(string))) { string++; }
        else /* Otherwise all the following continuous characters are */
         {  /* taken to the symbol: */
           while(iscontinuous(string)) { string++; }
         }
        
        lineptr    = string;  /* Parsing continues from this onward */
        save       = *string; /* Save the first non-continuous character */
        *string    = '\0'; /* So that it can be overwritten with zero */
                          /* so that intern works... */

        if(*nil_flag && !strcmp(start,"nil")) { result = NIL; }
        else { result = ((*internfun)(start)); }

        *string    = save;
        return(result);
}

#ifdef SVL
/* Like symbol_rm but makes symbol lowercase */
TOB lowersymbol_rm(line)
TOB line;
{
        TOB result;
        register BYTE *string;
        BYTE *start;
        BYTE save;

        string = tob_string(line);
        start = string; /* mark the start of the string */
        /* It is guaranteed that string is never "", (i.e. empty)
            so following line is unnecessary: */
/*      if(!*string) { return(NULL); } */

/* If first char is noncontinuous then symbol is made from that
    single character: */
        if(!(iscontinuous(string))) { *string = tolower(*string); string++; }
        else /* Otherwise all the following continuous characters are */
         {  /* taken to the symbol: */
           while(iscontinuous(string))
            { *string = tolower(*string); string++; }
         }
        
        lineptr    = string;  /* Parsing continues from this onward */
        save       = *string; /* Save the first non-continuous character */
        *string    = '\0'; /* So that it can be overwritten with zero */
                          /* so that intern works... */

        if(*nil_flag && !strcmp(start,"nil")) { result = NIL; }
        else { result = ((*internfun)(start)); }

        *string    = save;
        return(result);
}

#endif

/* Like lowersymbol_rm but makes symbol uppercase */
TOB uppersymbol_rm(line)
TOB line;
{
        TOB result;
        register BYTE *string;
        BYTE *start;
        BYTE save;

        string = tob_string(line);
        start = string; /* mark the start of the string */
        /* It is guaranteed that string is never "", (i.e. empty)
            so following line is unnecessary: */
/*      if(!*string) { return(NULL); } */

/* If first char is noncontinuous then symbol is made from that
    single character: */
        if(!(iscontinuous(string))) { *string = toupper(*string); string++; }
        else /* Otherwise all the folupping continuous characters are */
         {  /* taken to the symbol: */
           while(iscontinuous(string))
            { *string = toupper(*string); string++; }
         }
        
        lineptr    = string;  /* Parsing continues from this onward */
        save       = *string; /* Save the first non-continuous character */
        *string    = '\0'; /* So that it can be overwritten with zero */
                          /* so that intern works... */

        if(*nil_flag && !strcmp(start,"NIL")) { result = NIL; }
        else { result = ((*internfun)(start)); }

        *string    = save;
        return(result);
}


#ifdef SVL
/*
   Version of symbol_rm used by hubafish.
 */
TOB wordsymbol_rm(line)
TOB line;
{
        TOB result;
        register BYTE *string;
        BYTE *start;
        BYTE save;

        string = tob_string(line);
        start = string; /* mark the start of the string */
        /* It is guaranteed that string is never "", (i.e. empty)
            so following line is unnecessary: */
/*      if(!*string) { return(NULL); } */
        if(!(iscontinuous(string))) { string++; }
        else
         {
loop:
           while(iscontinuous(string)) { string++; }
           if(((*string == '.') || (*string == ':')))
            { /* Don't understand dot as end of sentence if there is
                  some alfanumeric character after it. */
              /* And also word YK:n is different thing than Jesse: "Huu !" */
              if(iscontinuous((string+1))) { string++; goto loop; }
              else /* If last dot from abbreviation like E.g. or I.e.
                       or one-letter abbreviation like n. or 1. */
               if((*string == '.') /* However, this applies only to dot */
                   && !(iscontinuous((string-2)))
                    && (iscontinuous((string-1))))
                { string++; goto loop; } /* Then also continue */
            }
         }
        
        lineptr    = string;  /* Parsing continues from this onward */
        save       = *string; /* Save the first non-continuous character */
        *string    = '\0'; /* So that it can be overwritten with zero */
                          /* so that intern works... */

        if(*nil_flag && !strcmp(start,"nil")) { result = NIL; }
        else /* Again some ugly code ahead */
         {
           BYTE flag;

           /* If encountered Capitalized (but not wholly UPPERCASE) symbol
               in sentence mode, then change it's first letter to lowercase
               and set xbit of that symbol.
              (This is an ugly kludge for the hubafish.lsp). By ak 17.7.90.
            */
/* Some potential bugs here for big [ and \ ? Especially with IBM-scandis,
    check f_isupper function in strfuns2.c: */
           if(f_isupper(*start) && !f_isupper(*(start+1)))
            { *start = _tolower(*start); flag = 1; }
           else { flag = 0; }
           result = ((*internfun)(start));
           if(flag) { setxbit(result,int_tob(1)); }
         }

        *string    = save;
        return(result);
}

#endif

TOB getnextstring(rest_of_string,source)
BYTE **rest_of_string;
register BYTE *source;
{
        register BYTE c;
        BYTE *start,*dest;
        UINT escres; /* Result of escape extension, returned by parse_char */
        TOB result;

        start = source; /* Let start point to the first quote */
        c = *source++;  /* skip first quote, and save it to c */
        dest = source;  /* Set dest point to the start of string */
        /* Go through source until '\0' or second quote met: */
        while(*source && (*source != c))
         {
           source = parse_char(&escres,source);
/* If esc_flag is on then escape-sequences are translated to corresponding
    characters: */
           if(*esc_flag)
            { /* If escape sequence resulted to characters, like \X8140 */
              if(escres > 255) /* Then put high byte first */
               { *dest++ = gethighbyte(escres); escres &= 0xFF; }
              *dest++ = escres; /* And put low byte to string in any case */
            }
/* If it's not on, they are still recognized, but kept intact: */
           else { dest = source; }
         }

        /* If no second quote found, then generate an error: */
        if(!*source) { lineptr = _EMPTY_STRING_; signerror(UNMATQUOTE,c); }

        c = *dest; /* Save character after last char of string */
        *dest      = '\0'; /* Overwrite it with 0, so that stringsave works */
        result     = stringsave(start+1);
        *dest      = c; /* Restore that char (is this necessary ?) */
        /* Pointing to the first character after quote: */
        *rest_of_string = (source+1);
        return(result);
}


/* This function can be used by user defined read macros to
   set internal variable lineptr to desired location.
 */
TOB set_lineptr(line)
TOB line;
{
    lineptr = tob_string(line);
    return(ENDMARK);
}


/* This function can be used by user defined read macros to read
    the following stuff to string.
 */
TOB read_next_to_string(str,maxcount)
TOB str,maxcount;
{
    register BYTE *s;
    register UINT max;

    s = tob_string(str);
    max = tob_uint(maxcount);

    if(!lineptr || (lineptr == _EMPTY_STRING_)) { return(NIL); }
    
    *s++ = *(lineptr-1);
    while(max-- && *lineptr && (iscontinuous(lineptr)))
     { *s++ = *lineptr++; }
    *s = '\0';

    return(str);
}

/* Signal some read-error, and possibly go back to toplevel: */
signerror(errcode,character)
UINT errcode,character;
{
    fprintf(stderr,"\n**READ ERROR:");
    switch(errcode)
     {
       case RUNBALANCED:
         {
           fprintf(stderr,
"\nUnbalanced list expression, missing %c detected at the end of file\n",
               character);
           break;
         }
       case LUNBALANCED:
         {
           fprintf(stderr,
            "\nUnbalanced list expression, extra %c at the line %ld\n",
              character,linecount);
           break;
         }
       case UNMATQUOTE:
         {
           fprintf(stderr,
            "\nMissing quote %c in quoted string at the line %ld\n",
               character,linecount);
           break;
         }
       case COMMENTNEVERENDS:
         {
           fprintf(stderr,
"\nMissing end delimiter of the comment, detected at the end of file\n");
           break;
         }
       case INV_DTPR:
         {
           fprintf(stderr,
            "\nInvalid dotted pair at the line %ld\n",linecount);
           break;
         }
       case INV_QUOTEXPR: case INV_QUOTCHAR:
         {
           fprintf(stderr,
            "\nInvalid quoted %s at the line %ld\n",
               ((errcode == INV_QUOTEXPR) ? "expression" : "character"),
                  linecount);
           break;
         }
       case INV_READMACRO:
         {
           fprintf(stderr,
             "\nCharacter %c,%u.,\\%o,0x%02x has invalid read-macro: ",
                character,character,character,character);
           eprint(cxr(character,_READ_MACROS_));
           break;
         }
       case INV_IBASE:
         {
           fprintf(stderr,"\nInvalid base: %u  ibase reset to 10. was: ",
                     character);
           eprint(value(_IBASE_));
           setvalue(_IBASE_,int_tob(10));
           break;
         }
       default: /*  Hyvin menee. */
         {
           fprintf(stderr,
"\nTilt Honk TyyT TyyT in signerror, errcode is %d, character is %c:%xH\n",
            errcode,character,character);
           break;
         }
     }

        fprintf(stderr,"**linebuf: %d/%s\n",strlen(linebuf),linebuf);
        fprintf(stderr,"**lineptr: %d/%s\n",strlen(lineptr),lineptr);
        longjmp(readerrbuf,-1); /* Jump back to genread */
}

/* ======================================================================== */


/* This is the function used by symbol_rm to decide whether character at
   *s should be still taken to symbol, or is it some delimiter character.
   String pointer s should always point to some location in buffer linebuf!
   Global string _CHARFLAGS_ (in lisp *charflags*) is used to decide
   whether character is "continuous" or not. If corresponding byte in
   corresponding location contains non-zero (currently 1) then that
   char is understood as continuous. But there is two exceptions:
   If there is umlaut-u (in IBM-PC-ascii) followed with miau (@) then
   that is understood as delimiter, because it's Shift-JIS blank.
   (skipwhitespaces will get rid of that after this).
   And if *s is left parenthesis, then if preceding character is ESC
   then that is not understood as delimiter, because ESC(J is shift-out
   sequence in JIS standard. (Unless s is in the beginning of linebuf,
   there isn't any preceding character, of course).
 */
maybe_static iscontinuous(s)
BYTE *s;
{
    return(((*(tob_string(_CHARFLAGS_) + *s)) && /* Non-zero in charflags ? */
            ((*s != 0x81) || (*(s+1) != '@'))) /* And not Shift-JIS blank */
            || /* Or left parentheses preceded by ESC: */
            ((*s == LPAR) && (s != linebuf) && (*(s-1) == 27)));
}

/* --------------------------------------------------------------------*/

/* Note that these function use subfromlow macro defined
   in mydefs.h (why there ?) which in turn uses the function difference
   defined in arithmetic.asm. And of course that's not the fastest possible
   code. I should code these too in assembly.
 */

TOB setvalue(x,y)
TOB x,y;
{
        if(!nonnilsymbolp(x)) { inv2arg("setvalue",1,x,y); }
/*      q_rplacd((cleartagbits(x)-SIZEOF_CONSCELL),y);  BUGGY CODE ! */
/*      q_rplaca(subfromlow(cleartagbits(x),SIZEOF_INTERNAL),y); */
/* Modified at 17.7.1990 by AK to use the normal rplaca instead of the
    q_rplaca, because xbit of the value must be saved now.
 */
        rplaca(subfromlow(cleartagbits(x),SIZEOF_INTERNAL),y);
        return(y);
}

TOB setplist(x,y)
TOB x,y;
{
        if(!nonnilsymbolp(x)) { inv2arg("setplist",1,x,y); }
        if(*plist_flag) /*{ q_rplaca((cleartagbits(x)-SIZEOF_CONSCELL),y); } */
         { q_rplaca(subfromlow(cleartagbits(x),SIZEOF_CONSCELL),y); }
        return(y);
}

TOB value(x)
TOB x;
{
        if(!nonnilsymbolp(x)) { inv1arg("value",x); }
/*      return(q_cdr(cleartagbits(x)-SIZEOF_CONSCELL)); */
        return(q_car(subfromlow(cleartagbits(x),SIZEOF_INTERNAL)));
}

TOB plist(x)
TOB x;
{
        if(!nonnilsymbolp(x)) { inv1arg("plist",x); }
        if(*plist_flag) /*{ return(q_car(cleartagbits(x)-SIZEOF_CONSCELL)); }*/
         { return(q_car(subfromlow(cleartagbits(x),SIZEOF_CONSCELL))); }
	else { return(NIL); }
}



/* This sets x-bit of the symbol (= compact-bit of the value) to zero
    if y is nil or zero, otherwise sets it to one.
 */
TOB setxbit(x,y)
TOB x,y;
{
        if(!nonnilsymbolp(x)) { inv2arg("setxbit",1,x,y); }
        rplacc(subfromlow(cleartagbits(x),SIZEOF_INTERNAL),
                 (!nilp(y) && !L_zerop(y)));
        return(y);
}

/* This returns 0 or 1 (as TOB) according to the state of the xbit. */
TOB getxbit(x)
TOB x;
{
    if(!nonnilsymbolp(x)) { inv1arg("getxbit",x); }
    return(int_tob(!!compactp(/* (void *) */ subfromlow(cleartagbits(x),
                                                   SIZEOF_INTERNAL))));
}


/* This is like previous, but returns nil or t. */
TOB xbitp(x)
TOB x;
{
    if(!nonnilsymbolp(x)) { inv1arg("xbitp",x); }
    return(T_OR_NIL(compactp(/* (void *) */ subfromlow(cleartagbits(x),
                                                  SIZEOF_INTERNAL))));
}


int bcdp(x)
TOB x;
{
/* See the note at initlists function ! About _Corg and _Cend !
   Ah, here is insidious logical bug. There's no way to tell
   some strings from the bcd's if those strings are in segment 0,
   like some environment variables and command line arguments
   can be.
        if(!otherp(x) || getsegbits(x)) { return(0); }
        x = ptrtoabs(tob_fun(x),get_cs());
        return((x >= code_lowerlim) && (x < code_upperlim));
 */ /* So use this old definition: */
        return(otherp(x) && _bcdp(x));
}

int portp(x)
TOB x;
{
        if(!otherp(x)) { return(0); }
        x = ptrtoabs(cleartagbits(x));
        return((x >= fp_lowerlim) && (x < fp_upperlim));
}

int i_stringp(x)
TOB x;
{
/* Is of type other, but is not function pointer nor file pointer: */
        return(otherp(x) && !_bcdp(x) && !portp(x) && !endmarkp(x));
/* And not ENDMARK ! Important fix made at 29-Jul-1990 ^^^^^^^^^^^^ */
}


TOB cons(car_part,cdr_part)
TOB car_part,cdr_part;
{
        register TOB z;
        
        z = consalloc();
        
        q_rplaca(z,car_part);
        q_rplacd(z,cdr_part);
        return(z);
}

TOB make_longcell(longnum,cell)
ULI longnum;
TOB cell;
{
    if(!consp(cell))
     { return(cons(int_tob(getlow(longnum)),int_tob(gethigh(longnum)))); }
    else
     {
       rplaca(cell,int_tob(getlow(longnum)));
       rplacd(cell,int_tob(gethigh(longnum)));
       return(cell);
     }
}


/* ======================================================================= */


/* Some format-strings: */

/* These are now unnecessary:
maybe_static BYTE *F_signed_int   = "%d";
maybe_static BYTE *F_unsigned_int = "%u";
maybe_static BYTE *F_int          = "%d";
 */
#define F_other        "<OTHER-0x"
#define F_fp           "<OTHER/FP-0x"
#define F_bcd          "<OTHER/BCD-0x"


/* Initializes the list system. I.e: allocates the big chunk of memory,
    and initializes some system variables:
   This code is very Aztec-C specific.
   E.g. Cbuffs[] is array of MAXSTREAM (defined in stdio.h) FILE
   structures where stdin, stdout, stderr, etc are kept.
   _Corg and _Cend are Aztec-C's symbols to denote the origin and the end
   of compiled code respectively. Note that if using overlays, like in
   KANJIDIC then overlaid functions are after _Cend (at least I think so!)
 */
initlists(allocspace)
ULI allocspace;
{
    int prstat();
    extern PFI _addr_of_prstat;
    extern FILE Cbuffs[];
    extern _Corg(),_Cend();
    register BYTE *s;
    register UINT i;
    ULI l;

    lineptr = _EMPTY_STRING_;

    /* If user has defined environment variable MAXMEM then use that
       as allocspace, but if not then use argument user has supplied
       if it's not zero, and if it's zero then use LDEFAULT_LISTSPACE
       defined in lists.h:
     */
    if(!(s = getenv("MAXMEM")) || !(l = atol(s))) /* And is non-zero! */
     { allocspace = (allocspace ? allocspace : LDEFAULT_LISTSPACE); }
    else { allocspace = l; }

    /* If user has defined env.var OBTABSIZE then use that as
       size for _obtable (if it's not zero). Otherwise use the
       constant DEFAULT_OBTAB_SIZE defined in lists.h:
     */
    if(!(s = getenv("OBTABSIZE")) || !(i = atoi(s)))
     { _OBTAB_SIZE_ = DEFAULT_OBTAB_SIZE; }
    else { _OBTAB_SIZE_ = i; }

    if(!(s = getenv("LINEBUFSIZE")) || !(i = atoi(s)))
     { _LINEBUFSIZE_ = DEFAULT_LINEBUFSIZE; }
    else { _LINEBUFSIZE_ = i; }


/* Link to allocs.c, so that allocation-funs (challoc, galloc, palloc, etc.)
 *  can call prstat() if they bomb out. (if all space has been exhausted.)
 */
    _addr_of_prstat = prstat;

    /* Pointer to the beginning of area where file pointers are kept: */
    fp_lowerlim = ptrtoabs(Cbuffs);
    /* Pointer to first location which is beyond the file pointers. */
    fp_upperlim = (fp_lowerlim + (MAXSTREAM * sizeof(FILE)));

    /* In same way define absolute addresses for lower & upper limits
       for binary code, so that bcdp can check whether its argument
       is pointer to C-coded function or string in some funny place.
       Note that this works only with small code memory model:
       (I think that these are not currently used for any sensible
       purpose...)
     */
    code_lowerlim = ptrtoabs(_Corg,get_cs());
    code_upperlim = ptrtoabs(_Cend,get_cs());

#if LARGEDATAP
    _allocp = _lowlim = getheap();

    _uplim = ptradd(_lowlim,allocspace);

    if(brk(_uplim))
     {
       fprintf(stderr,
"\ninitlists: can't set heap pointer to 0x%lx ! heap: 0x%lx   allocspace: %ld.\n",
        _uplim,getheap(),allocspace);
       prstat(stderr);
       fprintf(stderr,
"Please set environment variable MAXMEM to suitable (smaller) value, e.g:\n");
       fprintf(stderr,
"SET MAXMEM=50000\n");
       myexit(1);
     }
#else
    3++; CANNOT BE COMPILED WITH SMALL DATA MEMORY MODEL !
#endif

    _freelist = NIL; /* Must be NIL before consing */

    lists2sparecons = cons(NIL,NIL);

    /* Allocate some global tables: */
    _CHARFLAGS_  = new_string(256);
    _IO_FLAGS_   = new_string(MAXFLAGS);
/* Allocate vector of 256 lambda-forms for read-macros (for every character).
     Initially every TOB is NIL. Readmacros can be accessed via the global
     variable *read-macros*
 */
    _READ_MACROS_        = new_clist(256);
/* Make 17 element long vector of printing forms of subtypes of integers: */
    _INTEGER_PRINTTYPES_ = new_clist(17);
    /* Six bytes more to be sure that there's enough space: */
    _LINEBUF_ = new_string(int_tob(_LINEBUFSIZE_+6));
    /* Allocate _obtable before interning */    
    _obtable = new_clist(_OBTAB_SIZE_);

    /* Assign these flag pointers at C level: */
    s = tob_string(_IO_FLAGS_);
    abrev_char      = s;
    debug_flag      = s+1;
    dins_flag       = s+2;
    dtpr_flag       = s+3;;
    esc_flag        = s+4;
    ibmscand_flag   = s+5;
    nil_flag        = s+6;
    plist_flag      = s+7;
    prefix_char     = s+8;
    quote_char      = s+9;
    quote_flag      = s+10;
    sent_flag       = s+12;
    speclist_flag   = s+12;
    warning_flag    = s+13;

    /* And reset them: (Must be done before any interning!) */
    reset_io_flags();

    /* _NEWSYMBVALUE_ must be set before other symbols are interned: */
    _NEWSYMBVALUE_ = NIL;
    _NEWSYMBVALUE_ = intern("*newsymbvalue*");
    _UNBOUND_      = intern("*unbound*");
    setvalue(_UNBOUND_,_UNBOUND_);
    /* All new symbols' values are initialized to *unbound* */
    setvalue(_NEWSYMBVALUE_,_UNBOUND_);

    /* Intern some important symbols of lisp: */
    _DOT_      = intern(".");        /* Used when reading dotted pairs */
    _FUNCTION_ = intern("function"); /* Used by function_rm */
    _IBASE_    = intern("ibase");    /* Default base of integers */
    _QUOTE_    = intern("quote");    /* Used by quote_rm */
    _T_        = intern("t");        /* Important lisp variable */

 /* Put three blanks before the actual buffer so that when some functions
    like symbol_rm and iscontinuous check characters before the
    current linepointer they find something sensible there:
    (But what happens when reading stuff from user supplied string ???)
                                  123   */
    strcpy(tob_string(_LINEBUF_),"   ");

    reset1readmode(); /* Reset the rest */

}

TOB reset_readmode()
{
    reset_io_flags();
    reset1readmode();
    return(NIL);
}


TOB reset1readmode()
{
    register UINT i;
    register BYTE *s;
    register TOB cl;

    /* RESET _CHARFLAGS_ BYTES TO THEIR DEFAULT VALUES: */
    s = tob_string(_CHARFLAGS_);
    for(i=0; i < 256;) { s[i++] = 1; } /* First set all */
    for(i=0; i < 33;)  { s[i++] = 0; } /* Then clear controls & space. */
    s[27] = 1; /* Except set the ESC */
    s['('] = s[')'] = 0; /* Clear the parentheses */
    s[127] = 0; /* And DEL */

    /* INITIALIZE THE READ MACRO TABLE: */
    /* First clear all: */
    for(cl = _READ_MACROS_; !nilp(cl); cl = cdr(cl))
     { rplaca(cl,NIL); }
    /* Then set those default read macros: */
    /* symbol_rm pseudo-readmacro is stored into location for
        letter '\0' which is never encountered on input: */
    rplaca(_READ_MACROS_,fun_tob(symbol_rm));
    rplacx('0',_READ_MACROS_,fun_tob(octhex_rm));
    rplacx('1',_READ_MACROS_,fun_tob(number_rm));
    rplacx('2',_READ_MACROS_,fun_tob(number_rm));
    rplacx('3',_READ_MACROS_,fun_tob(number_rm));
    rplacx('4',_READ_MACROS_,fun_tob(number_rm));
    rplacx('5',_READ_MACROS_,fun_tob(number_rm));
    rplacx('6',_READ_MACROS_,fun_tob(number_rm));
    rplacx('7',_READ_MACROS_,fun_tob(number_rm));
    rplacx('8',_READ_MACROS_,fun_tob(number_rm));
    rplacx('9',_READ_MACROS_,fun_tob(number_rm));
    rplacx('-',_READ_MACROS_,fun_tob(number_rm));
    rplacx('+',_READ_MACROS_,fun_tob(number_rm));
    rplacx(';',_READ_MACROS_,fun_tob(zapline_rm));
    rplacx('/',_READ_MACROS_,fun_tob(comment_rm));
#ifdef SVL
    rplacx('\'',_READ_MACROS_,fun_tob(quote_rm));
    rplacx('#',_READ_MACROS_,fun_tob(function_rm));
#endif
    rplacx('`',_READ_MACROS_,fun_tob(charquote_rm));
    rplacx('"',_READ_MACROS_,fun_tob(stringquote_rm));
    rplacx('(',_READ_MACROS_,fun_tob(listbegin_rm));
    rplacx(')',_READ_MACROS_,fun_tob(listend_rm));


    /* RESET _INTEGER_PRINTTYPES_ TO THEIR DEFAULT VALUES: */
    /* 0 Decimal, unsigned by default */
    rplaca(_INTEGER_PRINTTYPES_,stringsave("%u")); 
    /* 1 Hex, precision four digits */
    rplacx(1,_INTEGER_PRINTTYPES_,fun_tob(out_hex));
    rplacx(2,_INTEGER_PRINTTYPES_,stringsave("0%o")); /* 2 Octal, C-style */
    rplacx(3,_INTEGER_PRINTTYPES_,fun_tob(out_char)); /* 3 Character */
    rplacx(4,_INTEGER_PRINTTYPES_,fun_tob(out_bytepair)); /* 4 Bytepair  */
    rplacx(5,_INTEGER_PRINTTYPES_,stringsave("<#pick %u>")); /* 5 Pick form */
    /* Printtypes 6-15 are left intact, by default they are () but if user has
       assigned them to something then those definitions are not removed.*/
    /* 10 This is for the longcells */
    rplacx(16,_INTEGER_PRINTTYPES_,stringsave("%lu"));

    /* Set default values some of the global variables of lisp: */
    setvalue(_UNBOUND_,_UNBOUND_);
    /* All new symbols' values are initialized to *unbound* */
    setvalue(_NEWSYMBVALUE_,_UNBOUND_);

    setvalue(_IBASE_,int_tob(10)); /* Use decimal base by default */
    setvalue(_T_,_T_);

    setvalue(intern("*oblist*"),_oblist);
    setvalue(intern("*obtable*"),_obtable);
    setvalue(intern("*linebuf*"),(_LINEBUF_+3));

    setvalue(intern("*integer-printtypes*"),_INTEGER_PRINTTYPES_);
    setvalue(intern("*io-flags*"),_IO_FLAGS_);
    setvalue(intern("*charflags*"),_CHARFLAGS_);
    setvalue(intern("*read-macros*"),_READ_MACROS_);


#ifdef SVL
    /* Create global variables for read macros: */
    setvalue(intern("*symbol-rm*"),fun_tob(symbol_rm));
    setvalue(intern("*lowersymbol-rm*"),fun_tob(lowersymbol_rm));
    setvalue(intern("*uppersymbol-rm*"),fun_tob(uppersymbol_rm));
    setvalue(intern("*wordsymbol-rm*"),fun_tob(wordsymbol_rm));
    setvalue(intern("*octhex-rm*"),fun_tob(octhex_rm));
    setvalue(intern("*number-rm*"),fun_tob(number_rm));
    setvalue(intern("*zapline-rm*"),fun_tob(zapline_rm));
    setvalue(intern("*comment-rm*"),fun_tob(comment_rm));
    setvalue(intern("*quote-rm*"),fun_tob(quote_rm));
    setvalue(intern("*function-rm*"),fun_tob(function_rm));
    setvalue(intern("*charquote-rm*"),fun_tob(charquote_rm));
    setvalue(intern("*stringquote-rm*"),fun_tob(stringquote_rm));
    setvalue(intern("*value-rm*"),fun_tob(value_rm));
    setvalue(intern("*listbegin-rm*"),fun_tob(listbegin_rm));
    setvalue(intern("*clistbegin-rm*"),fun_tob(clistbegin_rm));
    setvalue(intern("*listend-rm*"),fun_tob(listend_rm));

    /* Create global variables pointing to IO-flags: */
    setvalue(intern("*abrev-char*"),string_tob(abrev_char));
    setvalue(intern("*debug-flag*"),string_tob(debug_flag));
    setvalue(intern("*dins-flag*"),string_tob(dins_flag));
    setvalue(intern("*dtpr-flag*"),string_tob(dtpr_flag));
    setvalue(intern("*esc-flag*"),string_tob(esc_flag));
    setvalue(intern("*ibmscand-flag*"),string_tob(ibmscand_flag));
    setvalue(intern("*nil-flag*"),string_tob(nil_flag));
    setvalue(intern("*plist-flag*"),string_tob(plist_flag));
    setvalue(intern("*prefix-char*"),string_tob(prefix_char));
    setvalue(intern("*quote-char*"),string_tob(quote_char));
    setvalue(intern("*quote-flag*"),string_tob(quote_flag));
    setvalue(intern("*sent-flag*"),string_tob(sent_flag));
    setvalue(intern("*speclist-flag*"),string_tob(speclist_flag));
    setvalue(intern("*warning-flag*"),string_tob(warning_flag));

#endif

    return(NIL);
}


/* Reset IO flags to their default values: */
TOB reset_io_flags()
{
    register BYTE *s;

/* Must set flag values before other operations like intern and so on: */
    s = tob_string(_IO_FLAGS_);

                         /* Default state */
    *abrev_char     = 0; /* No abbreviations */
    *debug_flag     = 0; /* No debug printing */
    *dins_flag      = 0; /* Intern new symbols */
    *dtpr_flag      = 1; /* Recognize dotted pairs on input */
    *esc_flag       = 1; /* Recognize escape sequences in strings */
    *ibmscand_flag  = 0; /* Do not convert scandis on output */
    *nil_flag       = 1; /* Convert nil in input to () */
    *plist_flag     = 1; /* Allocate space for plist's too */
    *prefix_char    = 0; /* No prefixes (this is for KANJIDIC) */
    *quote_char     = '`'; /* Use backquote for printing */
    *quote_flag     = 1; /* By default use quotes on printing */
    *sent_flag      = 0; /* No sentence mode */
    *speclist_flag  = 0; /* No special printing for special lists */
    *warning_flag   = 0; /* No warning system enabled (for KANJIDIC) */

    return(NIL);
}

/* 'Frees' the cons_cell, in another words, puts it to front of _freelist,
     so it can be later used by cons when it needs a new cons cell.
    Returns as result the previous cdr of that cons cell.
 */
TOB free_cons(cons_cell)
TOB cons_cell;
{
        if(nilp(cons_cell)) { return(NIL); }
        else if(listp(cons_cell))
         {
           TOB z;

           z = cdr(cons_cell); /* Save the previous cdr */
           _freelist = rplacd(cons_cell,_freelist);
           return(z);
         }
        else
         {
           inv1arg("free_cons",cons_cell);
         }
}


TOB get_free_list()
{
        return(_freelist);
}


/* Put lista to front of _freelist, so cons cells of its toplevel can
    be later used by cons when it needs new cons cells.
   And of course lista shouldn't be used in anyway after free'ing.
 */
/* The last cons cell of freelist is kept in last_of_freelist to help
   preventing circular lists being accidentally made. (If free'ed same
   list twice).
 */

TOB free_list(lista)
register TOB lista;
{
        TOB last_one;

        if(nilp(lista)) { return(NIL); } /* Do nothing for NIL's */
        if(!listp(lista)) { inv1arg("free_list",lista); }

        last_one = last(lista);

        if(nilp(_freelist)) /* Starting to make new freelist again... */
         {
           _freelist = lista;
           last_of_freelist = last_one;
         }
        else if(eq(last_one,last_of_freelist))
         {
           BYTE buf[12];

           freelist_warnings++;
/* Comment out the printing of warning message, encountered too often:
           fprintf(stderr,
"\n\007**free_list: tried to free same list twice, ignored: (from %s) ",
             ftohex(buf,getretadr()));
           printexprnl(lista,stderr);
 */
           return(NIL);
         }
        else { rplacd(last_one,_freelist); _freelist = lista; }
        return(_freelist);
}
        

int prstat(fp) /* Print some memory statistics to fp */
FILE *fp;
{
       extern ULI _strsavecnt,_clistsavecnt;

       fprintf(fp,"\n\n** MEMORY USAGE **\n");
       fprintf(fp,"heap: 0x%lx\n",((ULI) getheap()));
       fprintf(fp,"_lowlim: 0x%lx   _allocp: 0x%lx   _uplim: 0x%lx\n",
          ((ULI)_lowlim),((ULI)_allocp),((ULI)_uplim));
       fprintf(fp,"_uplim  - _lowlim: (TOTAL) 0x%lx / %ld.\n",
                    lptrdiff(_uplim,_lowlim),lptrdiff(_uplim,_lowlim));
       fprintf(fp,"_allocp - _lowlim: (USED)  0x%lx / %ld.\n",
                    lptrdiff(_allocp,_lowlim),lptrdiff(_allocp,_lowlim));
       fprintf(fp,"_uplim  - _allocp: (FREE)  0x%lx / %ld.\n",
                    lptrdiff(_uplim,_allocp),lptrdiff(_uplim,_allocp));
       fprintf(fp,
        "sizeof(void *): %d   SIZEOF_INTERNAL: %d   SIZEOF_CONSCELL: %d\n",
              sizeof(void *),SIZEOF_INTERNAL,SIZEOF_CONSCELL);
       fprintf(fp,
  "_strsavecnt: %ld. / 0x%lx   _clistsavecnt: %ld.   *%d: %ld. / 0x%lx\n",
         _strsavecnt,_strsavecnt,
           _clistsavecnt,SIZEOF_INTERNAL,(_clistsavecnt * SIZEOF_INTERNAL),
             (_clistsavecnt * SIZEOF_INTERNAL));
       fprintf(fp,
   "(_strsavecnt + (%d * _clistsavecnt) + (%d * length(_oblist))) = %ld.\n",
       SIZEOF_INTERNAL,SIZEOF_CONSCELL,
        (_strsavecnt + (SIZEOF_INTERNAL * _clistsavecnt) +
                        (SIZEOF_CONSCELL * ((ULI) length(_oblist)))));

       fprintf(fp,
        "_consalloc_cnt: %ld   *%d: %lu   _residueconscnt: %ld\n",
          _consalloc_cnt,SIZEOF_CONSCELL,
            (_consalloc_cnt * SIZEOF_CONSCELL),_residueconscnt);
       fprintf(fp,
        "linebuf=%s\nstrlen(linebuf): %d\n",linebuf,strlen(linebuf));
       fprintf(fp,
        "lineptr=%s\nstrlen(lineptr): %d\n",lineptr,strlen(lineptr));
       fprintf(fp,"length(_freelist): %u   ",length(_freelist));
       fprintf(fp,"length(_free_strings_list): %u\n",
                    length(_free_strings_list));
       fprintf(fp,"length(_oblist): %d.   *%d: %ld.\n",
        length(_oblist),SIZEOF_CONSCELL,
         ((ULI) (((ULI) SIZEOF_CONSCELL) * length(_oblist))));
       fprintf(fp,
"plist_flag: %d   freelist_warnings: %u\n",
         *plist_flag,freelist_warnings);

/*     printexprnl(last_of_freelist,fp); */
/* This bullshit commented out:
       fprintf(fp,
                  "lists2sparecons: %d ",
                     length(lists2sparecons));
       printexprnl(lists2sparecons,fp);
 */
       return(1);
}


/* --------------------------------------------------------------------- */


int n_fputs(),sputs();

static PFI f_or_sprint = n_fputs;

#define gprint(STR,DST) ((*f_or_sprint)(STR,DST))

maybe_static UINT maxsprintcount,sprintcount;
maybe_static BYTE *sprintdest;

/* out_hex, out_char, out_bytepair - these are functions used by default in
   _INTEGER_PRINTTYPES_ vector for outputting integers with corresponding
   subtypes. They all shoud have two arguments, first which is character
   buffer where stuff to be output is stored, and second is TOB expr,
   which should contain integer to be converted.
   As result they return buf, although it's not currently used.
 */

BYTE *out_hex(buf,expr)
BYTE *buf;
TOB expr;
{
/* output hex number always with precision 4, with leading zeros and uppercase
    digits, and with C-like prefix 0x so for example -1 is output as 0xFFFF
   Note that because Aztec-C version 3.40 doesn't recognize %X as opposed
    to %x in format-strings of printf (to print uppercase instead of lowercase
    hex-digits), we must use itohex function instead (because lowercase
    hex-numbers look awful). In some other C we could just put format-string
    "0x%04X" to _INTEGER_PRINTTYPES_ vector, and this function wouldn't be
    needed at all.
 */
    itohex((buf+2),tob_int(expr));
    *buf     = '0';
    *(buf+1) = 'x';
    return(buf);
}

BYTE *out_char(buf,expr)
BYTE *buf;
TOB expr;
{
    register UINT x;

    x = tob_int(expr);

/* Put quotes around if *quote_flag is on, otherwise don't: */
    if(*quote_flag) { *buf++ = *quote_char; }
/* Now also recognizes wide characters, which are printed high byte first,
    low byte last: (and high byte is masked off) */
    if(x > 255) { *buf++ = gethighbyte(x); x &= 0xFF; }
    if(x) { *buf++ = x; } /* Low byte if it's not zero */
    if(*quote_flag) { *buf++ = *quote_char; }
    *buf = '\0';

    return(buf);
}


BYTE *out_bytepair(buf,expr)
BYTE *buf;
TOB expr;
{
    sprintf(buf,"<%d,%d>",highbyte(tob_int(expr)),lowbyte(tob_int(expr)));
    return(buf);
}

int n_fputs(s,fp)
BYTE *s;
FILE *fp;
{
        BYTE c;
	register UINT count;
	BYTE strangeflag=0;
        /* If printing symbol, and some non-printable char. is printed,
            or symbol is longer than MAXOUTCHARS, or printed nothing
             or printed just single dot (.), then it is little bit weird
           and warningflag is set to WDETECTED_STRANGE,
            if warning system is enabled.
         */

        count = 0;
        while(c = *s)
         {
	   if((*warning_flag == WALERT) && !iscontinuous(s))
            { strangeflag = 1; }

	   if(count > MAXOUTCHARS)
            { putc('$',fp); break; strangeflag = 1; }

           if(*ibmscand_flag) { c = asc_scand2ibm(c); s++; }
/*
           else if((c == substwithblankochar) && printingsymbolflag)
            { c = ' '; s++; }
 */
/* Escape extension is now done for string at reading time:
           else if(*esc_flag && !*quote_flag)
            { s = parse_char(&c,s); }
 */
           else { c = *s++; }

           putc(c,fp);
           count++;
         }
        if(!count) { strangeflag = 1; }
        if((count == 1) && (*(s-2) == '.')) { strangeflag = 1; }
        if(strangeflag && (*warning_flag == WALERT))
         { *warning_flag = WDETECTED_STRANGE; }
}


int sputs(s,dest)
BYTE *s;
BYTE *dest;
{
        BYTE c;

        while(c = *s)
         {
	   if(sprintcount >= maxsprintcount)
            {
              if(dest) { *sprintdest++ = '$'; }
              sprintcount++;
              longjmp(sprint_errbuf,-1); /* Jump back to sprintnexpr */
            }
           if(*ibmscand_flag) { c = asc_scand2ibm(c); s++; }
/*
           else if((c == substwithblankochar) && printingsymbolflag)
            { c = ' '; s++; }
 */
/*
           else if(*esc_flag && !*quote_flag)
            { s = parse_char(&c,s); }
 */
           else { c = *s++; }

           if(dest) { *sprintdest++ = c; }
           sprintcount++;
         }
}

/*
int sputs(buf,dest)
BYTE *buf,*dest;
{
        strcat(dest,buf);
}
 */


int sprintexpr(expr,string)
TOB expr;
BYTE *string;
{
        return(sprintnexpr(expr,string,32767));
}

/* If string is NULL then stuff is not printed anywhere, it is just counted
    how many characters it would take.
 */
int sprintnexpr(expr,string,n)
TOB expr;
BYTE *string;
UINT n;
{
        int sputs();
        PFI save_printfun;

        save_printfun = f_or_sprint;
        f_or_sprint = sputs;

/*      *string = '\0'; */ /* Not needed anymore */
        maxsprintcount = (n-1);
	sprintcount = 0;
        sprintdest  = string;

/* When setting sprint_errbuf returns 0 so printexpr is executed,
    but if coming with longjmp from sputs returns -1 and in that
    case doesn't execute printexpr (for second time): */
        if(!setjmp(sprint_errbuf))
         {
           printexpr(expr,string);
         }

        if(string) { *sprintdest = '\0'; } /* Put the ending zero. */

        f_or_sprint = save_printfun;

        return(sprintcount);
}


/* 15.8.1991 functions princ, prin1 and print modified so that if supplied
   () as second argument they return the count of bytes required to print the
   first argument, but doesn't print it anywhere.
 */

TOB L_princ(expr,port)
TOB expr,port;
{
        register UINT len,savequotesflag;

        savequotesflag = *quote_flag;
        *quote_flag = 0;
        len = genprint(expr,port);
        *quote_flag = savequotesflag;
        return(nilp(port) ? int_tob(len) : expr);
}

TOB L_prin1(expr,port)
TOB expr,port;
{
        register UINT len,savequotesflag;

        savequotesflag = *quote_flag;
        *quote_flag = 1;
        len = genprint(expr,port);
        *quote_flag = savequotesflag;
        return(nilp(port) ? int_tob(len) : expr);
}


TOB L_print(expr,port)
TOB expr,port;
{
        register UINT len,savequotesflag;

        savequotesflag = *quote_flag;
        *quote_flag = 1;
        len = genprint(expr,port);
        *quote_flag = savequotesflag;
        if(endmarkp(port) || portp(port)) { L_terpri(port); }
        return(nilp(port) ? int_tob(len) : expr);
}

/* Don't print nothing if not proper file pointer: */
TOB L_terpri(port)
TOB port;
{
        if(endmarkp(port)) { terpri(stdout); }
        else if(portp(port)) { terpri(tob_fp(port)); }
        return(NIL);
}

TOB print_longcell(cell,port)
TOB cell,port;
{
    TOB x;
    BYTE *s;
    BYTE buf[81]; /* Enough... */

    if(consp(cell) && intp(car(cell)) && intp(cdr(cell)))
     { /* Get the last element, which is a format string for longcells: */
       x = cxr(16,_INTEGER_PRINTTYPES_);
       s = (gen_stringp(x) ? tob_string(x) : byteptr("%lu"));

       sprintf(buf,s,
         (tob_uint(car(cell)) + (((ULI) tob_uint(cdr(cell))) << 16)));
       x = L_princ(string_tob(buf),port);
       return(nilp(port) ? x : cell);
     }
    else { return(L_princ(cell,port)); }
}


int genprint(expr,port)
TOB expr,port;
{
        if(endmarkp(port)) { port = fp_tob(stdout); }
        if(portp(port)) { printexpr(expr,tob_fp(port)); }
        else if(nilp(port) || stringp(port))
         { return(sprintexpr(expr,tob_string(port))); }
        else { inv2arg("genprint",2,expr,port); }
}



printexpr(expr,dest)
TOB expr;
void *dest; /* dest can be FILE-pointer (e.g. stdout) or string */
{
        if(*debug_flag)
         {
           BYTE buf[11];

           gprint(lto2hex(buf,expr,':'),dest);
	   gprint(",",dest);
         }

        if(listp(expr))
         {
           prlist(expr,dest);
         }
        else if(symbolp(expr))
         {
           BYTE buf[11];

           if(*warning_flag != WDISABLED) { *warning_flag = WALERT; }
/*         printingsymbolflag = 1; */
           gprint(pname(expr),dest);
/*         printingsymbolflag = 0; */
           if(*warning_flag == WALERT) { *warning_flag = WENABLED; }
           else if(*warning_flag == WDETECTED_STRANGE)
            {
              fprintf(stderr,
" <-THAT SYMBOL IS A LITTLE BIT WEIRD, IT'S ADDRESS IS: %s ",
               lto2hex(buf,expr,':'));
              *warning_flag = WENABLED;
            }
         }
        else if(intp(expr))
         {
           TOB x; /* Just a temporary work variable */
           BYTE buf[81];

           /* Get the printing form from vector: */
           x = cxr(subtype(expr),_INTEGER_PRINTTYPES_);

           if(nilp(x)) { goto oudot; } /* If not printing type defined */

           if(gen_stringp(x)) /* format_string for sprintf ? */
            {
              sprintf(buf,tob_string(x),tob_uint(expr));
            }
           else if(bcdp(x)) /* If there's C-function defined */
            {
              ((*(tob_fun(x)))(buf,expr));
            }
#ifdef SVL /* If compiled for St. Vitus Lisp */
           else if(consp(x)) /* Lisp function ? */
            {
              TOB apply();
              BYTE savequotesflag;

              /* Call the function with expr as argument:
                 (apply accepts also single atoms for arglist) */
              x = apply(x,expr);

/* Don't use quotes when outputting expression x which lisp-function
    returned, because it may want use strings to represent output:
 */
              savequotesflag = *quote_flag;
              *quote_flag = 0;
              printexpr(x,dest); /* Print the resulting expr */
              *quote_flag = savequotesflag;
              return; /* Exit from printexpr */
            }
#endif
           else /* awful bug */
            {
              fprintf(stderr,
"\n**Internal error: %uth element of *integer-printtypes* is invalid:  ",
                subtype(expr));
              eprint(x);
              goto oudot;
            }
           gprint(buf,dest);
         }
        else /* it is other */
         {
           BYTE buf[41];

           if(endmarkp(expr))
            {
oudot: /* The Strange Ones */
              strcpy(buf,F_other);
              lto2hex((buf+(sizeof(F_other)-1)),expr,':');
              strcat(buf,">");
              gprint(buf,dest);
            }
           else if(portp(expr))
            {
              strcpy(buf,F_fp);
              lto2hex((buf+(sizeof(F_fp)-1)),expr,':');
              strcat(buf,">");
              gprint(buf,dest);
            }
           else if(bcdp(expr))
            {
              strcpy(buf,F_bcd);
              lto2hex((buf+(sizeof(F_bcd)-1)),expr,':');
              strcat(buf,">");
              gprint(buf,dest);
            }
           else /* it is string */
            {
              if(*quote_flag)
               { *(buf+1) = '\0'; *buf = '"'; gprint(buf,dest); }
              gprint(tob_string(expr),dest);
              if(*quote_flag) { gprint(buf,dest); }
            }
         }
}


 
prlist(lista,dest)
TOB lista;
void *dest; /* dest can be FILE-pointer (e.g. stdout) or string */
{
        int begchar,endchar;
        BYTE buf[12];

/* If speclist_flag is on and there's specharp as first item in
    list, then use that and corresponding list-end char for surrounding
    the list instead of normal ( and )
 */
        if(*speclist_flag &&
            specharp(car(lista)) && (begchar = tob_spechar(car(lista))))
         {
           lista = cdr(lista);
         }
        else
         { begchar = LPAR; }

        endchar = getlendchar(begchar);
        
        *(buf+1) = '\0';
        *buf = begchar;
        gprint(buf,dest);
        
        while(lista)
         {
           printexpr(car(lista),dest);
           lista = cdr(lista);
           if(nilp(lista)) { break; }
           if(!consp(lista))
            { /* Not proper list, i.e. it is dotted pair */
              gprint(" . ",dest); /* Print the dot */
              printexpr(lista,dest); /* ... and the cdr-part */
              break;
            }
           gprint(" ",dest);

           if(*debug_flag)
            {
              gprint(lto2hex(buf,lista,':'),dest);
	      gprint("-",dest);
            }
         }

        *buf = endchar;
        *(buf+1) = '\0';
        gprint(buf,dest);
}


printhex(expr,fp)
TOB expr;
FILE *fp;
{
        BYTE buf[21];

        fputs(lto2hex(buf,expr,':'),fp);
}



/* ----------------------------------------------------------------------- */

/* ----------------------------------------------------------------------- */

TOB prstat_stderr()
{
        int i;
        i = 0xBABA; /* Mark which can be found with debugger */
        prstat(stderr);
        return(_T_);
}


/* ------------------------------------------------------------------- */

/* Tests whether jmp_buf is initialized with setjmp. If not then it contains
    just zeros and 0 is returned.
 */
maybe_static int initialized(jump_bufo)
UINT *jump_bufo;
{
        int i=6;

        while(i--)
         { if((*(jump_bufo+i)) != 0) { return(1); } }
        return(0);
}


#ifdef SVL
TOB ertzu(note,expr)
BYTE *note;
TOB expr;
{
        fprintf(stderr,"\n%s",note);
        eprint(expr);
        longjmp(to_toplevel_buf,-1);
}
#endif


TOB _inv1arg(funcname,arg,ret3adr,ret2adr,retadr)
BYTE *funcname;
TOB arg;
FUN_ADR ret3adr,ret2adr,retadr;
{
        BYTE bafu[12],bafu2[12],bafu3[12];

        fprintf(stderr,
         "\n**ERROR: argument for function %s is invalid !\n",
           funcname);
        fprintf(stderr,"%s %s %s\n",
          ftohex(bafu3,ret3adr),ftohex(bafu2,ret2adr),ftohex(bafu,retadr));
        printhex(arg,stderr); putc(' ',stderr);
        eprint(arg);
        if(initialized(to_toplevel_buf)) { longjmp(to_toplevel_buf,-1); }
        prstat_stderr();
        myexit(1);
}



TOB _inv2arg(funcname,nth_one,arg1,arg2,ret3adr,ret2adr,retadr)
BYTE *funcname;
int nth_one;
TOB arg1,arg2;
FUN_ADR ret3adr,ret2adr,retadr;
{
        BYTE bafu[12],bafu2[12],bafu3[12];

        fprintf(stderr,
         "\n**ERROR: %d. argument for function %s is invalid !\n",
         nth_one,funcname);

        fprintf(stderr,"%s %s %s\n",
          ftohex(bafu3,ret3adr),ftohex(bafu2,ret2adr),ftohex(bafu,retadr));
        printhex(arg1,stderr); putc(' ',stderr);
        printhex(arg2,stderr); terpri(stderr);

        fputs("1st arg: ",stderr);
        eprint(arg1);

        fputs("2nd arg: ",stderr);
        eprint(arg2);

        if(initialized(to_toplevel_buf)) { longjmp(to_toplevel_buf,-1); }
        prstat_stderr();
        myexit(1);
}

/* ---------------------------------------------------------------- */


UINT get_cs()
{
#asm
        MOV     AX,CS
#endasm
}


UINT get_ds()
{
#asm
        MOV     AX,DS
#endasm
}

UINT get_es()
{
#asm
        MOV     AX,ES
#endasm
}


UINT get_ss()
{
#asm
        MOV     AX,SS
#endasm
}

#ifdef PASKAA

/* This code doesn't probably work ! */
UINT get_offset_to_ss_segment(a_offset,a_segment)
UINT a_offset,a_segment;
{
        UINT get_ss();

        /* probably a_segment (usually DS) and SS should be near enough
            each other that this worked.... */
        return(a_offset + (16 * (a_segment - get_ss())));
}

#endif
