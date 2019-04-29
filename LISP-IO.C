
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

extern TOB _CURIN_; /* Heit{ t{m{ globals.h:hon kun jaksat ! */

/*
   lists1.c:st{ poistettuja Lispiin liittyvi{ I/O funktioita
 */

TOB L_iscontinuous(line)
TOB line;
{
    return((iscontinuous(tob_string(line))) ? line : NIL);
}


/* This takes saved previous curin-port from the plist of *curin*
    and sets it value of *curin*. If plist of *curin* is accidentally
    empty, then set *curin* to *stdin*.
 */
restore_curin()
{
    register TOB x;

    setvalue(_CURIN_,
       (nilp(x = plist(_CURIN_)) ? fp_tob(stdin) : car(x)));

/* Of course first list node could also be freed, but I don't care to
    meditate it now: */
    setplist(_CURIN_,cdr(x));
}



TOB infile(filename)
TOB filename;
{
        FILE *fp;

        if(!(fp = fopen(pname(filename),"r")))
         {
           ertzu("infile: can't open file: ",filename);
         }

/* Save old value of the *curin* to head of its plist, and set this port
    its new value:
 */
        setplist(_CURIN_,cons(value(_CURIN_),plist(_CURIN_)));
        setvalue(_CURIN_,fp_tob(fp));
        return(fp_tob(fp));
}


TOB outfile(filename,mode) /* vlambda */
TOB filename,mode;
{
        FILE *fp;

        if(!(fp = fopen(pname(filename),
              (endmarkp(mode) ? "w" : /* Hmm, quite understandable ? */
               ((toupper(*pname(mode)) == 'A') ? "a" : "w")))))
         {
           ertzu("outfile: can't open file: ",filename);
         }
        return(fp_tob(fp));
}

TOB L_close(port)
TOB port;
{
        fclose(tob_fp(port));
        return(NIL);
}

TOB drain(port) /* vlambda */
TOB port;
{
        fflush(endmarkp(port) ? stdout : tob_fp(port));
        return(NIL);
}




TOB L_spaces(n,port)
TOB n,port;
{
        register UINT i;

/* Old code:
        if(endmarkp(port)) { port = fp_tob(stdout); }
        if(portp(port))
         {
            for(i=tob_int(n); i; i--)
             { putc(' ',tob_fp(port)); }
         }
        else { inv2arg("spaces",2,n,port); }
Replaced by:
 */
        i = tob_uint(n);
        while(i--)
         {
           L_princ(char_tob(' '),port);
           if(gen_stringp(port)) { port = addtolow(port,1); }
         }

        return(n);
}

TOB L_read(source,eof)
TOB source,eof;
{
        TOB result;

        if(endmarkp(source)) { source = fp_tob(stdin); eof = NIL; }
        else if(endmarkp(eof)) { eof = NIL; }

        if(portp(source)) { result = readexpr(tob_fp(source)); }
        else if(stringp(source)) { result = sreadexpr(tob_string(source)); }
        else { inv1arg("read",source); }
        return(endmarkp(result) ? 
                 ((portp(source) ? restore_curin() : 0), eof) : result);
}

/* Reads one line from port and puts it into string */
TOB readtostring(string,maxcount,port)
TOB string,maxcount,port;
{
        if(!myfgets(tob_string(string),
                    tob_uint(maxcount),
                    (endmarkp(port) ? stdin : tob_fp(port))))
         { restore_curin(); return(NIL); } /* If End of File, return nil */
        return(string);
}

/* Reads one line from port and builds a list from it */
TOB readline(port,eof) /* vlambda 0-2 */
TOB port,eof;
{
        char puskuri[161]; /* puskuri puskee, p{{ on mahtava alasin. */

        if(endmarkp(port)) { port = fp_tob(stdin); eof = NIL; }
        else if(endmarkp(eof)) { eof = NIL; }

        if(stringp(port)) { strcpy((puskuri+1),tob_string(port)); }
        else if(portp(port))
         {
           if(!myfgets((puskuri+1),81,tob_fp(port)))
            { restore_curin(); return(eof); }
         }
        *puskuri = '(';
        strcat(puskuri,")");
        return(sreadexpr(puskuri));
}

/* 4-Jan-1991.
   New functions tyi, tyipeek and tyo for reading & writing binary bytes.
   Trying to be Franz Lisp compatible, except that tyi and tyipeek
   without the arguments get the character from current read buffer.
   In that mode they can be used by read macros to examine what characters
   are following in current read line.
 */

TOB tyi(port,exp) /* vlambda 0 - 2 */
TOB port,exp;
{
    UINT c;

    if(endmarkp(port))
     {
       extern UINT getnextchar();

       if(!(c = getnextchar()))
        { restore_curin(); return(int_tob(-1)); }
       else return(char_tob(c));
     }
    else
     {
       if((c = fgetc(tob_fp(port))) == EOF)
        { restore_curin(); return(endmarkp(exp) ? int_tob(-1) : exp); }
       else return(char_tob(c));
     }
}


TOB tyipeek(port,exp) /* vlambda 0 - 2 */
TOB port,exp;
{
    UINT c;

    if(endmarkp(port))
     {
       extern BYTE *lineptr;

       return(char_tob(*lineptr));
     }
    else
     {
       c = fgetc(tob_fp(port));
       ungetc(c, tob_fp(port));
       if(c == EOF)
        { return(endmarkp(exp) ? int_tob(-1) : exp); }
       else return(char_tob(c));
     }
}



TOB tyo(fixnum,port) /* vlambda 1 - 2 */
TOB fixnum,port;
{
    fputc(tob_uint(fixnum),(endmarkp(port) ? stdout : tob_fp(port)));

    return(fixnum);
}


/* Load transferred to this file at 4-Jan-1991: */

TOB load(arg)
TOB arg;
{
        FILE *fopen();
        char *index();
        FILE *fp;
        register TOB expr;
        TOB lista;
        char s[88];
        int exprcnt,defcnt;

        strncpy(s,pname(arg),81);

        /* If user omitted extension then use LSP by the default: */
        if(!index(s,'.')) { strcat(s,".lsp"); }

/* This commented out, use infile instead:
        if(!(fp = fopen(s,"r")))
         {
           fprintf(stderr,
            "\nload: cannot open file %s in mode %s !\n",s,"r");
           return(NIL);
         }
 */
        fp = tob_fp(infile(string_tob(s)));

        fprintf(stderr,"\n**Loading file %s\n",s);

        lista = NIL;
        exprcnt = defcnt = 0;


/* This could be edited so that also the names of global variables set with
    setq would be printed too: (but with different way)
 */
        while(!endmarkp(expr = readexpr(fp)))
         {
           if(consp(expr) && eq(car(expr),_DEFUN_))
            {
              lista = cons(print(eval(expr)),lista);
              defcnt++;
            }
           else { eval(expr); }
           exprcnt++;
         }
        fclose(fp);
        restore_curin();

        if(nonnilsymbolp(arg)) { setvalue(arg,nreverse(lista)); }
        else { /* print(lista); */ free_list(lista); }

        return(int_tob(exprcnt));
}

