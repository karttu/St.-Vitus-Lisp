
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
#include "time.h"
#include "mydefs.h"



/* Like fopen, but prints error message and exits if couldn't open
    a file:
 */
FILE *myfopen(filename,moodi)
char *filename,*moodi;
{
        FILE *fopen();
        FILE *z;
        
        if(!filename) { filename = "NULL"; goto bad_habit; }
        if((z = fopen(filename,moodi))) { return(z); }
        else
         {
bad_habit:
           fprintf(stderr,"\nCannot open file %s in mode %s\n",
                    filename,moodi);
           myexit(1);
         }
}



/* This is like fgets, except that:

1) It doesn't put to linebuf newline ('\n') from the end of line read in.
2) It doesn't strip seventh bit away from > 0x7F non-ascii characters
    (because this uses getc function instead of agetc of Aztec-C).

So this puts all the other characters without altering to linebuf, except:
  '\0':s (zeros), which are skipped.
  '\n':s & '\r':s (linefeeds & carriage returns) terminate the line,
   and are NOT put to linebuf.

Note that CTRL-Z is handled just like any other character, so reading
 is not stopped, until true EOF condition is encountered.

Also note that lines of file can be separated with three different ways,
 and this handles all of them:

1) with CR+LF's. ('\r' + '\n'). This is usual MS-DOS usage.
2) with single LF's ('\n').     Used in Unix.
3) with single CR's ('\r').     Some anomalous cases.


By Antti Karttunen at 24.3.1990, modified from fgets function at page 155
 in K/R.
 */

char *myfgets(linebuf, maxline, fp)
char *linebuf;
unsigned int maxline;
FILE *fp;
{
	register int c;
	register char *cs;

        /* If some jerk tries to read 0 characters: */
        if(!maxline) { return(NULL); }

	cs = linebuf;
	while(--maxline) /* Read only (maxline-1) characters, because  */
         {              /* terminating '\0' must fit to linebuf too.  */
do_it_sugo:
           if(!(c = getc(fp))) { goto do_it_sugo; } /* Skip '\0' :s */
           if((c == EOF) || (c == '\n')) { break; }
           /* There is single CR or CR+LF: */
           if(c == '\r')
            {
              /* If next character is not LF, then put it back to input stream
                  for next time: */
              if((c = getc(fp)) != '\n') { ungetc(c,fp); }
              break;
            }
           *cs++ = c;
	 }


	*cs = 0; /* Put the final zero */

        /* If (c == EOF) but (cs != linebuf), then there is still the last
            line of file in linebuf (there wasn't any newline after that)
            so return that, and return EOF next time:
         */
	if((c == EOF) && (cs == linebuf)) { return(NULL); }
	else { return(linebuf); }
}



/* Like fgets, but get rid of that newline hanging in the end */
/* Obsolete:
char *myfgets(line,maxline,fp)
char *line;
int maxline;
FILE *fp;
{
        char *fgets();

        if(!fgets(line,maxline,fp)) { return(NULL); }

        if(*line && (LASTCHAR(line) == '\n')) { LASTCHAR(line) = '\0'; }
        return(line);
}
 */


/* Like fseek, but with error checking: */
int myfseek(stream,offset,origin)
FILE *stream;
register long offset;
register int origin;
{
        int fseek();
        extern int errno;
        register int result;
        
        if(result = fseek(stream,offset,origin))
         {
           fprintf(stderr,
   "**Error in myfseek: returned %d   errno: %d   offset: %ld   origin: %d",
            result,errno,offset,origin);
           myexit(1);
         }
        return(result);
}


long filesize(fp)
FILE *fp;
{
	long ftell();
	long savepos,endpos;

	savepos = ftell(fp);
	myfseek(fp,0L,2); /* Set current position to end of file */
	endpos = ftell(fp);
	myfseek(fp,savepos,0); /* Restore the old position */
	return(endpos);
}


char *filetime(fp)
FILE *fp;
{
        static char datebuf[31];
	long ftime();
	char *ctime();
	long clock;

	clock = ftime(0,fileno(fp));
	strcpy(datebuf,ctime(&clock));
/* There was something dubious in these macros:
	if(GETLASTCHAR(datebuf) == '\n') { SETLASTCHAR(datebuf,'\0'); }
 */
        /* Overwrite the final '\n' with '\0' if there is any: */
        if(*datebuf && (LASTCHAR(datebuf) == '\n'))
         { LASTCHAR(datebuf) = '\0'; }
	return(datebuf);
}


char *daytime()
{
        char *asctime();
        static char date2buf[27];
        struct tm buf;

        dostime(&buf);
        strcpy(date2buf,asctime(&buf));
        /* Overwrite the final '\n' with '\0' if there is any: */
        if(*date2buf && (LASTCHAR(date2buf) == '\n'))
         { LASTCHAR(date2buf) = '\0'; }
        return(date2buf);
}


