
/*
;
;   This module defines some global variables for the list subsystem.
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
 
#include "stdio.h"
#include "ctype.h"
#include "setjmp.h"
#include "signal.h"
#include "time.h"
#include "mydefs.h"
#include "lists.h"
#include "fundefs.h"


#define foo 

foo TOB _DEFUN_;            /* "defun" */
foo TOB _DOLLAR_;           /* "$" */
foo TOB _DOT_;              /* "." */
foo TOB _FUNCTION_;         /* "function" */
foo TOB _LAMBDA_;           /* "lambda" */
foo TOB _NLAMBDA_;          /* "nlambda" */
foo TOB _PICK_;             /* "pick" */
foo TOB _QUOTE_;            /* "quote" */
foo TOB _T_;                /* "t" */
foo TOB _UNBOUND_;          /* "*unbound*" */
foo TOB _VLAMBDA_;          /* "vlambda" */
foo TOB _TEST_;             /* ":TEST" */
foo TOB _TEST_NOT_;         /* ":TEST-NOT" */
foo TOB _AUX_;              /* "&aux" */
foo TOB _OPT1_;             /* "&opt" */
foo TOB _OPT2_;             /* "&optional" */
foo TOB _REST_;             /* "&rest" */

foo TOB _DEFUN_KEYWORDS_;   /* (&aux &opt &optional &rest) */

foo TOB _LAMBDAFORMS_;      /* List of (lambda nlambda vlambda) */

foo TOB _oblist   = NIL;    /* Object list */
foo TOB _obtable  = NIL;    /* Hash Table to oblist */
foo TOB _freelist = NIL;    /* Free list */
foo TOB _free_strings_list = NIL; /* Free'ed strings are kept here */

foo ULI _clistsavecnt=0L;

foo jmp_buf readerrbuf;
foo jmp_buf to_toplevel_buf;
foo jmp_buf sprint_errbuf;

foo PTR _lowlim=NULL;
foo PTR _uplim=NULL;
foo PTR _allocp=NULL;

foo ULI fp_lowerlim, fp_upperlim;
foo ULI code_lowerlim, code_upperlim;

foo BYTE _max_args = DEFAULT_MAXARGS;

foo UINT _underflowlim,_overflowlim;
foo UINT _framepointer;

foo TOB _IBASE_;
foo TOB _INTEGER_PRINTTYPES_;
foo TOB _READ_MACROS_;

foo TOB lists2sparecons;

foo BYTE **G_argv;
foo UINT G_argc;

foo UINT _OBTAB_SIZE_;


/* I hope that these are all initialized to zeros (& NILS/NULLS),
   otherwise the results can be quite unexpectable!
 */

foo TOB last_of_freelist; /* =NIL; */
foo UINT freelist_warnings; /* =0; */

foo ULI _consalloc_cnt; /* =0; */
foo ULI _residueconscnt; /* =0; */
foo UINT interncount; /* 0; */

foo TOB  _LINEBUF_;  /* Read line buffer */
foo UINT _LINEBUFSIZE_;
foo TOB  _STATICBUF_; /* For Lisp */

foo TOB _CHARFLAGS_;
foo TOB _IO_FLAGS_;
foo TOB _NEWSYMBVALUE_;

foo BYTE *abrev_char;
foo BYTE *debug_flag;
foo BYTE *dins_flag;
foo BYTE *dtpr_flag;
foo BYTE *esc_flag;
foo BYTE *ibmscand_flag;
foo BYTE *nil_flag;
foo BYTE *plist_flag;
foo BYTE *prefix_char;
foo BYTE *quote_char;
foo BYTE *quote_flag;
foo BYTE *sent_flag;
foo BYTE *speclist_flag;
foo BYTE *warning_flag;

