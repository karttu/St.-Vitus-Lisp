
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

/* St. Vitus' Lisp, lisp interpreter for MS-DOS machines.
   Copyright (C) 1991  Antti Karttunen.
   This file LISP.C contains apply, eval and some other essential
   functions for the interpreter, together with some initialization
   code for functions & some symbols, and with toplevel
   read-eval-print loop.
   Coding of this file started at 11. March 1989.
 */

#include "linclude.h"


TOB _CURIN_; /* Muuta t{m{ johonkin toiseen failiin (globals.c ?) sitten
 kun jaksat, samoin lisp-io.c:st{ extern TOB _CURIN_ globals.h:hon */

ljmp_buf retubuf;
#define RETUBUF_END (((UINT *) retubuf) + (LJBUFSIZE/2))


extern BYTE _Uorg,_Uend;
extern UINT _HEAPSIZ,_STKSIZ,_STKLOW,_STKRED;

/* arithmetic functions: */
/*
TOB add1(),sub1(),plus(),difference(),times(),quotient(),remainder();
TOB vplus();
 */

/* predicates in alphabetical order: (14) */
/*
TOB L_atom(),L_boundp(),L_compactp(),L_consp(),L_endmarkp(),L_eq(),L_equal();
TOB L_greaterp(),L_intp(),L_lessp(),L_listp(),L_nilp(),L_nonnilsymbolp();
TOB L_otherp(),L_symbolp();
TOB L_zerop();
 */

#define MAXLOADFILES 101 /* Enough ? ;-) */

BYTE *files_to_load[MAXLOADFILES+1];
BYTE **p_to_filenames = files_to_load;

main(argc,argv)
UINT argc;
BYTE **argv;
{
    int i,j;
    ULI atol();


    for(i = 1, j = 0; argv[i]; )
     {
       if((*(argv[i]) == '-') && (tolower(*(argv[i]+1)) == 'l'))
        {
          if(*(argv[i]+2))
           {
             if(j < MAXLOADFILES)
              { files_to_load[j++] = (argv[i]+2); }
/* I'm too tired now to code function move_args_left, and I don't
   know whether it's good idea at all to delete -lfilename.lsp
   arguments from the command line.
             move_args_leftward(argv[i],1);
             argc--;
 */
             i++;
           }
          else
           {
             if(j < MAXLOADFILES)
              { files_to_load[j++] = (argv[++i]); }
             i++;
 /*
             move_args_leftward(argv[i],2);
             argc -= 2;
  */       }
        }
       else { i++; }
     }

    files_to_load[j] = NULL; /* End marker */
    G_argv = argv;
    G_argc = argc;

    initlists(0L);
    initsymbols();

    /* Unmeditated shit: */
    _underflowlim = get_sp();
    _overflowlim  = _underflowlim;
/* (get_offset_to_ss_segment(&_Uend) + _STKRED); */

    fprintf(stderr,
"St. Vitus's Lisp (%u)  Copyright (C) 1991 A. Karttunen. Enter (help) for help.\n",
      initfuns());

    REP_loop();
}

initsymbols()
{
    _LAMBDA_   = intern("lambda");
    _NLAMBDA_  = intern("nlambda");
    _VLAMBDA_  = intern("vlambda");
/*  _FUNCTION_ = intern("function"); */
/*  _PICK_     = intern("pick");     */
    _DEFUN_    = intern("defun");
    _DOLLAR_   = intern("$");
    _TEST_     = intern(":test");
    _TEST_NOT_ = intern(":test-not");
    _AUX_      = intern("&aux");
    _OPT1_     = intern("&opt");
    _OPT2_     = intern("&optional");
    _REST_     = intern("&rest");

    setvalue(_TEST_,_TEST_);
    setvalue(intern(":TEST"),_TEST_);
    setvalue(intern(":TEST-NOT"),_TEST_NOT_);
    setvalue(_TEST_NOT_,_TEST_NOT_);

    setvalue(_AUX_,_AUX_);
    setvalue(_OPT1_,_OPT1_);
    setvalue(_OPT2_,_OPT2_);
    setvalue(_REST_,_REST_);

    _LAMBDAFORMS_ = list3(_LAMBDA_,_NLAMBDA_,_VLAMBDA_);
    setvalue(intern("*lambdaforms*"),_LAMBDAFORMS_);

    _DEFUN_KEYWORDS_ = list4(_AUX_,_OPT1_,_OPT2_,_REST_);

    setvalue(intern("*stdin*"),other_tob(stdin));
/* *curin* contains always current input port, stdin by default.
    infile and load sets it. Must be used by read macros if they want
    to read stuff from the same port than they are read from,
     e.g: (read *curin*)
 */
    setvalue((_CURIN_ = intern("*curin*")),other_tob(stdin));
    setvalue(intern("*stdout*"),other_tob(stdout));
    setvalue(intern("*stderr*"),other_tob(stderr));
    setvalue(intern("*screen*"),string_tob(0xB0008000L));
    pokeb((0xB0008000L + 4000),0); /* Set end '\0' for string *screen* */
/* Character table ctp_[] used by isalpha, isdigit, etc. of ctype.h
   Note that this is inherently buggy. If ctp_ is lying on the segment
   boundary, then functions follow *ctp* to the wrong location: */
    setvalue(intern("*ctp*"),string_tob(ctp_));

    setvalue(intern("*staticbuf*"),
               (_STATICBUF_ = new_string(int_tob(STATICBUFSIZE+3))));
}


struct fun_entry
 {
   char *internal_name;
   TOB *adrofdiscipline;
   int argcount;
   PFTOB address;
 };



/* static */ struct fun_entry fun_table[] =
 {
/* Fundamental list-functions, car, cdr & cons: */
   { "car",            &_LAMBDA_, 1, car },
   { "cdr",            &_LAMBDA_, 1, cdr },
   { "prevcdr",        &_LAMBDA_, 1, prevcdr },
   { "cons",           &_LAMBDA_, 2, cons },
   { "symeval",        &_LAMBDA_, 1, value },
   { "plist",          &_LAMBDA_, 1, plist },    /* Get property-list field */
   { "setplist",       &_LAMBDA_, 2, setplist }, /* Set property-list field */
   { "setxbit",        &_LAMBDA_, 2, setxbit },
   { "getxbit",        &_LAMBDA_, 1, getxbit },
   { "xbitp",          &_LAMBDA_, 1, xbitp },
/* Some list functions: */
   { "append",         &_LAMBDA_, 2, append },
   { "list",           &_VLAMBDA_,0, listn },
   { "clist",          &_VLAMBDA_,0, clistn },
   { "last",           &_LAMBDA_, 1, last },
   { "length",         &_LAMBDA_, 1, L_length },
   { "member",         &_VLAMBDA_,4, member }, /* 2 to 4 args */
   { "memq",           &_LAMBDA_, 2, memq },
   { "memqnth",        &_LAMBDA_, 2, nthmemq },
   { "nth",            &_LAMBDA_, 2, L_nth },
   { "nthcdr",         &_LAMBDA_, 2, L_nthcdr },
   { "qnth",           &_LAMBDA_, 2, L_qnth },
   { "cxr",            &_LAMBDA_, 2, L_qnth },
   { "qnthcdr",        &_LAMBDA_, 2, L_qnthcdr },
   { "rplacx",         &_LAMBDA_, 3, L_rplacx },
   { "reverse",        &_LAMBDA_, 1, reverse },
   { "topcopy",        &_LAMBDA_, 1, topcopy },
   { "ctopcopy",       &_LAMBDA_, 1, ctopcopy },
/* Functions for compact-lists: */
   { "clistsave",      &_LAMBDA_, 1, clistsave },
   { "new-clist",      &_VLAMBDA_,2, L_new_clist },

/* Some destructive list functions: */
   { "attach",         &_LAMBDA_, 2, attach },
   { "rplaca",         &_LAMBDA_, 2, rplaca },
   { "rplacd",         &_LAMBDA_, 2, rplacd },
   { "nconc",          &_VLAMBDA_,0, L_nconc },
   { "putlast",        &_LAMBDA_, 2, putlast },
   { "movelist",       &_LAMBDA_, 2, movelist },
   { "nreverse",       &_LAMBDA_, 1, nreverse },
   { "dsubst",         &_LAMBDA_, 3, dsubst },
   { "*dsubst",        &_LAMBDA_, 3, idsubst },
/* { "deepdel",        &_LAMBDA_, 2, deepdel }, Buggy code in lists2.c ! */
   { "delnth",         &_LAMBDA_, 2, L_delnth },
   { "delete",         &_VLAMBDA_,3, L_delete },
   { "delq",           &_VLAMBDA_,3, L_delq },
   { "dremove",        &_VLAMBDA_,3, L_delq },
   { "delete-if",      &_VLAMBDA_,3, L_delete_if },
   { "delete-if-not",  &_VLAMBDA_,3, L_delete_if_not },
   { "linsert",        &_LAMBDA_, 3, L_list_insert },
   { "mcl",            &_LAMBDA_, 1, mcl },
/* Value modifying functions: */
   { "set",            &_LAMBDA_, 2, setvalue },
   { "setq",           &_NLAMBDA_,1, setq },
   { "defun",          &_NLAMBDA_,1, defun },
   { "get",            &_LAMBDA_, 2, get },
   { "getproploc",     &_LAMBDA_, 2, getproploc },
   { "remprop",        &_LAMBDA_, 2, remprop },
   { "putprop",        &_LAMBDA_, 3, putprop },
/* Functions for evaluating & applying, and so on: */
   { "eval",           &_LAMBDA_, 1, eval },
   { "apply",          &_LAMBDA_, 2, apply },
   { "funcall",        &_NLAMBDA_,1, funcall },
/* Control-forms, cond, and, or, etc. */
   { "cond",           &_NLAMBDA_,1, cond },
   { "if",             &_NLAMBDA_,1, L_if },
   { "and",            &_NLAMBDA_,1, and },
   { "or",             &_NLAMBDA_,1, or },
   { "progn",          &_NLAMBDA_,1, eval_list },
   { "while",          &_NLAMBDA_,1, L_while },
   { "return",         &_VLAMBDA_,1, L_return },
   { "mapc",           &_VLAMBDA_,0, mapc },
   { "mapcar",         &_VLAMBDA_,0, mapcar },
   { "mapcan",         &_VLAMBDA_,0, mapcan },
   { "maprplaca",      &_VLAMBDA_,0, LL_maprplaca },
/* I/O - functions */
   { "infile",         &_LAMBDA_, 1, infile },
   { "outfile",        &_VLAMBDA_,2, outfile },
   { "close",          &_LAMBDA_, 1, L_close },
   { "drain",          &_VLAMBDA_,1, drain },
   { "read",           &_VLAMBDA_,2, L_read },
   { "readtostring",   &_VLAMBDA_,3, readtostring },
   { "readline",       &_VLAMBDA_,2, readline },
   { "print",          &_VLAMBDA_,2, L_print },
   { "prin1",          &_VLAMBDA_,2, L_prin1 },
   { "princ",          &_VLAMBDA_,2, L_princ },
   { "terpri",         &_VLAMBDA_,1, L_terpri },
   { "spaces",         &_VLAMBDA_,2, L_spaces },
   { "prinx",          &_VLAMBDA_,2, prinx },
   { "print-longcell", &_VLAMBDA_,2, print_longcell },
   { "load",           &_LAMBDA_, 1, load },
   { "tyi",            &_VLAMBDA_,2, tyi },     /* 0 - 2 */
   { "tyipeek",        &_VLAMBDA_,2, tyipeek }, /* 0 - 2 */
   { "tyo",            &_VLAMBDA_,2, tyo },     /* 1 - 2 */
   { "zapline",        &_LAMBDA_, 0, zapline_rm },
   { "iscontinuousp",  &_LAMBDA_, 1, L_iscontinuous },
   { "set-lineptr",    &_LAMBDA_, 1, set_lineptr },
   { "read-next-to-string",
                       &_LAMBDA_, 2, read_next_to_string },

   { "intern",         &_LAMBDA_, 1, L_intern },
   { "lookup",         &_LAMBDA_, 1, L_lookup },
   { "reset-readmode", &_LAMBDA_, 0, reset_readmode },
/* string functions: */
/* { "free-string",    &_LAMBDA_, 1, free_string }, Obsolete ! */
   { "new-string",     &_LAMBDA_, 1, new_string },
   { "@",              &_VLAMBDA_,2, fetch_char },
   { "@=",             &_VLAMBDA_,3, set_char },
   { "strcat",         &_LAMBDA_, 2, L_strcat },
   { "strncat",        &_LAMBDA_, 2, L_strncat },
   { "strcpy",         &_LAMBDA_, 2, L_strcpy },
   { "strncpy",        &_LAMBDA_, 2, L_strncpy },
   { "strcmp",         &_LAMBDA_, 2, L_strcmp },
   { "strncmp",        &_LAMBDA_, 2, L_strncmp },
   { "strchr",         &_LAMBDA_, 2, L_strchr },
   { "strrchr",        &_LAMBDA_, 2, L_strrchr },
   { "strlen",         &_LAMBDA_, 1, L_strlen },
   { "strdup",         &_LAMBDA_, 1, L_strdup },
   { "strequ",         &_LAMBDA_, 2, L_strequ },
   { "*strequ",        &_LAMBDA_, 2, L_monostrequ },
   { "streq",          &_LAMBDA_, 2, L_streq },
   { "*streq",         &_LAMBDA_, 2, L_monostreq },
   { "strmatchp",      &_LAMBDA_, 2, strmatchp },
   { "*strmatchp",     &_LAMBDA_, 2, strmatchp2 },
   { "strfmatchp",     &_LAMBDA_, 3, strfmatchp },
   { "istrmatchp",     &_LAMBDA_, 2, istrmatchp },
   { "convert-string", &_LAMBDA_, 2, L_convert_string },
   { "substchars",     &_LAMBDA_, 3, L_substchars },
   { "delchars",       &_LAMBDA_, 2, L_delchars },
   { "isalphap",       &_LAMBDA_, 1, isalphap },
   { "isupperp",       &_LAMBDA_, 1, isupperp },
   { "islowerp",       &_LAMBDA_, 1, islowerp },
   { "isdigitp",       &_LAMBDA_, 1, isdigitp },
   { "isxdigitp",      &_LAMBDA_, 1, isxdigitp },
   { "isalnump",       &_LAMBDA_, 1, isalnump },
   { "isspacep",       &_LAMBDA_, 1, isspacep },
   { "ispunctp",       &_LAMBDA_, 1, ispunctp },
   { "iscntrlp",       &_LAMBDA_, 1, iscntrlp },
   { "isprintp",       &_LAMBDA_, 1, isprintp },
   { "isgraphp",       &_LAMBDA_, 1, isgraphp },
   { "isasciip",       &_LAMBDA_, 1, isasciip },
   { "isoctdigitp",    &_LAMBDA_, 1, isoctdigitp },
   { "tolower",        &_LAMBDA_, 1, L_tolower },
   { "toupper",        &_LAMBDA_, 1, L_toupper },
   { "ibm2sevenbit",   &_LAMBDA_, 1, L_ibm2sevenbit },
   { "sevenbit2ibm",   &_LAMBDA_, 1, L_sevenbit2ibm },
   { "setscandmode",   &_LAMBDA_, 1, setscandmode },
/* Non-returning functions */
   { "exit",           &_VLAMBDA_,1, quit },
   { "reset",          &_LAMBDA_, 0, reset },

/* Composites of the car & cdr: */
   { "caar",           &_LAMBDA_, 1, caar },
   { "cadr",           &_LAMBDA_, 1, cadr },
   { "cdar",           &_LAMBDA_, 1, cdar },
   { "cddr",           &_LAMBDA_, 1, cddr },
/* Predicates: */
   { "atom",           &_LAMBDA_, 1, L_atom },
   { "boundp",         &_LAMBDA_, 1, L_boundp },
   { "compactp",       &_LAMBDA_, 1, L_compactp },
   { "consp",          &_LAMBDA_, 1, L_consp },
   { "endmarkp",       &_LAMBDA_, 1, L_endmarkp },
   { "eq",             &_LAMBDA_, 2, L_eq },
   { "equal",          &_LAMBDA_, 2, L_equal },
   { "greaterp",       &_LAMBDA_, 2, L_greaterp },
   { "intp",           &_LAMBDA_, 1, L_intp },
   { "lessp",          &_LAMBDA_, 2, L_lessp },
   { "listp",          &_LAMBDA_, 1, L_listp },
   { "neq",            &_LAMBDA_, 2, L_neq },
   { "nilp",           &_LAMBDA_, 1, L_nilp },
   { "not",            &_LAMBDA_, 1, L_nilp },
   { "null",           &_LAMBDA_, 1, L_nilp },
   { "nonnilsymbolp",  &_LAMBDA_, 1, L_nonnilsymbolp },
   { "otherp",         &_LAMBDA_, 1, L_otherp },
   { "bcdp",           &_LAMBDA_, 1, L_bcdp },
   { "portp",          &_LAMBDA_, 1, L_portp },
   { "symbolp",        &_LAMBDA_, 1, L_symbolp },
   { "stringp",        &_LAMBDA_, 1, L_stringp },
   { "*stringp",       &_LAMBDA_, 1, L_gen_stringp },
   { "zerop",          &_LAMBDA_, 1, L_zerop },
   { "minusp",         &_LAMBDA_, 1, L_minusp },
   { "plusp",          &_LAMBDA_, 1, L_plusp },
   { "sub-while-plusp",&_LAMBDA_, 2, sub_while_plusp },
/* Arithmetic functions: */
   { "add1",           &_LAMBDA_, 1, add1 },
   { "sub1",           &_LAMBDA_, 1, sub1 },
   { "+",              &_LAMBDA_, 2, plus },
   { "-",              &_LAMBDA_, 2, difference },
   { "*",              &_LAMBDA_, 2, times },
   { "/",              &_LAMBDA_, 2, quotient },
   { "%",              &_LAMBDA_, 2, remainder },
   { "neg",            &_LAMBDA_, 1, _neg }, /* Unary minus */
   { "sxt",            &_LAMBDA_, 1, _sxt }, /* Sign Extend */
   { "$not",           &_LAMBDA_, 1, _not }, /* Bit complement */
   { "$and",           &_LAMBDA_, 2, _and },/* Bit and, or, xor and shifts: */
   { "$or",            &_LAMBDA_, 2, _or },
   { "$xor",           &_LAMBDA_, 2, _xor },
   { "$shl",           &_LAMBDA_, 2, _shl },
   { "$shr",           &_LAMBDA_, 2, _shr },
   { "$sar",           &_LAMBDA_, 2, _sar },
   { "plus",           &_VLAMBDA_,0, vplus }, /* Variable arg-count plus */
   { "times",          &_VLAMBDA_,0, vtimes },
/* { "$bytepair",      &_LAMBDA_, 1, xbytepair }, */
   { "subtype",        &_LAMBDA_, 1, L_subtype },
   { "setsubtype",     &_LAMBDA_, 2, L_setsubtype },
   { "pick",           &_LAMBDA_, 1, pick },
   { "intran",         &_LAMBDA_, 1, L_intran },
   { "setseed",        &_VLAMBDA_,1, L_setseed },
/* Miscellaneous: */
   { "apro",           &_VLAMBDA_,1, apropos },
   { "argv",           &_LAMBDA_, 1, L_argv },
   { "help",           &_LAMBDA_, 0, help },
   { "dir",            &_VLAMBDA_,1, dir },
   { "getenv",         &_LAMBDA_, 1, L_getenv },
   { "shell",          &_VLAMBDA_,1, shell },
   { "sleep",          &_LAMBDA_, 1, L_sleep },
   { "end*mark",       &_LAMBDA_, 0, get_endmark }, /* For nice tricks */
   { "pick-subst",     &_LAMBDA_, 2, pick_subst },
   { "handle-farglist",&_LAMBDA_, 1, handle_farglist },
   { "free-cons",      &_LAMBDA_, 1, free_cons },
   { "free-list",      &_LAMBDA_, 1, free_list },

   { "switch-compact-bit",  &_LAMBDA_, 2, switch_compact_bit },
   { "set-max-args",   &_LAMBDA_, 1, set_max_args },
/* Debugging & Testing functions: */
   { "daytime",        &_LAMBDA_, 0, L_daytime },
   { "seconds-of-day", &_LAMBDA_, 0, L_seconds_of_day },
   { "hsecs",          &_LAMBDA_, 0, L_hsecs },
   { "memstat",        &_LAMBDA_, 0, prstat_stderr },
   { "mem-free",       &_VLAMBDA_,1, mem_free },
   { "mem-used",       &_VLAMBDA_,1, mem_used },
   { "get-free-list",  &_LAMBDA_, 0, get_free_list },
   { "gfp",            &_LAMBDA_, 0, gfp },
   { "deb",            &_VLAMBDA_,1, debug_info },
   { "fake",           &_LAMBDA_, 2, fake },

   { NULL,             NULL,      0, ((PFTOB) NULLFP) /* end marker */ }
 };


int initfuns()
{
        struct fun_entry *p;
        int count=0;
        char *name;

        p = fun_table; /* address of first element */

        while((name = p->internal_name) != NULL)
         {
           setvalue(intern(name),
                     list3((*(p->adrofdiscipline)),
                           int_tob(p->argcount),
                           fun_tob(p->address)));
           p++;
           count++;
         }

        return(count);
}



#define buf_size 34

REP_loop() /* Read-Eval-Print loop */
{
    TOB x;
    TOB pick_buf[buf_size];

    /* Clear pick_buf area to zeros, i.e. NIL's: */
    memset(pick_buf,0,(buf_size*sizeof(TOB))); /* Is this unix-function ? */
    /* Assign _framepointer to pick_buf, so that if user meddles with pick
        at toplevel, then that won't crash the system.
     */
    _framepointer = getlow(pick_buf);
    pick_buf[1] = intern("Hello!"); /* Just debugging... */

/* Assign to_toplevel_buf to jump to here, and also assign retubuf first
    to point here so that if user enters (return something) at toplevel
    (not in while-loop as it should), then interpreter returns control
    here in that case also, and sets something to be value of $
 */
    if(setjmp(to_toplevel_buf) ||
       !endmarkp(setvalue(_DOLLAR_,lsetjmp(retubuf)))) /* I hope this works */
     {
       fprintf(stderr,"\n**Back to toplevel.\n");
     }
    else { setvalue(_DOLLAR_,NIL); }

    signal(SIGINT,sighandler);

    while(*p_to_filenames)
     {
       /* Make sure that p_to_filenames points already to the next file to be
          loaded, when it loads current file, so that if there's reset, return
          or some error in that file, and when control returns to toplevel 
          (i.e. here) interpreter doesn't try to load same file again: */
       p_to_filenames++;
       fprintf(stderr,"\n**Loaded %u expression.\n",
                tob_uint(load(string_tob(*(p_to_filenames-1)))));
     }

    while(1)
     {
       fputs("->",stderr);
       setvalue(_DOLLAR_,(x = eval(readexpr(stdin))));
       print(x);
     }
}



void sighandler(sig)
int sig;
{
/*      char bafu[20]; */
        UINT c;

        signal(SIGINT,sighandler);
        fprintf(stderr,
"\n**Sighandler: Received signal %u. C - Continue, R - Reset, X - Exit to DOS,"
                  ,sig);
        fprintf(stderr,
"\nT - Like R but don't reset IO>");
        if(feof(stdin) || ferror(stdin)) { myexit(1); }
        c = scr_getc(); /* Use this so that F3-buffer is left intact */
/*      myfgets(bafu,10,stdin); */
        if(toupper(c) == 'X') { myexit(1); } /* X as eXit */
        else if(toupper(c) == 'C') { return; } /* C as Continue */
        else if(toupper(c) == 'R') { reset_readmode(); }
        /* else it's T or any other */
        reset();
}


#define _eapply(fun,arglist) gapply(fun,arglist,evalargs)


TOB eval(expr)
register TOB expr;
{
/*      UINT stack_pointer; */

        if(pickp(expr))
         {
           return(pickl(_framepointer+tob_uint(expr)));
         }

        if(intp(expr) || nilp(expr) || otherp(expr)) { return(expr); }

/*      stack_pointer = get_sp(); */

/* I think this code doesn't work in any case !
        if(stack_pointer >= _underflowlim)
         {
           fputs("\n\r**eval: stack underflow !",stderr);
           goto ressu;
         }
        else if(stack_pointer <= _overflowlim)
         {
           fputs("\n\r**eval: stack overflow !",stderr);
ressu:
           debug_info(ENDMARK);
           reset();
         }
 */

        if(symbolp(expr))
         {
           TOB val;

           val = value(expr);
           if(eq(val,_UNBOUND_))
            {
              ertzu("eval: Unbound Variable: ",expr);
              reset();
            }
           else { return(val); }
         }
        if(consp(expr))
         {
           register TOB first;
           first = car(expr);
           /* This is for the longcells: */
           if(intp(first)) { return(expr); }
           if(eq(first,_QUOTE_) || eq(first,_FUNCTION_))
            { return(cadr(expr)); }
/* Obsolete code:
           if(eq(first,_PICK_))
            {
              expr = cadr(expr);
              if(!intp(expr)) { expr = eval(expr); }
              return(pickl(_framepointer+(tob_int(expr) << 2)));
            }
 */
           else { return(_eapply(first,cdr(expr))); }
         }
}


TOB apply(fun,arglist)
TOB fun;
TOB arglist;
{
        return(gapply(fun,arglist,NULLFP));
}

TOB funcall(argl) /* nlambda */
TOB argl;
{
        return(_eapply(eval(car(argl)),cdr(argl)));
}




TOB eapply(fun,arglist)
TOB fun;
TOB arglist;
{
        return(gapply(fun,arglist,evalargs));
}


/* General Apply Function */
TOB gapply(fun,arglist,eval_args_fun)
register TOB fun;
TOB arglist;
PFI eval_args_fun;
{
        TOB discipline,body,result;
        UINT argcnt,savesp;

        if(nonnilsymbolp(fun)) { body = fun; fun = value(fun); }
        if(!consp(fun))
         {
           ertzu("gapply: Undefined Function: ",body);
           reset();
         }

        discipline = car(fun);
        argcnt = tob_uint(cadr(fun));
/*      body = car(cddr(fun)); */
        body = cddr(fun);

/* Although Aztec-C compiles instructions PUSH BP & MOV BP,SP to beginning
   of functions, this is still needed: */
        savesp = get_sp();    /* Save the sp */

        if(eq(discipline,_LAMBDA_))
         { goto hevoinen; }
        else if(eq(discipline,_NLAMBDA_))
         {
           pushl(arglist); /* Just push arglist without evaluating */
         }
        else if(eq(discipline,_VLAMBDA_))
         { /* Tell pushargs that this is vlambda by making
               high byte of argcnt non-zero: */
           argcnt |= 256;
hevoinen:
           argcnt = pushargs(arglist,argcnt);
           if(eval_args_fun != NULLFP) { ((*eval_args_fun)(argcnt)); }
         }
        else
         {
           ertzu("gapply: Invalid Discipline: ",discipline);
         }

/*      if(otherp(body)) { result = ((*(tob_fun(body)))()); } */
        if(otherp(result = car(body)))
         { result = ((*(tob_fun(result)))()); }
        else
         {
           /* (at this point sp points to first argument) */
           pushw(_framepointer); /* Save the old _framepointer */
           /* (but at this point sp points to first_arg-2) */

           /* And now get the new _framepointer. It should point four bytes
            *  "upward" (in stack sense, i.e. in reality minusward)
            *    from first argument:
            */
           _framepointer = (get_sp() - 2);
/*         result = eval(body); */
           result = eval_list(body); /* Evaluate 'extended' lambda-form */
           _framepointer = popw(); /* Restore the old _framepointer */
         }

/* This is needed because Aztec-C compiles POP SI & POP DI before those
     MOV SP,BP  and  POP BP  and if sp points to wrong place then SI & DI
      are corrupted: (from viewpoint of calling function)
 */
        set_sp(savesp);    /* Restore the sp */

        return(result);
}


TOB quit(status)
TOB status;
{
/*      fprintf(stderr,"\nExiting.\n"); */
        myexit(endmarkp(status) ? 0 : tob_uint(status));
}


TOB setq(args)
TOB args;
{
        register TOB x,y;

        x = car(args);
        y = eval(cadr(args));

        if(pickp(x))
         {
           return(pokeltostack((_framepointer+(tob_uint(x))),y));
         }
        else { return(setvalue(x,y)); }
}


TOB reset()
{
        longjmp(to_toplevel_buf,-1);
}

/* ---------------------------------------------------------------- */

/* Do equalize(cdr_of_clause,arglist), because we want to save stack space */
#define cdr_of_clause arglist

TOB cond(arglist)
register TOB arglist;
{
        TOB clause,res;

        while(!nilp(arglist))
         {
           clause = car(arglist);
           if(!nilp(res = eval(car(clause))))
            { /* If clause is just ((test)), then return value of (test): */
              if(nilp(cdr_of_clause = cdr(clause))) { return(res); }
              else { return(eval_list(cdr_of_clause)); }
/*            else { return(eval(car(cdr_of_clause))); } */
            }
           arglist = cdr(arglist);
         }
        return(NIL);
}

/*
    (if a b) is equivalent to (cond (a b))
    (if a b c ...) is equivalent to (cond (a b) (t c ...))
 */
TOB L_if(args) /* nlambda */
/* register */ TOB args;
{  /* If first expression is true, then return second expr. evaluated: */
        if(!nilp(eval(car(args)))) { return(eval(cadr(args))); }
   /* Otherwise execute rest of expressions and return value of last: */
        else { return(eval_list(cddr(args))); }
   /* Note that if expression is of the form (if a b) then cddr(args)
       is nil, and eval_list returns nil for that. */
}


TOB and(argl)
register TOB argl;
{
        TOB res;

        while(!nilp(argl))
         {
           res = eval(car(argl));
           if(nilp(res)) { return(NIL); }
           argl = cdr(argl);
         }
        return(res);
}


TOB or(argl)
register TOB argl;
{
        TOB res;

        while(!nilp(argl))
         {
           res = eval(car(argl));
           if(!nilp(res)) { return(res); }
           argl = cdr(argl);
         }
        return(NIL);
}


TOB L_while(argl)
TOB argl;
{
        register TOB test;
        TOB body;
        register int i=0;
        register UINT *p,*q;
        UINT save_framepointer;
        ljmp_buf save_retubuf;

        save_framepointer = _framepointer;
        /* Move retubuf to save_retubuf: */
        for((p = (UINT *) retubuf),(q = (UINT *) save_retubuf);
              (p < RETUBUF_END);)
         { *q++ = *p++; }

        if(endmarkp(test = lsetjmp(retubuf)))
         {
           test = car(argl);
           body = cdr(argl);

           while(!nilp(eval(test)))
            {
              eval_list(body);
              i++;
            }

           test = int_tob(i);
         }

        _framepointer = save_framepointer;
        /* Move save_retubuf to retubuf: */
        for((p = (UINT *) retubuf),(q = (UINT *) save_retubuf);
              (p < RETUBUF_END);)
         { *p++ = *q++; }

        return(test);
}

TOB L_return(arghu) /* vlambda */
TOB arghu;
{
        llongjmp(retubuf,(endmarkp(arghu) ? NIL : arghu));
}


/* Evaluate expressions in list, and returns result of last one.
    (Auxiliary function of eval,cond & L_while) */
/* By The Way, this is just progn ! (when called as nlambda) */
TOB eval_list(lista)
register TOB lista;
{
        TOB res;

        res = NIL; /* If lista is (accidentally ?) NIL */

        while(!nilp(lista))
         {
           res = eval(car(lista));
           lista = cdr(lista);
         }
        return(res);
}


/* Auxiliary function for map-functions: (modified from listn)
   Make list from the car's of those lists in the stack,
   and advance them by one, by cdr'ing them to next list nodes.
 */
TOB mapauxlist(argptr) /* argptr is pointer to list-args of mapc & others */
register TOB *argptr; 
{
        TOB start;
        register TOB lista;

/*
        if(endmarkp(*argptr)) { return(NIL); }
 */
        if(nilp(*argptr)) { return(NIL); }
        start = lista = cons(car(*argptr),NIL); /* first element ... */
        *argptr = cdr(*argptr);

        while(!endmarkp(*++argptr))
         { /* If some other than first list has come to end, then free that
               piece which we have constructed, and return nil: */
           if(nilp(*argptr))
            { free_list(start); return(NIL); }
           rplacd(lista,cons(car(*argptr),NIL));
           /* Replace the list in the stack by cdr of itself: */
           *argptr = cdr(*argptr);
           lista = cdr(lista);
         }

        return(start);
}


TOB mapc(fun) /* vlambda */
TOB fun;
{
        register TOB tmp;
        TOB res;

        while(!nilp(tmp = mapauxlist((&fun)+1)))
         {
           res = apply(fun,tmp);
           free_list(tmp);
         }
        return(res);
}

/* There is potential bug: (in mapping functions)
    If fun is nlambda, and it uses its arglist without copying, like:
     (defun veba nlambda (arglist) (nreverse arglist))
    then (mapcar 'veba '(1 2 3 4 5) '(a b c d e))
     produces some garbage, because those arglists constructed for apply
      are freed after call to apply.
 */


TOB mapcar(fun) /* vlambda */
TOB fun;
{
        TOB start;
        register TOB lista,tmp;

        start = lista = cons(NIL,NIL);

        while(!nilp(tmp = mapauxlist((&fun)+1)))
         {
           rplacd(lista,cons(apply(fun,tmp),NIL));
           lista = cdr(lista);
           free_list(tmp);
         }
/*
        lista = cdr(start);
        free_cons(start);
        return(lista);
 */ /* Nowadays free_cons returns cdr of its argument: */
        return(free_cons(start));
}


TOB mapcan(fun) /* vlambda */
TOB fun;
{
        TOB start;
        register TOB lista,new_list,tmp;

        start = lista = cons(NIL,NIL);  /* Allocate the root. */

        while(!nilp(tmp = mapauxlist((&fun)+1)))
         {
           new_list = apply(fun,tmp);
/* Concatenate a new list returned by apply after last list returned by apply:
 */
           lista = nconc(lista,new_list);
/* This optimizes code so that nconc don't need start browsing the list
    from the very first one when it searches the last cons cell: */
           if(consp(new_list)) { lista = new_list; }
           free_list(tmp);
         }

        free_cons(start);
}


TOB LL_maprplaca(fun,first_arg_list) /* vlambda */
TOB fun,first_arg_list;
{
        register TOB tmp;
        TOB z,first_list;

        /* Must use the separate list to keep the first argument list,
            because mapauxlist cdr's it before the rplaca is applied
            to it. Also one variable (z) is needed to keep the start
            of it so it can be returned as result: */
        z = first_list = first_arg_list;
        while(!nilp(tmp = mapauxlist(&first_arg_list)))
         {
           rplaca(first_list,apply(fun,tmp));
           free_list(tmp);
           first_list = cdr(first_list);
         }
        return(z);
}


/* Hieman meditoimista... */ /* vlambda */ /* cdr version of mapc */
/*
TOB mapl(fun)
TOB fun;
{
        register TOB tmp;
        TOB res;

        while(!nilp(tmp = mapauxlist((&fun)+1)))
         {
           res = apply(fun,tmp);
           free_list(tmp);
         }
        return(res);
}

*/


/* ----------------------------------------------------------------------- */

TOB /* nlambda */ defun(argl)
TOB argl;
{
        TOB fun_name,arglist,type,body;

/*      argl = topcopy(argl); */

        fun_name = car(argl);
        arglist  = cadr(argl);

        /* If it's not arglist, but type (discipline): */
        if(nonnilsymbolp(arglist))
         {
           type = arglist;
           argl = cdr(argl);
           arglist = cadr(argl);
         }
        else
         {
           type = _LAMBDA_;
         }

        if(!nilp(value(fun_name)) && !eq(value(fun_name),_UNBOUND_))
         {
           fprintf(stderr,"defun: function redefined: ");
           eprint(fun_name);
         }

        body     = cddr(argl);
        setvalue(fun_name,nconc(list2(type,arglist),body));
        pick_subst(NIL,value(fun_name));
        return(fun_name);
}
        

#define return_body goto nukuhiva

/* Replace (physically) args of lambda-expressions with pick-constructs */
TOB pick_subst(arglist,body)
TOB arglist; /* formal arglist of lambda form */
register TOB body;
{
        TOB local; /* Just the local work variable */

        if(atom(body)) { return_body; }

        /* Don't go through "quote-barrier",
            i.e. don't spoil quoted expressions:
           However, go through function-quotes (#') because
            they probably contain more lambda-forms:
         */
        else if(eq(car(body),_QUOTE_)) { return_body; }

        /* if lambdaform encountered, e.g. (lambda (x y) (cons x y))
            then handle that as case of its own:
         */
        else if(memq(car(body),_LAMBDAFORMS_))
         { /* Delete the keywords from the farglist and get proper length: */
           local = handle_farglist(cadr(body));
           /* Handle the body. Give () as arglist if its length was zero: */
           pick_subst((zerop(local) ? NIL : cadr(body)),cddr(body));
           rplaca(cdr(body),local); /* rplaca arglist with its length. */
           return_body;
         }

        /* If found formal argument from the body:  (Note that nilp should
           be a macro which uses its argument only once, or a function): */
        else if(!nilp(local = nthmemq(car(body),arglist)))
         { /* Then replace it by the corresponding pick-form: */
           rplaca(body,pick_tob((tob_uint(local)+1) << 2));
           pick_subst(arglist,cdr(body)); /* And continue substituting */
           return_body;
         }
        else
         {
           pick_subst(arglist,car(body));
           pick_subst(arglist,cdr(body));
           return_body;
         }

nukuhiva:
        return(body);
}

TOB handle_farglist(fargs)
TOB fargs;
{
    register UINT count;
    TOB previous;

    count = 0;
    previous = NIL;

    while(!nilp(fargs))
     {
       if(consp(car(fargs)) || eq(car(fargs),_REST_) || otherp(car(fargs)))
        {
          fprintf(stderr,
"handle_farglist: unimplemented keyword in the formal arguments: "); 
          eprint(car(fargs));
          goto kala;
        }
       if(memq(car(fargs),_DEFUN_KEYWORDS_))
        {
kala:
 /* If previous is not () then just rplacd it by the cdr of fargs: */
          if(!nilp(previous))
           { rplacd(previous,(fargs = cdr(fargs))); }
 /* If previous is () and the cdr of it is also, then it means that fargs
    were something like (&aux) and they should be replaced by NIL: */
          else if(nilp(cdr(fargs))) { return(ZERO); }
          else /* Previous was (), i.e. this is the first element in fargs */
           {
             rplaca(fargs,cadr(fargs));
             rplacd(fargs,cddr(fargs));
           }
        }
       else { count++; previous = fargs; fargs = cdr(fargs); }
     }

    return(int_tob(count));
}
                    


TOB debug_info(port)
TOB port;
{
        extern int _Corg(),_Cend();
        register FILE *fp;

        fp = (portp(port) ? tob_fp(port) : stdout);

        fprintf(fp,
"\nCS: 0x%04x  DS: 0x%04x  ES: 0x%04x  SS: 0x%04x  SP: 0x%04x  %u.\n",
           get_cs(),get_ds(),get_es(),get_ss(),get_sp(),get_sp());
        fprintf(fp,
"_Corg: 0x%04x  _Cend: 0x%04x  &_Uorg: 0x%08lx  &_Uend: 0x%08lx\n",
          _Corg,_Cend,&_Uorg,&_Uend);
        fprintf(fp,
"_HEAPSIZ: %u.  *16: %u.   _STKSIZ: %u.  *16: %u.\n",
                   _HEAPSIZ,(_HEAPSIZ * 16),_STKSIZ,(_STKSIZ * 16));
        fprintf(fp,
"_STKLOW: %u.    _STKRED: %u.   _max_args: %u.\n",
            _STKLOW,_STKRED,_max_args);
        fprintf(fp,
"_underflowlim: 0x%04x   _overflowlim: 0x%04x  _framepointer: 0x%04x\n",
        _underflowlim,_overflowlim,_framepointer);
        fprintf(fp,
"fp_lowerlim:   0x%08lx  fp_upperlim:   0x%08lx\n",
          fp_lowerlim,fp_upperlim);
        fprintf(fp,
"code_lowerlim: 0x%08lx  code_upperlim: 0x%08lx\n",
          code_lowerlim,code_upperlim);

        return(NIL);
}

TOB gfp()
{
        return(hex_tob(_framepointer));
}

/* Archaic self-modifying bullshit-code.
TOB test_drop(n)
TOB n;
{
        dropnbytes(tob_int(n));
        reset();
}
*/

TOB prinx(expr,port)
TOB expr,port;
{
        printhex(expr,(portp(port) ? tob_fp(port) : stdout));
        /* terpri(stdout); */
        return(expr);
}


TOB set_max_args(n)
TOB n;
{
        int i;

        i = _max_args;
        _max_args = tob_int(n);
        return(int_tob(i));
}

TOB fake(high,low)
TOB high,low;
{
    return(tob_uint(low) + (((ULI) tob_uint(high)) << 16));
}

