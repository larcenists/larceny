/* The local debugger. A marvel of technological standstill. */

/* $Id: localdebugger.c,v 1.1 91/12/06 15:06:13 lth Exp Locker: lth $ */

#include <stdio.h>
#include "machine.h"
#include "gcinterface.h"
#include "offsets.h"
#include "main.h"
#include "millicode.h"
#include "layouts.h"
#include "macros.h"
#include "exceptions.h"

localdebugger()
{
  while (1) {
    char cmd[ 80 ], cmdl[ 80 ];
    word val;
    int regno;
    extern int break_counter;

    printf( "> " ); fflush( stdout );
    if (fgets( cmd, 80, stdin ) == NULL) {
      printf( "\n<EOF>\n" );
      exit( 1 );
    }
    else if (sscanf( cmd, "%s", cmdl ) == 1) {
      switch (*cmdl) {
        case '=' : setreg( cmd ); break;
        case 'B' : backtrace(); break;
        case 'b' : printf( "break count: %d\n", break_counter ); break;
        case 'd' : dumpregs(); break;
        case 'r' : return;
	case 'p' : dumpproc(); break;
	case 'c' : dumpcodevec(); break;
	case 'X' : examine( cmd ); break;
	case '?' : help(); break;
	default : printf( "Nah.\n" );
      }
    }
    else 
      printf( "Nah.\n" );
  }
}

/*
 * Go back thru the stack and continuation chain and print those procedure
 * identifiers which are symbols. [Some may be #f, some may be weird for
 * some reason.]
 *
 * This procedure has detailed knowledge about frame layouts.
 */
static backtrace()
{
  word wretn, p, w;
  int len;

  wretn = (word) millicode[ 3 ];
  p = globals[ SP_OFFSET ];

  /* First trace stack */
  w = *(word *)p;            /* return address in stack frame */
  while (w != wretn) {
    w = *((word *)p + 2);    /* R0 */
    if ((w & TAG_MASK) == PROC_TAG) {
      w &= ~TAG_MASK;
      w = *(word *) (w + 8);   /* get constant vector ptr */
      printf( "%08x  ", w );
      w = *(word *) (w - 3 + 4);  /* get slot 0, the procedure name */
      printf( "%08x  ", w );
      if ((w & TAG_MASK) == VEC_TAG) {  /* may be symbol! */
	if ((*(word *) (w - 3) & 0xFF) == SYMBOL_HDR) { /* Yes!! */
	  w = *(word *) (w - 3 + 4) & ~TAG_MASK;  /* get bv ptr */
	  len = *(word *) w >> 8;
	  w += 4;
	  while (len--) {
	    putchar( *(char *) w );
	    w++;
	  }
	  putchar( '\n' );
	}
	else
	  printf( "<really weird name>\n" );
      }
      else 
	printf( "<anonymous or weird name>\n" );
    }
    else
      printf( "<placeholder procedure>\n" );
    p += *((word *)p + 1);   /* add frame size to get to next frame. */
    if (p % 8) p += 4;
    w = *(word *) p;
  }
  if (globals[ CONTINUATION_OFFSET ] != FALSE_CONST)
    printf( "Don't know how to do continuation chains (yet).\n" );
}

static setreg( cmd )
char *cmd;
{
  int regno;
  unsigned val;

  if (sscanf( cmd, "= R%d %x", &regno, &val ) == 2) {
    globals[ REG0_OFFSET + regno ] = val;
  }
    else printf( "Nah.\n" );
}

static help()
{
  printf( "d - dump regs\n" );
  printf( "b - break count\n" );
  printf( "B - stack backtrace\n" );
  printf( "r - return to running program\n" );
  printf( "p - dump the current procedure (if possible)\n" );
  printf( "Xx <loc> <count> - examine memory; x is w(ord),b(yte),c(har)\n" );
  printf( "c - dump the current code vector (symbolic) to /tmp/larceny.\n" );
  printf( "= <reg> <val> --  set register\n" );
  printf( "\n" );
  printf( "<loc> is either an address (hex number) or a register (Rn).\n" );
  printf( "The tag is masked off the register content before use.\n" );
  printf( "Register numbers and counts are in decimal.\n" );
}

static examine( cmdl )
char *cmdl;
{
  unsigned loc, count, regno;
  char type;

  if (sscanf( cmdl, "X%c %x %d", &type, &loc, &count ) == 3) {
  }
  else if (sscanf( cmdl, "X%c R%d %d", &type, &regno, &count ) == 3) {
    loc = (globals[ REG0_OFFSET + regno ] & ~TAG_MASK);
  }
  else {
    printf( "Nah (1).\n" );
    return;
  }
  if (type != 'c' && type != 'b' && type != 'w') {
    printf( "Nah.\n" );
    return;
  }

  if (type == 'c') { 
    unsigned char *p = (unsigned char *) loc;

    while (count--)
      printf( "%c", *p++ );
    printf( "\n" );
  }
  else if (type == 'b') {
    unsigned char *p = (unsigned char *) loc;

    while (count--) {
      printf( "%08x    %u\n", p, *p );
      p++;
    }
  }
  else {
    word *p = (word *) loc;

    while (count--) {
      printf( "%08x    %08x\n", p,  *p );
      p++;
    }
  }
}

static dumpproc()
{
  word w = globals[ REG0_OFFSET ];
  word q, l, *p;

  if (tagof( w ) != PROC_TAG) {
    printf( "REG0 does not have a procedure pointer.\n" );
    return;
  }

  p = ptrof( w );
  q = *p++;
  printf( "0x%08x\n", q );
  l = sizefield( q ) / 4;
  while (l--) {
    q = *p++;
    printf( "0x%08x\n", q );
  }
}

static dumpregs()
{
  int j, k;

  for (j = 0 ; j < 8 ; j++ ) {
    for (k = 0 ; k < 4 ; k++ )
      printf( "REG %2d=0x%08x  ", j*4+k, globals[ REG0_OFFSET + j*4+k ] );
    putchar( '\n' );
  }
  printf( "RESULT=0x%08x  ARGREG2=0x%08x  ARGREG3=0x%08x  RETADDR=0x%08x\n", 
	  globals[ RESULT_OFFSET ],
	  globals[ ARGREG2_OFFSET ],
	  globals[ ARGREG3_OFFSET ],
	  globals[ SAVED_RETADDR_OFFSET ] );
  printf( "TIMER=0x%08x\n", globals[ TIMER_OFFSET ] );
}

static dumpcodevec()
{
  word w = globals[ REG0_OFFSET ];
  word q, l, *p;
  unsigned char *cp;
  FILE *fp;

  if (tagof( w ) != PROC_TAG) {
    printf( "REG0 does not have a procedure pointer.\n" );
    return;
  }
  q = *(ptrof( w ) + 1);
  if (tagof( q ) != BVEC_TAG) {
    printf( "Rotten bytevector.\n" );
    return;
  }
  p = ptrof( q );
  l = sizefield( *p );
  cp = (unsigned char *) (p+1);
  if ((fp = fopen( "/tmp/larceny", "w" )) == NULL) {
    printf( "Couldn't open temp file." );
    return;
  }
  fprintf( fp, "(#(" );
  while (l--)
    fprintf( fp, "%d ", *cp++ );
  fprintf( fp, ") . #())" );
  fclose( fp );
}
