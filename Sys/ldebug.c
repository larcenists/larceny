/*
 * This is the file Sys/ldebug.c.
 *
 * Larceny run-time system (Unix) -- the run-time system debugger.
 *
 * History
 *   July 1, 1994 / lth (v0.20)
 *     Commented out the worst bits, cleaned up the rest.
 */

#include <stdio.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

void localdebugger()
{
  printf( "[runtime system debugger; type '?' for help]\n" );
  while (1) {
    char cmd[ 80 ], cmdl[ 80 ];
    word val;
    int regno;

    printf( "> " ); fflush( stdout );
    if (fgets( cmd, 80, stdin ) == NULL) {
      printf( "\n" );
      exit( 1 );
    }
    else if (sscanf( cmd, "%s", cmdl ) == 1) {
      switch (*cmdl) {
        case '=' : setreg( cmd ); break;
        case 'B' : backtrace(); break;
        case 'd' : dumpregs(); break;
        case 'r' : return;
	case 'p' : dumpproc(); break;
	case 'c' : dumpcodevec(); break;
	case 'X' : examine( cmd ); break;
        case 'z' : step( cmd ); break;
	case '?' : help(); break;
	default  : confused(); break;
      }
    }
    else 
      confused();
  }
}

static confused()
{
  printf( "?\n" );
}

static step( cmd )
char *cmd;
{
  if (cmd[ 1 ] == '+') 
    globals[ G_SINGLESTEP_ENABLE ] = TRUE_CONST;
  else if (cmd[ 1 ] == '-')
    globals[ G_SINGLESTEP_ENABLE ] = FALSE_CONST;
  else
    confused();
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
#if 0
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
#else
  printf( "Backtrace is disabled\n" );
#endif
}

static setreg( cmd )
char *cmd;
{
  int regno;
  unsigned val;

  if (sscanf( cmd, "= R%i %i", &regno, &val ) == 2) {
    globals[ G_REG0 + regno ] = val;
  }
  else confused();
}

static help()
{
  printf( "d - dump regs\n" );
  printf( "B - stack backtrace\n" );
  printf( "r - return to running program\n" );
  printf( "p - dump the current procedure (if possible)\n" );
  printf( "Xx <loc> <count> - examine memory; x is w(ord),b(yte),c(har)\n" );
  printf( "z{+|-}  - manipulate single stepping status.\n" );
  printf( "c - dump the current code vector (symbolic) to /tmp/larceny.\n" );
  printf( "= <reg> <val> --  set register\n" );
  printf( "\n" );
  printf( "<loc> is either an address (number) or a register (Rn).\n" );
  printf( "The tag is masked off the register content before use.\n" );
  printf( "Numbers are decimal, octal, or hex.\n" );
}

static examine( cmdl )
char *cmdl;
{
  unsigned loc, count, regno;
  char type;

  if (sscanf( cmdl, "X%c %i %i", &type, &loc, &count ) == 3) {
  }
  else if (sscanf( cmdl, "X%c R%i %i", &type, &regno, &count ) == 3) {
    loc = (globals[ G_REG0 + regno ] & ~TAG_MASK);
  }
  else {
    confused();
    return;
  }
  if (type != 'c' && type != 'b' && type != 'w') {
    confused();
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
  word w = globals[ G_REG0 ];
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
      printf( "REG %2d=0x%08x  ", j*4+k, globals[ G_REG0 + j*4+k ] );
    putchar( '\n' );
  }
  printf( "RESULT=0x%08x  ARGREG2=0x%08x  ARGREG3=0x%08x  RETADDR=0x%08x\n", 
	  globals[ G_RESULT ],
	  globals[ G_ARGREG2 ],
	  globals[ G_ARGREG3 ],
	  globals[ G_RETADDR ] );
  printf( "TIMER=0x%08x  Flags=%c%c%c  PC=0x%08x  CONT=0x%08x\n",
	  globals[ G_TIMER ],
	  (globals[ G_TIMER_ENABLE ] == TRUE_CONST ? 'T' : ' '),
	  (globals[ G_SINGLESTEP_ENABLE ] == TRUE_CONST ? 'S' : ' '),
	  (globals[ G_BREAKPT_ENABLE ] == TRUE_CONST ? 'B' : ' '),
	  globals[ G_RETADDR ],
          globals[ G_CONT ] );

  printf( "STKP=0x%08lx STKBOT=0x%08lx EBOT=0x%08lx ETOP=0x%08lx\n",
	  globals[ G_STKP ],
	  globals[ G_STKBOT ],
	  globals[ G_EBOT ],
          globals[ G_ETOP ] );
}

static dumpcodevec()
{
  word w = globals[ G_REG0 ];
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

/* eof */
