/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny -- the run-time system debugger.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "larceny.h"

static void confused( char * );
static void step( char * );
static void breakpt( char * );
static void backtrace( void );
static void setreg( char * );
static void help( void );
static void examine( char * );
static void dumpproc( void );
static void dumpregs( void );
static void dumpglob( void );
static void dumpcodevec( void );
static void dump_top_frame( void );
static int getreg( char ** );
static int getuint( char ** );

void localdebugger( void )
{
/*  printf( "[runtime system debugger; type '?' for help]\n" ); */
  while (1) {
    char cmd[ 80 ], cmdl[ 80 ];

    printf( "[rtsdebug] " ); fflush( stdout );
    if (fgets( cmd, 80, stdin ) == NULL) {
      printf( "\n" );
      exit( 1 );
    }
    else if (sscanf( cmd, "%s", cmdl ) == 1) {
      switch (*cmdl) {
        case '=' : setreg( cmd ); break;
	case 'b' : breakpt( cmd ); break;
        case 'B' : backtrace(); break;
        case 'd' : dumpregs(); break;
	case 'g' : dumpglob(); break;
        case 'r' : return;
	case 'p' : dumpproc(); break;
	case 'c' : dumpcodevec(); break;
	case 'X' : examine( cmd ); break;
        case 'z' : step( cmd ); break;
	case '?' : help(); break;
	default  : confused( "cmd" ); break;
      }
    }
    else 
      confused( "cmd" );
  }
}

void debugvsm( void )
{
  dumpregs();
  printf( "\n" );
  dump_top_frame();
}

static void confused( char *msg )
{
  printf( "Don't understand %s; type '?' for help.\n", msg );
}

static void step( char *cmd )
{
  if (cmd[ 1 ] == '+') 
    globals[ G_SINGLESTEP_ENABLE ] = TRUE_CONST;
  else if (cmd[ 1 ] == '-')
    globals[ G_SINGLESTEP_ENABLE ] = FALSE_CONST;
  else
    confused( "step" );
}

static void breakpt( char *cmd )
{
  if (cmd[ 1 ] == '+') 
    globals[ G_BREAKPT_ENABLE ] = TRUE_CONST;
  else if (cmd[ 1 ] == '-')
    globals[ G_BREAKPT_ENABLE ] = FALSE_CONST;
  else
    confused( "breakpt" );
}

/*
 * Go back thru the stack and continuation chain and print those procedure
 * identifiers which are symbols. [Some may be #f, some may be weird for
 * some reason.]
 *
 * This procedure has detailed knowledge about frame layouts.
 */
static void backtrace( void )
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

static void setreg( char *cmd )
{
  static struct {
    char *name;
    int index;
  } globassoc[] = {{ "STARTUP", G_STARTUP },
		   { "CALLOUTS", G_CALLOUTS },
		   { "SCHCALL_ARG4", G_SCHCALL_ARG4 },
		   { "ALLOCI_TMP", G_ALLOCI_TMP },
		   { "GENERIC_NRTMP1", G_GENERIC_NRTMP1 },
		   { "GENERIC_NRTMP2", G_GENERIC_NRTMP2 },
		   { "GENERIC_NRTMP3", G_GENERIC_NRTMP3 },
		   { "GENERIC_NRTMP4", G_GENERIC_NRTMP4 },
		   { "PUSHTMP", G_PUSHTMP },
		   { "CALLOUT_TMP0", G_CALLOUT_TMP0 },
		   { "CALLOUT_TMP1", G_CALLOUT_TMP1 },
		   { "CALLOUT_TMP2", G_CALLOUT_TMP2 },
		   { "SCHCALL_RETADDR", G_SCHCALL_RETADDR },
		   { 0, 0 }};
  int regno, i;
  unsigned val;
  char name[ 128 ];

  if (sscanf( cmd, "= R%i %i", &regno, &val ) == 2)
    globals[ G_REG0 + regno ] = val;
  else if (sscanf( cmd, "= RESULT %i", &val ) == 1)
    globals[ G_RESULT ] = val;
  else if (sscanf( cmd, "= ARGREG2 %i", &val ) == 1)
    globals[ G_ARGREG2 ] = val;
  else if (sscanf( cmd, "= ARGREG3 %i", &val ) == 1)
    globals[ G_ARGREG3 ] = val;
  else if (sscanf( cmd, "= G_%[A-Z0-9_] %i", name, &val ) == 2) {
    for ( i=0 ; globassoc[i].name && strcmp( globassoc[i].name, name ) ; i++ )
      ;
    if (globassoc[i].name)
      globals[ globassoc[i].index ] = val;
    else
      confused( "global name" );
  }
  else
    confused( "reg" );
}

static void help( void )
{
  printf( "b{+|-}   manipulate breakpoint enable\n" );
  printf( "B        stack backtrace\n" );
  printf( "c        dump the current code vector to /tmp/larceny\n" );
  printf( "d        display register values\n" );
  printf( "p        dump the current procedure (if possible)\n" );
  printf( "r        return\n" );
  printf( "Xo <loc> examine object\n" );
  printf( "Xx <loc> <count>\n" );
  printf( "         examine w(ord),b(yte),c(har)\n" );
  printf( "z{+|-}   manipulate single stepping enable\n" );
  printf( "= <reg> <val>\n" );
  printf( "         set register value\n" );
  printf( "= <globval> <val>\n" );
  printf( "         set global (as dumped with 'g') value\n" );
  printf( "\n" );
  printf( "<loc> is either an address (number) or a register name (syntax: Rn).\n" );
  printf( "For Xw and Xo the tag is masked off the <loc>.\n" );
  printf( "Numbers are decimal, octal, or hex (use C syntax).\n" );
}

static void examine( char *cmdl )
{
  word loc;
  unsigned count = 0;
  char type;
  int regno;

  cmdl++;
  type = *cmdl;
  if (type != 'c' && type != 'b' && type != 'w' && type != 'o') {
    confused( "size" );
    return;
  }
  cmdl++;

  while (isspace(*cmdl)) cmdl++;
  if (isdigit( *cmdl )) {
    int i;
    i = getuint( &cmdl );
    if (i == -1) {
      confused( cmdl );
      return;
    }
    loc = (word)i;
  }
  else {
    regno = getreg( &cmdl );
    if (regno == -1) {
      confused( cmdl );
      return;
    }
    loc = globals[ regno ];
  }
  if (type != 'o') {
    count = getuint( &cmdl );
    if (count == -1) {
      confused( cmdl );
      return;
    }
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
  else if (type == 'o') {
    int len, i, tag;
    word hdr, *p;
    char *cp;

    /* dump the entire object */
    switch( tagof( loc ) ) {
    case PAIR_TAG :
      printf( "Pair\n" );
      printf( "  0x%08x\n  0x%08x\n", pair_car( loc ), pair_cdr( loc ) );
      break;
    case VEC_TAG  :
    case PROC_TAG :
      hdr = *ptrof( loc );
      len = sizefield(hdr)/4;
      printf( "%s, length %d tag %d\n", 
	      (tagof( loc ) == VEC_TAG ? "Vector-like" : "Procedure"),
	      len, typetag( hdr ) );
      for ( i=0, p=ptrof( loc )+1 ; len ; i++, len--, p++ )
	printf( "  %04d 0x%08x\n", i, *p );
      break;
    case BVEC_TAG :
      hdr = *ptrof( loc );
      len = sizefield(hdr);
      tag = typetag( hdr );
      if (tag == STR_SUBTAG) {
	printf( "String, length %d\n", len );
	for ( cp = ((char*)ptrof( loc ))+4 ; len ; cp++, len-- )
	  putchar( *cp );
        putchar( '\n' );
      }
      else
	printf( "Bytevector-like, length %d, tag %d\n", len, tag );
      break;
    case IMM1_TAG :
    case IMM2_TAG :
      printf( "Immediate: 0x%08x\n", loc );
      break;
    case FIX1_TAG :
    case FIX2_TAG :
      printf( "Fixnum: %d\n", nativeint( loc ) );
      break;
    }
  }
  else {
    word *p = (word *) ptrof( loc );

    while (count--) {
      printf( "%08x    %08x\n", p,  *p );
      p++;
    }
  }
}

static int getreg( char **cmdl )
{
  int regno;
  char *p = *cmdl;
  while (isspace(*p)) p++;
  if (strncmp( p, "RESULT", 6 ) == 0) {
    p+=6;
    regno = G_RESULT;
  }
  else if (strncmp( p, "ARGREG2", 7 ) == 0) {
    p+=7;
    regno = G_ARGREG2;
  }
  else if (strncmp( p, "ARGREG3", 7 ) == 0) {
    p+=7;
    regno = G_ARGREG3;
  }
  else if (*p == 'R') {
    p++;
    regno = getuint( &p );
    if (regno == -1 || regno > 31) return -1;
    regno += G_REG0;
  }
  else
    return -1;
  if (isspace( *p ) || *p == 0) {
    *cmdl = p;
    return regno;
  }
  return -1;
}

static int getuint( char **cmdl )
{
  char *p = *cmdl, *q;
  char buf[ 40 ];
  int n;

  while (isspace(*p)) p++;
  if (!isdigit( *p )) return -1;
  q = p;
  while (isxdigit(*p) || *p == 'X' || *p == 'x') p++;
  if (isspace(*p) || *p == 0) {
    strncpy( buf, q, p-q );
    buf[p-q] = 0;
    if (sscanf( buf, "%i", &n ) == 1) {
      *cmdl = p;
      return n;
    }
  }
  return -1;
}

static void dumpproc( void )
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

static void dumpregs( void )
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
	  globals[ G_TIMER2 ] + globals[ G_TIMER ],
	  (globals[ G_TIMER_ENABLE ] == TRUE_CONST ? 'T' : 't'),
	  (globals[ G_SINGLESTEP_ENABLE ] == TRUE_CONST ? 'S' : 's'),
	  (globals[ G_BREAKPT_ENABLE ] == TRUE_CONST ? 'B' : 'b'),
	  globals[ G_RETADDR ],
          globals[ G_CONT ] );

  printf( "STKP=0x%08lx STKBOT=0x%08lx EBOT=0x%08lx ETOP=0x%08lx\n",
	  globals[ G_STKP ],
	  globals[ G_STKBOT ],
	  globals[ G_EBOT ],
          globals[ G_ETOP ] );
}

static void dumpglob( void )
{
  printf( "Roots for precise GC:\n" );
  printf( "  G_STARTUP=0x%08x  G_CALLOUTS=0x%08x  G_SCHCALL_ARG4=0x%08x\n",
	  globals[ G_STARTUP ],
	  globals[ G_CALLOUTS ],
	  globals[ G_SCHCALL_ARG4 ] );
  printf( "  G_ALLOCI_TMP=0x%08x\n", globals[ G_ALLOCI_TMP ] );
  printf( "\nNon-roots:\n" );
  printf( "  G_GENERIC_NRTMP1=0x%08x G_GENERIC_NRTMP2=0x%08x\n", 
	  globals[ G_GENERIC_NRTMP1 ], globals[ G_GENERIC_NRTMP2 ] );
  printf( "  G_GENERIC_NRTMP3=0x%08x G_GENERIC_NRTMP4=0x%08x\n", 
	  globals[ G_GENERIC_NRTMP3 ], globals[ G_GENERIC_NRTMP4 ] );
  printf( "  G_PUSHTMP=0x%08x G_SCHCALL_RETADDR=0x%08x\n",
	  globals[ G_PUSHTMP ], globals[ G_SCHCALL_RETADDR ] );
  printf( "  G_CALLOUT_TMP0=0x%08x G_CALLOUT_TMP1=0x%08x G_CALLOUT_TMP2=0x%08x\n",
	  globals[ G_CALLOUT_TMP0 ], globals[ G_CALLOUT_TMP1 ],
	  globals[ G_CALLOUT_TMP2 ] );
}

static void dump_top_frame( void )
{
  word *stkp = (word*)globals[ G_STKP ];
  word size = stkp[0]/4;
  int i;

  for ( i=0 ; i < size && i < 10 ; i++ )
    printf( "%-3d  %08x\n", i*4, stkp[i] );
  if (i < size)
    printf( "... (%d slots)\n", size );
}

static void dumpcodevec( void )
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
