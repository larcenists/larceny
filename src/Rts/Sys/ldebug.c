/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny -- the run-time system debugger.
 */

/* TODO: Support FOURTH as a register name */
/* TODO: Support PC, STKP as register names */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include "larceny.h"

static void confused( char * );
static void step( char * );
static void breakpt( char * );
static void backtrace( void );
static void setreg( char * );
static void trace( char* );
static void help( void );
static void examine( char * );
static void dumpproc( void );
static void dumpregs( void );
static void dumpglob( void );
static void dumpcodevec( void );
static void dump_top_frame( void );
static int getreg( char ** );
static unsigned int getuint( char **, int* );

static int tracing = 0;
static char tracing_until[100] = { 0 };

void localdebugger_step(word* globals)
{
  char buf[ 300 ];
  int l;
#ifdef G_SECOND
  word s = globals[G_SECOND];
#else
  word s = globals[G_ARGREG2];
#endif
  /* TODO: flat1/flat4 distinction */
  if ((*ptrof(s) & 255) == USTR_HDR) {
    /*flat4*/
    int i;
    char* p = buf;
    uint32_t* q = (uint32_t*)(ptrof(s)+BVEC_HEADER_WORDS);
    l = *ptrof(s) >> 10;
    for ( i=0 ; i < l && i < sizeof(buf)-1 ; i++ )
      *p++ = (*q++ >> 8) & 127;
    *p++ = 0;
  }
  else {
    l = string_length( s );
    strncpy( buf, string_data( s ), min( l, sizeof( buf )-1 ) );
    buf[ l ] = 0;
  }
  hardconsolemsg( "Step: %s", buf );
  /* Trace until the string in tracing_until[] is a substring of the buffer */
  if (tracing) {
    if (tracing_until[0] == 0)
      return;
    if (strstr(buf, tracing_until) == NULL)
      return;
    tracing = 0;
    tracing_until[0] = 0;
  }
  localdebugger();
}

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
        case 't' : trace( cmd ); return;
        case 'r' : return;
	case 'p' : dumpproc(); break;
	case 'c' : dumpcodevec(); break;
	case 'X' : examine( cmd ); break;
        case 'z' : step( cmd ); break;
	case '?' : help(); break;
        case 'a' : exit(0); break;
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

static void trace( char* cmd )
{
  tracing = 1;
  tracing_until[0] = 0;
  while (*cmd != 't')
    cmd++;
  cmd++;
  if (*cmd == '-') {
    tracing = 0;
    return;
  }
  while (isspace(*cmd))
    cmd++;
  if (*cmd == 0)
    return;
  int k=0;
  while (k < sizeof(tracing_until)-1 && *cmd != 0 && *cmd != '\n' && *cmd != '\r')
    tracing_until[k++] = *cmd++;
  tracing_until[k] = 0;
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
#ifdef G_SCHCALL_ARG4
		   { "SCHCALL_ARG4", G_SCHCALL_ARG4 },
#endif
#ifdef G_FOURTH
		   { "FOURTH", G_FOURTH },
#endif
#ifdef G_ALLOCI_TMP
		   { "ALLOCI_TMP", G_ALLOCI_TMP },
#endif
#ifdef G_GENERIC_NRTMP1
		   { "GENERIC_NRTMP1", G_GENERIC_NRTMP1 },
		   { "GENERIC_NRTMP2", G_GENERIC_NRTMP2 },
		   { "GENERIC_NRTMP3", G_GENERIC_NRTMP3 },
		   { "GENERIC_NRTMP4", G_GENERIC_NRTMP4 },
#endif
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
#ifdef G_SECOND
  else if (sscanf( cmd, "= SECOND %i", &val ) == 1)
    globals[ G_SECOND ] = val;
#else
  else if (sscanf( cmd, "= ARGREG2 %i", &val ) == 1)
    globals[ G_ARGREG2 ] = val;
#endif
#ifdef G_THIRD
  else if (sscanf( cmd, "= THIRD %i", &val ) == 1)
    globals[ G_THIRD ] = val;
#else
  else if (sscanf( cmd, "= ARGREG3 %i", &val ) == 1)
    globals[ G_ARGREG3 ] = val;
#endif
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
  printf( "a        abort execution\n");
  printf( "b{+|-}   manipulate breakpoint enable\n" );
  printf( "B        stack backtrace\n" );
  printf( "c        dump the current code vector to /tmp/larceny\n" );
  printf( "d        display register values\n" );
  printf( "p        dump the current procedure (if possible)\n" );
  printf( "r        return\n" );
  printf( "t [str]  enable tracing (until instruction has str as substring) and return\n" );
  printf( "t-       stop tracing\n" );
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
    int success = 0;
    unsigned int i; 
    i = getuint( &cmdl , &success);
    if (success == -1) {
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
    int success = 0;
    count = getuint( &cmdl, &success ); 
    if (success == -1) {
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
      printf( "%08x    %u\n", (unsigned int)p, *p );
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
	printf( "String (flat1), length %d\n", len );
	for ( cp = ((char*)ptrof( loc ))+4 ; len ; cp++, len-- )
	  putchar( *cp );
        putchar( '\n' );
      }
      else if (tag == USTR_SUBTAG) {
	printf( "String (flat4), length %d\n", len/4 );
	uint32_t* up;
	for ( up = ((uint32_t*)ptrof(loc))+1 ; len ; up++, len-=4 )
	  putchar( ((*up) >> 8) & 255 ); /* FIXME - would want to do better when we can */
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
      printf( "%08x    %08x\n", (unsigned int)p,  *p );
      p++;
    }
  }
}

static int getreg( char **cmdl )
{
  unsigned int regno;
  char *p = *cmdl;
  while (isspace(*p)) p++;
  if (strncmp( p, "RESULT", 6 ) == 0) {
    p+=6;
    regno = G_RESULT;
  }
#ifdef G_SECOND
  else if (strncmp( p, "SECOND", 6 ) == 0) {
    p+=6;
    regno = G_SECOND;
  }
#else
  else if (strncmp( p, "ARGREG2", 7 ) == 0) {
    p+=7;
    regno = G_ARGREG2;
  }
#endif
#ifdef G_THIRD
  else if (strncmp( p, "THIRD", 5 ) == 0) {
    p+=5;
    regno = G_THIRD;
  }
#else
  else if (strncmp( p, "ARGREG3", 7 ) == 0) {
    p+=7;
    regno = G_ARGREG3;
  }
#endif
  else if (*p == 'R') {
    int success = 0;
    p++;
    regno = getuint( &p, &success );
    if (success == -1 || regno > 31) return -1;
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

static unsigned int getuint( char **cmdl, int* success )
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
    if (sscanf( buf, "0x%x", &n ) == 1) {
      *cmdl = p;
      return n;
    } else if (sscanf( buf, "0X%x", &n ) == 1) {
      *cmdl = p;
      return n;
    } else if (sscanf( buf, "%i", &n ) == 1) {
      *cmdl = p;
      return n;
    }
  }
  *success = -1;
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

#ifdef G_SECOND
  printf( "RESULT=0x%08x  SECOND=0x%08x  THIRD= 0x%08x  FOURTH=0x%08x\n", 
	  globals[ G_RESULT ],
	  globals[ G_SECOND ],
	  globals[ G_THIRD ],
	  globals[ G_FOURTH ] );
#else
  printf( "RESULT=0x%08x  ARGREG2=0x%08x  ARGREG3=0x%08x  RETADDR=0x%08x\n", 
	  globals[ G_RESULT ],
	  globals[ G_ARGREG2 ],
	  globals[ G_ARGREG3 ],
	  globals[ G_RETADDR ] );
#endif
  for (j = 0 ; j < 8 ; j++ ) {
    for (k = 0 ; k < 4 ; k++ )
      printf( "REG %2d=0x%08x  ", j*4+k, globals[ G_REG0 + j*4+k ] );
    putchar( '\n' );
  }
  printf( "TIMER=0x%08x  Flags=%c%c%c  PC=0x%08x  CONT=0x%08x\n",
	  globals[ G_TIMER2 ] + globals[ G_TIMER ],
	  (globals[ G_TIMER_ENABLE ] == TRUE_CONST ? 'T' : 't'),
	  (globals[ G_SINGLESTEP_ENABLE ] == TRUE_CONST ? 'S' : 's'),
	  (globals[ G_BREAKPT_ENABLE ] == TRUE_CONST ? 'B' : 'b'),
	  globals[ G_RETADDR ],
          globals[ G_CONT ] );
  printf( "STKP=0x%08lx STKBOT=0x%08lx EBOT=0x%08lx ETOP=0x%08lx\n",
	  (long unsigned int)globals[ G_STKP ],
	  (long unsigned int)globals[ G_STKBOT ],
	  (long unsigned int)globals[ G_EBOT ],
          (long unsigned int)globals[ G_ETOP ] );
  printf( "CALLOUTS=0x%08x\n", globals[ G_CALLOUTS ] );
}

static void dumpglob( void )
{
  printf( "Roots for precise GC:\n" );
  printf( "  G_STARTUP=0x%08x  G_CALLOUTS=0x%08x"
#ifdef G_SCHCALL_ARG4
	  "G_SCHCALL_ARG4=0x%08x"
#endif
	  "\n",
	  globals[ G_STARTUP ],
	  globals[ G_CALLOUTS ]
#ifdef G_SCHCALL_ARG4
	  , globals[ G_SCHCALL_ARG4 ]
#endif
	  );
#ifdef G_ALLOCI_TMP
  printf( "  G_ALLOCI_TMP=0x%08x\n", globals[ G_ALLOCI_TMP ] );
#endif
  printf( "\nNon-roots:\n" );
#ifdef G_GENERIC_NRTMP1
  printf( "  G_GENERIC_NRTMP1=0x%08x G_GENERIC_NRTMP2=0x%08x\n", 
	  globals[ G_GENERIC_NRTMP1 ], globals[ G_GENERIC_NRTMP2 ] );
  printf( "  G_GENERIC_NRTMP3=0x%08x G_GENERIC_NRTMP4=0x%08x\n", 
	  globals[ G_GENERIC_NRTMP3 ], globals[ G_GENERIC_NRTMP4 ] );
#endif
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
