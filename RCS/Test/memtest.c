/*
 * Test program for memory management subsystem.
 * Derived from garbage collector test program. 
 * EXEC and DEF are no longer supported.
 *
 * $Id: memtest.c,v 1.1 91/06/27 16:34:47 lth Exp Locker: lth $
 *
 * Usage:
 *   memtest <inheap> <outheap>
 *
 * Heaps are described using the Heap Description Language (HDL).
 * The syntax for a HDL "program" is as follows:
 *
 * program --> <stmt>* "END"
 * stmt    --> "ROOTS"
 *           | "EPHEMERAL"
 *           | "TENURED"
 *           | "ENTRYLIST"
 *           | "STACK"
 *           | "E-COLLECT"
 *           | "T-COLLECT"
 *           | "S-FLUSH"
 *           | "E-DUMP"
 *           | "T-DUMP"
 *           | "Y-DUMP"
 *           | "R-DUMP"
 *           | "S-DUMP"
 *           | "SS-DUMP"
 *           | "POINTERS"
 *           | "ALLOC" <hex>
 *           | "ALLOCI" <hex> <hex>
 *           | "FRAME" <hex> ...
 *           | <word>
 *           
 * word    --> ["E","T","C"]<hex>
 * hex     --> ["0"-"9","a"-"f","A"-"F"]+
 *
 * Lines cannot be blank; if a line does not start with a ';' then it
 * must have a meaningfull program segment.
 */

#include <stdio.h>
#include <ctype.h>
#include "gcinterface.h"
#include "offsets.h"
#include "millicode.h"
#include "main.h"
#include "macros.h"

word globals[ GLOBALS_TABLE_SIZE ];
void (*millicode[ MILLICODE_TABLE_SIZE ])();

word *roots = &globals[ FIRST_ROOT ];
word rootcnt = 0;

/* Our files */
FILE *ifp, *ofp;

int (*keyword())();        /* I love it */

char *strdup();
char *malloc();

/*
 * Here goes.
 */
main( argc, argv )
int argc;
char **argv;
{
  extern void alloc(), alloci(), setcar(), setcdr(), vectorset(), gcstart(),
              stkoflow(), stkuflow(), exception();
  char s[ 200 ];
  int i;
  int (*f)();

  if (argc != 3)
    panic( "usage: %s <inheap> <outheap>\n", argv[ 0 ] );

  printf( "Initializing\n" );

  /* Millicode is used by initializers, so do them first! */

  millicode[ M_ALLOC ] = alloc;
  millicode[ M_ALLOCI ] = alloci;
  millicode[ M_SETCAR ] = setcar;
  millicode[ M_SETCDR ] = setcdr;
  millicode[ M_VECTORSET ] = vectorset;
  millicode[ M_GCSTART ] = gcstart;
  millicode[ M_EXCEPTION ] = exception;
  millicode[ M_STKOFLOW ] = stkoflow;
  /* The address calculation in the next line is necessary because the stack
     underflow routine is *returned* into, necessitating a weird address to
     compensate for the compensation done by the return instruction. */
  millicode[ M_STKUFLOW ] = (void (*)())((word)stkuflow - 8);

  if (!init_mem( 1024*1024, 1024*1024, 1024*1024, 1024*8, 1024*512 ))
    panic( "Unable to initialize!" );

  printf( "Init done; running...\n" );

  if ((ifp = fopen( argv[ 1 ], "r" )) == NULL)
    panic( "Unable to open input file" );

  if ((ofp = fopen( argv[ 2 ], "w" )) == NULL)
    panic( "Unable to create output file" );

  while (getline( ifp, s )) {
    f = keyword( s );
    if (f != NULL)
      (*f)( s );
    else
      panic( "error in input" );
  }

  fclose( ifp );
  fclose( ofp );
}


void exception()
{
  fprintf( stderr, "Exception handler was called! Panicking...\n" );
  exit( 1 );
}


test_alloc( s )
char *s;
{
  unsigned wds;

  s += strlen( "ALLOC" );
  if (sscanf( s, "%x", &wds ) != 1)
    panic( "Illegal ALLOC" );
  test_alloc_2( wds );
}


test_alloci( s )
char *s;
{
  unsigned wds, init;

  s += strlen( "ALLOCI" );
  if (sscanf( s, "%x %x", &wds, &init ) != 2)
    panic( "Illegal ALLOCI" );
  test_alloci_2( wds, init );
}


test_nalloci( s )
char *s;
{
  unsigned wds, init, count;

  s += strlen( "NALLOCI" );
  if (sscanf( s, "%x %x %x", &count, &wds, &init ) != 3)
    panic( "Illegal NALLOCI" );

  while (count--)
    test_alloci_2( wds, init );
}

test_sum( s )
char *s;
{
  unsigned x;

  s += strlen( "SUM" );
  if (sscanf( s, "%x", &x ) != 1)
    panic( "Illegal SUM" );
  fprintf( ofp, "; sum value: %x\n", test_sum_2( x ) );
}

outl( s )
char *s;
{
  fprintf( ofp, "%s\n", s );
}

load_ephemeral()
{
  load( globals[ E_BASE_OFFSET ], &globals[ E_TOP_OFFSET ] );
}

load_tenured()
{
  load( globals[ T_BASE_OFFSET ], &globals[ T_TOP_OFFSET ] );
}

/* This has to be loaded from higher toward lower addresses since we don't 
   know how large it is before we've read it all. */
load_entrylist()
{
  char s[ 200 ];

  while(1) {
    if (!getline( ifp, s ))
      return;
    if (keyword( s ) != NULL) {
      ungetline( s );
      return;
    }
    *((word *)globals[ T_TRANS_OFFSET ]) = getword( s );
    globals[ T_TRANS_OFFSET ] -= 4;
  }
}

load_roots()
{
  char s[ 200 ];

  while (1) {
    if (!getline( ifp, s ))
      return;
    if (keyword( s ) != NULL) {
      ungetline( s );
      return;
    }
    *(roots+rootcnt) = getword( s );
    rootcnt++;
  }
}

/* Stacks must be specified from higher addresses toward lower addresses. */
load_stack()
{
  char s[ 200 ];

  while(1) {
    if (!getline( ifp, s ))
      return;
    if (keyword( s ) != NULL) {
      ungetline( s );
      return;
    }
    *((word *)globals[ SP_OFFSET ]) = getword( s );
    globals[ SP_OFFSET ] -= 4;
  }
}

getword( s )
char *s;
{
  word tmp;

  if (sscanf( s+1, "%lx", &tmp ) != 1)
    panic( "Unable to get word" );
  if (*s == 'E')
    return tmp + (word) globals[ E_BASE_OFFSET ];
  else if (*s == 'T')
    return tmp + (word) globals[ T_BASE_OFFSET ];
  else if (*s == 'C')
    return tmp;
  else
    panic( "Illegal word spec" );
}

t_collect()
{
  fprintf( ofp, "; tenuring collection\n" );
  collect( 2 );
}

e_collect()
{
  fprintf( ofp, "; ephemeral collection\n" );
  collect( 1 );
}

e_dump()
{
  dump( "ephemeral area", globals[ E_BASE_OFFSET ], globals[ E_TOP_OFFSET ] );
}

t_dump()
{
  dump( "tenured area", globals[ T_BASE_OFFSET ], globals[ T_TOP_OFFSET ] );
}

r_dump()
{
  int i;

  fprintf( ofp, "; roots\n" );
  for (i = FIRST_ROOT ; i < FIRST_ROOT + rootcnt ; i++ )
    dumpword( globals[ i ] );
}


/*
 * Dump stack cache between STK_START (exclusive) and SP (inclusive).
 */
s_dump()
{
  word *p, *q;

  fprintf( ofp, "; user portion of stack cache\n" );
  p = (word *) globals[ STK_START_OFFSET ] - 1;
  q = (word *) globals[ SP_OFFSET ];
  while (p >= q) {
    dumpword( *p );
    p--;
  }
}


/*
 * Dump stack cache between STK_MAX and STK_START (both inclusive).
 */
sstart_dump()
{
  word *p, *q;

  fprintf( ofp, "; system portion of stack cache\n" );
  p = (word *) globals[ STK_MAX_OFFSET ];
  q = (word *) globals[ STK_START_OFFSET ];
  while (p >= q) {
    dumpword( *p );
    p--;
  }
}


/*
 * Dump transaction area
 */
y_dump()
{
  word *p = (word *) globals[ T_MAX_OFFSET ];

  fprintf( ofp, "; entry list\n" );
  while (p > (word *) globals[ T_TRANS_OFFSET ])
    dumpword( *p-- );
}


pointers()
{
#define outit( s, x )  fprintf( ofp, "; %-20s %08lX\n", s, (word) x )
  outit( "e_base", globals[ E_BASE_OFFSET ] );
  outit( "e_top", globals[ E_TOP_OFFSET ] );
  outit( "e_limit", globals[ E_LIMIT_OFFSET ] );
  outit( "e_mark", globals[ E_MARK_OFFSET ] );
  outit( "e_max", globals[ E_MAX_OFFSET ] );
  outit( "t_base", globals[ T_BASE_OFFSET ] );
  outit( "t_top", globals[ T_TOP_OFFSET ] );
  outit( "t_trans", globals[ T_TRANS_OFFSET ] );
  outit( "t_max", globals[ T_MAX_OFFSET ] );
  outit( "stack_base", globals[ STK_BASE_OFFSET ] );
  outit( "stack_max", globals[ STK_MAX_OFFSET ] );
  outit( "stack_start", globals[ STK_START_OFFSET ] );
  outit( "stack_limit", globals[ STK_LIMIT_OFFSET ] );
  outit( "stack_ptr", globals[ SP_OFFSET ] );
  outit( "s_base", globals[ S_BASE_OFFSET ] );
  outit( "s_max", globals[ S_MAX_OFFSET ] );
  outit( "current_continuation", globals[ CONTINUATION_OFFSET ] );
}


stats()
{
  outit( "ecollections", globals[ ECOLLECTIONS_OFFSET ] );
  outit( "tcollections", globals[ TCOLLECTIONS_OFFSET ] );
  outit( "wcollected", globals[ WCOLLECTED_OFFSET ] );
  outit( "wallocated", globals[ WALLOCATED_OFFSET ] );
}


load( base, top )
word *base, **top;
{
  char s[ 200 ];

  while (getline( ifp, s )) {
    if (keyword( s ) != NULL) {
      ungetline( s );
      return;
    }
    *((*top)++) = getword( s );
  }
}


/* Doesn't take bytevectors into account, so some bytevectors have spurious
   'W' storage classes in them. This is not worth the bother to fix (yet). */

dump( name, base, top )
char *name;
word *base, *top;
{
  word *p = base;

  fprintf( ofp, "; %s\n", name );
  while (p < top)
    dumpword( *p++ );
}



panic( s )
char *s;
{
  fprintf( stderr, "%s\n", s );
  exit( 1 );
}


dumpword( x )
word x;
{
  word *p;
  
  if (isptr( x )) {
    p = (word *) ptrof( x );
    if (p >= (word *) globals[ E_BASE_OFFSET ] && p <= (word *) globals[ E_MAX_OFFSET ] )
      fprintf( ofp, "E %08lX\n", x - globals[ E_BASE_OFFSET ] );
    else if (p >= (word *) globals[ T_BASE_OFFSET ] && p <= (word *) globals[ T_MAX_OFFSET ])
      fprintf( ofp, "T %08lX\n", x - globals[ T_BASE_OFFSET ] );
    else
      fprintf( ofp, "W %08lX\n", x );    /* "weird" */
  }
  else {
    fprintf( ofp, "C %08lX\n", x );
  }
}


/* Look for keyword, return its function pointer */

struct {
  char *s;
  int (*f)();
} ops[] = {  { "EPHEMERAL", load_ephemeral },
	     { "TENURED", load_tenured },
	     { "ENTRYLIST", load_entrylist },
	     { "ROOTS", load_roots },
	     { "STACK", load_stack },
	     { "NALLOCI", test_nalloci },
	     { "ALLOCI", test_alloci },
	     { "ALLOC", test_alloc },
	     { "E-COLLECT", e_collect },
	     { "T-COLLECT", t_collect },
	     { "S-FLUSH", s_dump },
	     { "SS-FLUSH", sstart_dump },
	     { "E-DUMP", e_dump },
	     { "T-DUMP", t_dump },
	     { "Y-DUMP", y_dump },
	     { "R-DUMP", r_dump },
	     { "S-DUMP", s_dump },
	     { "POINTERS", pointers },
	     { "SUM", test_sum },
	     { "STATS", stats }};

int (*keyword( s ))()
char *s;
{
  int i;

  for (i = 0 ; i < sizeof( ops ) / sizeof( ops[ 0 ] ) ; i++ )
    if (strncmp( s, ops[i].s, strlen( ops[i].s ) ) == 0)
      return ops[i].f;
  return NULL;
}


/* Input stuff */

int ungotten = 0;
char ungottenline[ 200 ];

getline( fp, s )
FILE *fp;
char *s;
{
  if (ungotten) {
    strcpy( s, ungottenline );
    ungotten = 0;
    return 1;
  }

  do {
    if (fgets( s, 200, fp ) == NULL)
      return 0;
    if (strncmp( s, "END", 3 ) == 0)
      return 0;
    if (*s == ';')
      continue;
    return 1;
  } while(1);
}


ungetline( s )
char *s;
{
  if (ungotten)
    panic( "ungetting again!" );
  ungotten = 1;
  strcpy( ungottenline, s );
}

char *strdup( s )
char *s;
{
  char *p;

  if ((p = malloc( strlen( s ) + 1)) == NULL)
    return NULL;
  strcpy( p, s );
  return p;
}
