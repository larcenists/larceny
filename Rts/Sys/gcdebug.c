/* Rts/Sys/gcdebug.c
 * Larceny run-time system -- gc debugging infrastructure
 * 
 * January 22, 1997
 * $Id$
 *
 * Pilfered from pre-0.14 code, and _not_ polished.  This code is as yet
 * only suitable for testing the two-generation copying collector.
 *
 * Heaps are described using the Heap Description Language (HDL).  The 
 * syntax of HDL is line-oriented; in the following BNF, \n means newline.
 *
 * program --> <stmt>* "END" \n
 * stmt    --> "ROOTS" \n <pointer>*              load roots
 *           | "EPHEMERAL" \n <pointer>*          load ephemeral data
 *           | "TENURED" \n <pointer>*            load tenured data
 *           | "SSB" \n <pointer>*                load SSB data
 *           | "STACK" \n <pointer>*              load stack data (top-down)
 *           | "R-DUMP" \n                        dump roots
 *           | "E-DUMP" \n                        dump ephemeral area
 *           | "T-DUMP" \n                        dump tenured area
 *           | "Y-DUMP" \n                        dump remembered sets
 *           | "S-DUMP" \n                        dump (user) stack
 *           | "E-COLLECT" \n                     perform minor collection
 *           | "E-PROMOTE" \n                     promote data
 *           | "T-COLLECT" \n                     perform full collection
 *           | "Y-COMPACT" \n                     compact SSBs
 *           | "S-FLUSH" \n                       flush stack to heap
 *           | <dynamic>
 * dynamic --> "ALLOC" <hex>
 *           | "ALLOCI" <hex> <hex>
 *           | "SETCAR" <word> <word>
 *           | "SETCDR" <word> <word>
 *           | "VECTORSET" <word> <hex> <word>
 *           | "SUM" ??
 *           | "CONSUP" ??
 *           | "POINTERS"
 *
 * pointer --> <word> \n
 *           
 * word    --> ["E","T","C"]<hex>
 * hex     --> ["0"-"9","a"-"f","A"-"F"]+
 *
 * Comment lines start with a semicolon.
 *
 * Lines cannot be blank; if a line does not start with a ';' then it
 * must have a meaningfull program segment.
 * 
 * Dynamic commands are not currently supported.
 */

#include <stdio.h>
#include <ctype.h>
#include <memory.h>
#include "larceny.h"
#include "cdefs.h"
#include "macros.h"
#include "gc.h"
#include "memmgr.h"
#include "gclib.h"

#define mkeptr( x )  ((word) (x) + globals[ G_EBOT ] )
#define mktptr( x )  ((word) (x) + tbase )

static word tbase = 0;
static word *roots = &globals[ FIRST_ROOT ];
static word rootcnt = 0;
static FILE *ifp;
static FILE *ofp;
static gc_t *gc;

static void (*keyword( char * ))( char * );
static char *strdup( const char * );
static int getline( FILE *fp, char *s );
static void ungetline( char *s );
static int load( void );
static word getword( char *s );
static word mkwrd( char s, word w );
static void dump( char *name, word *base, word *top );
static void dumpword( word x );

void gcdebug( char *filename )
{
  char s[ 200 ];
  void (*f)( char * );
  old_param_t old_gen_info = { 0, 0, 0 };

  consolemsg( "Entering GC debug mode, script is '%s'.", filename );

  ofp = stdout;
  if ((ifp = fopen( filename, "r" )) == NULL)
    panic( "Unable to open input file" );

  consolemsg( "Init." );

  gc = create_gc( 0, 0, 0, 0, 0, 1, &old_gen_info, globals );
  gc->initialize( gc );

  consolemsg( "Init done." );

  while (getline( ifp, s )) {
    f = keyword( s );
    if (f != NULL)
      (*f)( s );
    else
      panic( "Error in input line '%s'.", s );
  }

  fclose( ifp );
}




/***************************************************************************
 *
 * Data loading
 */

static word load_area[ 400 ];

static void load_ephemeral( char *s )
{
  int n;

  consolemsg( "EPHEMERAL" );
  n = load();
  memcpy( (word*)globals[ G_ETOP ], load_area, n*sizeof(word) );
  globals[ G_ETOP ] += n*sizeof(word);
}


/* Arguably, there could be a generation # argument */
static void load_tenured( char *s )
{
  word *p;
  int n;

  consolemsg( "TENURED" );
  n = load();
  p = gc->data_load_area( gc, n*sizeof(word) );
  if (tbase == 0) tbase = (word)p;
  memcpy( p, load_area, n*sizeof(word) );
}


/* Arguably, there could be a generation # argument */
static void load_ssb( char *s )
{
#if 0
  ... FIXME ...
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
#endif
}


static void load_roots( char *s )
{
  consolemsg( "ROOTS" );
  rootcnt = load();
  memcpy( roots, load_area, rootcnt*sizeof( word ));
}


/* Stacks are specified from higher addresses toward lower addresses,
 * and frames must be padded in the input.
 */
static void load_stack( char *s )
{
  int n = load(), i;
  word *p = (word*)(globals[G_STKP]);

  consolemsg( "STACK" );
  globals[G_STKP] -= n*sizeof(word);
  for ( i = 0 ; i < n ; i++ )
    *--p = load_area[i];
}


/* load words into load_area[], return number of words */
static int load( void )
{
  char s[ 200 ];
  word *top;

  memset( load_area, 0, sizeof( load_area ) );
  top = load_area;
  while (getline( ifp, s )) {
    if (keyword( s ) != NULL) {
      ungetline( s );
      break;
    }
    *top++ = getword( s );
  }
  return roundup2(top-load_area);
}


static word getword( char *s )
{
  word tmp;

  if (sscanf( s+1, "%lx", &tmp ) != 1)
    panic( "Unable to get word" );
  return mkwrd( *s, tmp );
}


static word mkwrd( char s, word w )
{
  if (s == 'E')
    return mkeptr( w );
  else if (s == 'T')
    return mktptr( w );
  else if (s == 'C')
    return w;
  else
    panic( "Illegal word spec" );
}


/***************************************************************************
 *
 * Collection
 */

static void t_collect( char *s )
{
  consolemsg( "T-COLLECT" );
#if 0
  fprintf( ofp, "; tenuring collection\n" );
  collect( 2 );
#endif
}

static void e_collect( char *s )
{
  consolemsg( "E-COLLECT" );
  gc->collect( gc, 0, GC_COLLECT, 0 );
#if 0
  fprintf( ofp, "; ephemeral collection\n" );
  collect( 1 );
#endif
}


/***************************************************************************
 *
 * Heap dumping.
 */

static void e_dump( char *s )
{
  consolemsg( "E-DUMP" );
  dump( "ephemeral area",
       (word*)(globals[ G_EBOT ]), (word*)(globals[ G_ETOP ]) );
}

static void t_dump( char *s )
{
#if 0
  dump( "tenured area", globals[ T_BASE_OFFSET ], globals[ T_TOP_OFFSET ] );
#endif
}

static void r_dump( char *s )
{
  int i;

  consolemsg( "R-DUMP" );
  fprintf( ofp, "; roots\n" );
  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    dumpword( globals[ i ] );
}

/* Dump stack cache between STK_START (exclusive) and SP (inclusive). */
static void s_dump( char *s )
{
  word *p, *q;

  consolemsg( "S-DUMP" );
  fprintf( ofp, "; user portion of stack cache\n" );
  p = (word *) globals[ G_STKBOT ] - 1;
  q = (word *) globals[ G_STKP ];
  while (p >= q) {
    dumpword( *p );
    p--;
  }
}


/* Dump remembered sets */
static void y_dump( char *s )
{
  consolemsg( "Y-DUMP" );
#if 0
  word *p = (word *) globals[ T_MAX_OFFSET ];

  fprintf( ofp, "; entry list\n" );
  while (p > (word *) globals[ T_TRANS_OFFSET ])
    dumpword( *p-- );
#endif
}


/* Doesn't take bytevectors into account, so some bytevectors have spurious
   'W' storage classes in them. This is not worth the bother to fix (yet). */

static void dump( char *name, word *base, word *top )
{
  fprintf( ofp, "; %s\n", name );
  while (base < top)
    dumpword( *base++ );
}


static void dumpword( word x )
{
  word *p;
  FILE *ofp = stdout;

  if (isptr( x )) {
    p = (word *) ptrof( x );
    if (p >= (word *) globals[ G_EBOT ] && p <= (word *) globals[ G_ELIM ] )
      fprintf( ofp, "E %08lX\n", x - globals[ G_EBOT ] );
#if 0
    else if (p >= (word *) globals[ T_BASE_OFFSET ] && p <= (word *) globals[ T_MAX_OFFSET ])
      fprintf( ofp, "T %08lX\n", x - globals[ T_BASE_OFFSET ] );
#endif
    else
      fprintf( ofp, "W %08lX\n", x );    /* "weird" */
  }
  else {
    fprintf( ofp, "C %08lX\n", x );
  }
}



/***************************************************************************
 *
 * Miscellaneous.
 */

pointers()
{
#if 0
#define outit( s, x )  fprintf( stdout, "; %-20s %08lX\n", s, (word) x )
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
#endif
}


stats()
{
#if 0
  outit( "ecollections", globals[ ECOLLECTIONS_OFFSET ] );
  outit( "tcollections", globals[ TCOLLECTIONS_OFFSET ] );
  outit( "wcollected", globals[ WCOLLECTED_OFFSET ] );
  outit( "wallocated", globals[ WALLOCATED_OFFSET ] );
#endif
}




/* Look for keyword, return its function pointer */

static struct {
  char *s;
  void (*f)( char * );
} ops[] = {  { "EPHEMERAL", load_ephemeral },
	     { "TENURED", load_tenured },
	     { "SSB", load_ssb },
	     { "ROOTS", load_roots },
	     { "STACK", load_stack },
#if 0
	     { "NALLOCI", test_nalloci },
	     { "ALLOCI", test_alloci },
	     { "ALLOC", test_alloc },
	     { "S-FLUSH", s_dump },
	     { "SS-FLUSH", sstart_dump },
#endif
	     { "E-COLLECT", e_collect },
	     { "T-COLLECT", t_collect },
	     { "E-DUMP", e_dump },
	     { "T-DUMP", t_dump },
	     { "Y-DUMP", y_dump },
	     { "R-DUMP", r_dump },
	     { "S-DUMP", s_dump },
#if 0
	     { "POINTERS", pointers },
	     { "SUM", test_sum },
	     { "CONSUP", test_consup },
	     { "SETCAR", test_setcar },
	     { "SETCDR", test_setcdr },
	     { "VECTORSET", test_vectorset },
	     { "STATS", stats }
#endif
	   };

static void (*keyword( char *s ))( char * )
{
  int i;

  for (i = 0 ; i < sizeof( ops ) / sizeof( ops[0] ) ; i++ )
    if (strncmp( s, ops[i].s, strlen( ops[i].s ) ) == 0)
      return ops[i].f;
  return 0;
}


/* Input stuff */

static int ungotten = 0;
static char ungottenline[ 200 ];

static int getline( FILE *fp, char *s )
{
  if (ungotten) {
    strcpy( s, ungottenline );
    ungotten = 0;
    return 1;
  }

  while (1) {
    if (fgets( s, 200, fp ) == NULL)
      return 0;
    if (strncmp( s, "END", 3 ) == 0)
      return 0;
    if (*s == ';')
      continue;
    return 1;
  }
}


static void ungetline( char *s )
{
  if (ungotten)
    panic( "ungetting again!" );
  ungotten = 1;
  strcpy( ungottenline, s );
}

static char *strdup( const char *s )
{
  char *p;

  if ((p = (char*)malloc( strlen( s ) + 1)) == NULL)
    return NULL;
  strcpy( p, s );
  return p;
}

/***************************************************************************
 * 
 * "dynamic" code, unused for now
 */

#if 0
test_alloc( s )
char *s;
{
  unsigned wds;

  s += strlen( "ALLOC" );
  if (sscanf( s, "%x", &wds ) != 1)
    panic( "Illegal ALLOC" );
  millicall( alloc, wds, 0, 0 );
}


test_alloci( s )
char *s;
{
  unsigned wds, init;

  s += strlen( "ALLOCI" );
  if (sscanf( s, "%x %x", &wds, &init ) != 2)
    panic( "Illegal ALLOCI" );
  millicall( alloci, wds, init, 0 );
}


test_nalloci( s )
char *s;
{
  unsigned wds, init, count;

  s += strlen( "NALLOCI" );
  if (sscanf( s, "%x %x %x", &count, &wds, &init ) != 3)
    panic( "Illegal NALLOCI" );

  while (count--)
    millicall( alloci, wds, init, 0 );
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


test_consup( s )
char *s;
{
  unsigned x;
  int i;

  s += strlen( "CONSUP" );
  if (sscanf( s, "%x", &x ) != 1)
    panic( "Illegal CONSUP" );
  fprintf( ofp, "%d %d\n", x/4, listlength( test_consup_2( x ) ) );
  fprintf( ofp, "%lx %d\n",
	  globals[ RESULT_OFFSET ], listlength( globals[ RESULT_OFFSET ] ) );
  pointers();
  collect( EPHEMERAL_COLLECTION );
  pointers();
  fprintf( ofp, "%lx %d\n",
	  globals[ RESULT_OFFSET ], listlength( globals[ RESULT_OFFSET ] ) );
  for ( i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    fprintf( ofp, "%lx\n", globals[ i ] );
}


/* SETCAR <word> <word> */
/* The scanf from hell. */
test_setcar( s )
char *s;
{
  word ww1, ww2, w1, w2;
  char w1s[2], w2s[2];

  s += strlen( "SETCAR" );
  if (sscanf( s, "%*[ ]%[ETC] %lx%*[ ]%[ETC] %lx", w1s, &w1, w2s, &w2 ) != 4)
    panic( "setcar: kaboom (1)");

  ww1 = mkwrd( *w1s, w1 );
  ww2 = mkwrd( *w2s, w2 );
  if (!isptr( ww1 ) || tagof( ww1 ) != PAIR_TAG)
    panic( "setcar: kaboom (2)" );

  millicall( setcar, ww1, ww2, 0 );
}


/* SETCDR <word> <word> */
test_setcdr( s )
char *s;
{
  word ww1, ww2, w1, w2;
  char w1s[2], w2s[2];

  s += strlen( "SETCDR" );
  if (sscanf( s, "%*[ ]%[ETC] %lx%*[ ]%[ETC] %lx", w1s, &w1, w2s, &w2 ) != 4)
    panic( "setcdr: kaboom (1)");

  ww1 = mkwrd( *w1s, w1 );
  ww2 = mkwrd( *w2s, w2 );
  if (!isptr( ww1 ) || tagof( ww1 ) != PAIR_TAG)
    panic( "setcdr: kaboom (2)" );

  millicall( setcdr, ww1, ww2, 0 );
}


/* VECTORSET <word> <int> <word> */
test_vectorset( s )
char *s;
{
  word ww1, ww2, w1, w2, idx;
  char w1s[2], w2s[2];

  s += strlen( "VECTORSET" );
  if (sscanf( s, "%*[ ]%[ETC] %lx %lx%*[ ]%[ETC] %lx",
	      w1s, &w1, &idx, w2s, &w2 ) != 5)
    panic( "vectorset: kaboom (1)");

  ww1 = mkwrd( *w1s, w1 );
  ww2 = mkwrd( *w2s, w2 );
  if (!isptr( ww1 ) || tagof( ww1 ) != VEC_TAG)
    panic( "vectorset: kaboom (2)" );

  millicall( vectorset, ww1, idx, ww2 );
}

/* Dummy entry point */
testtest()
{
  word tmp, chainlength();

  tmp = 0;
  printf( "%lx\n", chainlength( tmp ) );
}

/* Test code to be called from debugger. */

word chainlength( p )
word p;
{
  word *q;
  unsigned tag;
  int l;

  l = 0;
  while (tagof( p ) == VEC_TAG) {
    q = ptrof( p );
    if ((*q & 0xFF) != 0xa6) {
      printf( "non-vector at address p = %lx\n", p );
      return l;
    }
    l++;
    p = *(q+1);
  }
  if (p != FALSE_CONST)
    printf( "abnormal terminating value: %lx\n", p );
  return l;
}

listlength( p )
word p;
{
  int l, space = -1;
  word *q;

  l = 0;
  while (p != NIL_CONST) {
    l++;
    if (tagof( p ) != PAIR_TAG) {
      printf( "non-list: %lx\n", p );
      return l;
    }
    q = ptrof( p );
    if (p >= globals[ E_BASE_OFFSET ] && p <= globals[ E_MAX_OFFSET ]) {
      if (space != 0)
	printf( "going into ephemeral space; l = %d/\n", l );
      space = 0;
    }
    else if (p >= globals[ T_BASE_OFFSET ] && p <= globals[ T_MAX_OFFSET ]) {
      if (space != 2)
	printf( "going into tenured space; l = %d\n", l );
      space = 2;
    }
    else {
      printf( "going into hyperspace; l = %d, p = %lx\n", l, p );
      space = -1;
    }
    p = *(q + 1);
  }
  return l;
}

#endif

/* eof */
