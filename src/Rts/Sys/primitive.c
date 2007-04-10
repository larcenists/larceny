/* Copyright 1998 Lars T Hansen
 *
 * $Id$
 *
 * Operating system independent syscall primitives.
 */

#include <errno.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "larceny.h"
#include "signals.h"
#include "gc_t.h"
#include "gc.h"
#include "stats.h"

extern void mem_icache_flush( void *lo, void *limit );

void primitive_get_stats( word w_buffer )
{
  globals[ G_RESULT ] = stats_fillvector( w_buffer );  /* Rts/Sys/stats.c */
}

void primitive_dumpheap( word w_fn, word w_proc )
{
  char *fn;

  fn = string2asciiz( w_fn );                  /* heap file name */
  globals[ G_STARTUP ] = w_proc;               /* startup procedure */

  if (fn == 0 || dump_heap_image_to_file( fn ) == -1)
    globals[ G_RESULT ] = FALSE_CONST;
  else 
    globals[ G_RESULT ] = TRUE_CONST;
}

void primitive_getenv( w_envvar )
word w_envvar;
{
  char *p;
  word *q;
  int l;

  p = getenv( string2asciiz( w_envvar ) );
  if (p == 0) {
    globals[ G_RESULT ] = FALSE_CONST;
    return;
  }
  l = strlen( p );
  q = (word*)gc_allocate( the_gc(globals), (4 + l), 0, 1 );
  *q = mkheader( l, (BV_HDR | STR_SUBTAG) );
  memcpy( string_data( q ), p, l );
  globals[ G_RESULT ] = (word)tagptr( q, BVEC_TAG );
}

void primitive_setenv( word w_name, word w_value )
{
  int rv;
  char *name;

  name  = strdup( string2asciiz( w_name ) );
  rv = osdep_setenv( name, string2asciiz( w_value ), 1 );
  free( name );

  globals[ G_RESULT ] = rv ? FALSE_CONST : TRUE_CONST;
}

void primitive_garbage_collect( w_gen, w_type )
word w_gen;			/* fixnum: generation */
word w_type;			/* fixnum: type requested */
{
  gc_collect( the_gc( globals ), 
	      nativeint( w_gen ), 
	      0,
	      (w_type ? GCTYPE_COLLECT : GCTYPE_PROMOTE ) );
}

void primitive_iflush( w_bv )
word w_bv;
{
  mem_icache_flush( ptrof( w_bv )+1, 
		    ptrof( w_bv )+roundup4(sizefield(*ptrof(w_bv)))/4 );
}

/* Floating-point operations */

#define flonum_val( p )    (*(double*)((char*)(p)-5+8))
#define box_flonum( p, v ) (*(double*)((char*)(p)-5+8) = (v))

/* One-argument math operations */
#define numeric_onearg( name, op ) \
  void name( w_flonum, w_result ) \
  word w_flonum, w_result; \
  { \
    box_flonum( w_result, op( flonum_val( w_flonum ) ) ); \
    globals[ G_RESULT ] = w_result; \
  }

numeric_onearg( primitive_flonum_log, log )
numeric_onearg( primitive_flonum_exp, exp )
numeric_onearg( primitive_flonum_sin, sin )
numeric_onearg( primitive_flonum_cos, cos )
numeric_onearg( primitive_flonum_tan, tan )
numeric_onearg( primitive_flonum_asin, asin )
numeric_onearg( primitive_flonum_acos, acos )
numeric_onearg( primitive_flonum_atan, atan )
numeric_onearg( primitive_flonum_sinh, sinh )
numeric_onearg( primitive_flonum_cosh, cosh )

void primitive_flonum_atan2( w_flonum1, w_flonum2, w_result )
word w_flonum1, w_flonum2, w_result;
{
  box_flonum( w_result, atan2(flonum_val(w_flonum1), flonum_val(w_flonum2)) );
  globals[ G_RESULT ] = w_result;
}

numeric_onearg( primitive_flonum_sqrt, sqrt )

/* Statistics dump interface */

void primitive_stats_dump_on( w_fn )
word w_fn;
{
  char *fn = string2asciiz( w_fn );
  globals[ G_RESULT ] = fixnum( stats_opendump( fn ) );
}

void primitive_stats_dump_off( void )
{
  stats_closedump();
}

void primitive_stats_dump_stdout( void )
{
  stats_dumpstate_stdout();
}

void primitive_gcctl_np( word heap, word rator, word rand )
{
  /* Heap# comes in as 1..n, but RTS uses 0..n-1 */
  gc_set_policy( the_gc( globals ),
		 nativeint( heap )-1,
		 nativeint( rator ), 
		 (unsigned)nativeint( rand ) );
}

void primitive_block_signals( word code )
{
  static signal_set_t old_mask;

  if (code == fixnum(1))
    block_all_signals( &old_mask );
  else if (code == fixnum(0))
    unblock_signals( &old_mask );
}

void primitive_allocate_nonmoving( word w_length, word w_tag )
{
  globals[ G_RESULT ] = 
    allocate_nonmoving( nativeint( w_length ), nativeint( w_tag ) );
}

void primitive_object_to_address( word w_obj )
{
  /* Invariant: the pointer _must_ point to nonrelocatable memory,
   * or the result may be completely invalid.
   * FIXME: can't we check that this invariant holds?
   */
  if (isptr(w_obj))
    globals[ G_RESULT ] = box_uint( (word)ptrof( w_obj ) );
  else
    globals[ G_RESULT ] = w_obj;
}

void primitive_sysfeature( word v /* a vector of sufficient length */ )
{
  int major, minor, ans[4];

  switch (nativeint(vector_ref( v, 0 ))) {
  case 0  : /* larceny-major */
    vector_set( v, 0, fixnum( larceny_major_version ) );
    break;
  case 1  : /* larceny-minor */
    vector_set( v, 0, fixnum( larceny_minor_version ) );
    break;
  case 2  : /* os-major */
    osdep_os_version( &major, &minor );
    vector_set( v, 0, fixnum( major ) );
    break;
  case 3  : /* os-minor */
    osdep_os_version( &major, &minor );
    vector_set( v, 0, fixnum( minor ) );
    break;
  case 4  : /* gc-info */
    gc_parameters( the_gc( globals ), 0, ans );
    vector_set( v, 0, fixnum(ans[0]) );	/* technology */
    vector_set( v, 1, fixnum(ans[1]) );	/* generations */
    break;
  case 5  : /* gen-info, generations numbered 1..n */
    ans[0] = 0; ans[1] = 0; ans[2] = 0; ans[3] = 0;
    gc_parameters( the_gc( globals ), nativeint(vector_ref( v, 1 )), ans );
    vector_set( v, 0, fixnum(ans[0]) ); /* type */
    vector_set( v, 1, fixnum(ans[1]) ); /* size */
    vector_set( v, 2, fixnum(ans[2]) ); /* parameter (maybe) */
    vector_set( v, 3, fixnum(ans[3]) ); /* parameter (maybe) */
    break;
  case 6 : /* arch-name */
    if (strcmp( larceny_architecture, "SPARC" ) == 0)
      vector_set( v, 0, fixnum(0) );
    else if (strcmp( larceny_architecture, "Standard-C" ) == 0)
      vector_set( v, 0, fixnum(1) );
    else if (strcmp( larceny_architecture, "X86-NASM" ) == 0)
      vector_set( v, 0, fixnum(2) );
    else if (strcmp( larceny_architecture, "IAssassin" ) == 0)
      vector_set( v, 0, fixnum(3) );
    else
      vector_set( v, 0, fixnum(-1) );
    break;
  case 7 : /* os-name */
#if defined(SUNOS4) || defined(SUNOS5)
    vector_set( v, 0, fixnum(0) );
#elif defined(LINUX)
    vector_set( v, 0, fixnum(1) );
#elif defined(MACOS)
    vector_set( v, 0, fixnum(2) );
#elif defined(WIN32)
    vector_set( v, 0, fixnum(3) );
#elif defined(MACOS_X) // must come before BSD_UNIX; both features are set on MacOS X
    vector_set( v, 0, fixnum(8) );
#elif defined(BSD_UNIX)
    vector_set( v, 0, fixnum(7) );
#elif defined(UNIX)  // Generic unix, this case should come after other Unix variants
    vector_set( v, 0, fixnum(5) );
#else
#error "Unknown operating system."
#endif
    break;
  case 8 : /* endianness */
#if defined(BIG_ENDIAN)
    vector_set( v, 0, fixnum( 0 ) );
#elif defined(ENDIAN_LITTLE)
    vector_set( v, 0, fixnum( 1 ) );
#else
#error "No endianness."
#endif
    break;
  case 9 : /* stats-generations */
    vector_set( v, 0, fixnum( stats_parameter( 0 ) ) );
    break;
  case 10 : /* stats-remsets */
    vector_set( v, 0, fixnum( stats_parameter( 1 ) ) );
    break;
  case 11 : /* codevec */
#if defined PETIT_LARCENY || defined X86_NASM
# if defined CODEPTR_SHIFT2
    vector_set( v, 0, fixnum( 3 ) );  // pointer shifted two bits
# elif defined CODEPTR_SHIFT1
    vector_set( v, 0, fixnum( 2 ) );  // pointer shifted one bit
# else
    vector_set( v, 0, fixnum( 1 ) );  // raw pointer
# endif
#else
    vector_set( v, 0, fixnum( 0 ) );  // bytevector
#endif
    break;
  default : 
    panic_exit( "Unknown code %d passed to primitive_sysfeature", nativeint( vector_ref( v, 0 ) ) );
  }
}

/* primitive_sro: implements SRO operation. */
void primitive_sro( word w_ptrtag, word w_hdrtag, word w_limit )
{
  int ptrtag = (int)nativeint(w_ptrtag);
  int hdrtag = (int)nativeint(w_hdrtag);
  int limit = (int)nativeint(w_limit);

  supremely_annoyingmsg( "SRO %d %d %d", ptrtag, hdrtag, limit );
  globals[ G_RESULT ] = sro( the_gc( globals ), ptrtag, hdrtag, limit);
}

void primitive_exit( word code )
{
  exit( nativeint( code ) );
}

/* Copy a file name from a Scheme string to a C string. */
char *string2asciiz( word w_str )
{
  static char *fnbuf = 0;
  static int fnbuf_length = 0;
  size_t l;

  l = string_length( w_str );
  if (l >= fnbuf_length) {
    if (fnbuf != 0) free( fnbuf );
    fnbuf = must_malloc( l+1 );
    fnbuf_length = l+1;
  }
  strncpy( fnbuf, string_data( w_str ), l );
  fnbuf[ l ] = 0;
  return fnbuf;
}

void primitive_errno( void )
{
  globals[ G_RESULT ]= fixnum( errno );
}

void primitive_seterrno( w_errcode )
word w_errcode;
{
  errno = nativeint( w_errcode );
}

/* eof */
