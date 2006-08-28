/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- main header file.
 */

#ifndef INCLUDED_LARCENY_H
#define INCLUDED_LARCENY_H

#include <limits.h>
#include <stddef.h>
#include "config.h"
#include "larceny-types.h"
#include "macros.h"
#include "cdefs.h"
#include "assert.h"

#ifdef GC_INTERNAL
#define NOGLOBALS      /* globals[] array is not declared in this file */
#endif

/* Environment variable for where to find Larceny support files: */
#define LARCENY_ROOT   "LARCENY_ROOT"

/* In "Build/table.s" */

#ifndef NOGLOBALS
extern word globals[];
#endif

/* In the garbage collector (Rts/Sys/memmgr.c or Rts/Sys/bdw-collector.c) */

#ifndef GC_INTERNAL
extern const char *larceny_gc_technology;
#endif

/* In "Rts/Sys/larceny.c" */

extern int  panic_exit( const char *fmt, ... );
extern int  panic_abort( const char *fmt, ... );
extern void annoyingmsg( const char *fmt, ... );
extern void supremely_annoyingmsg( const char *fmt, ... );
extern void consolemsg( const char *fmt, ... );
extern void hardconsolemsg( const char *fmt, ... );
extern void conditional_abort( void );

/* In "Rts/Sys/heapio.c" */

#if !defined( GC_INTERNAL ) && !defined( HEAPIO_INTERNAL )
extern int heap_is_bootstrap( void );
extern void load_bootstrap_heap( word *sbase, word *tbase, word *globals );
extern void load_dumped_heap( word *globals );
extern int dump_bootstrap_heap( char *filename, semispace_t *data, 
			        semispace_t *text, word *globals );
extern int dump_dumped_heap( char *filename, gc_t *gc, word *globals );
#endif

/* In "Rts/Sys/gc.c" -- an old-looking front-end for the new collector */

#if !defined( GC_INTERNAL )
extern int  create_memory_manager( gc_param_t *params, int *generations );
extern word *alloc_from_heap( int nbytes );
extern word allocate_nonmoving( int length, int tag );
extern int  load_heap_image_from_file( const char *filename );
extern int  dump_heap_image_to_file( const char *filename );
extern int  reorganize_and_dump_static_heap( const char *filename );
#endif

/* In "Rts/Sys/cglue.c", called only from millicode */

#ifndef GC_INTERNAL
extern void C_allocate( word request );
extern void C_morecore( void );
extern void C_compact_ssb( void );
extern void C_stack_overflow( void );
extern void C_creg_get( void );
extern void C_creg_set( void );
extern void C_restore_frame( void );
extern void C_panic( char *fmt, ... );
extern void C_varargs( void );
extern void C_exception( word code, word pc );
extern void C_debugvsm( void );
extern void C_singlestep( word s );
extern void C_syscall( void );
extern void C_wb_compact( int generation );

#endif /* not GC_INTERNAL */


/* In Rts/Sys/primitive.c, called only as syscalls */

#ifndef GC_INTERNAL
extern char *string2asciiz( word );
extern void primitive_get_stats( word );
extern void primitive_dumpheap( word, word );
extern void primitive_getenv( word );
extern void primitive_setenv( word, word );
extern void primitive_garbage_collect( word, word );
extern void primitive_iflush( word );
extern void primitive_flonum_exp( word, word );
extern void primitive_flonum_log( word, word );
extern void primitive_flonum_sin( word, word );
extern void primitive_flonum_cos( word, word );
extern void primitive_flonum_tan( word, word );
extern void primitive_flonum_asin( word, word );
extern void primitive_flonum_acos( word, word );
extern void primitive_flonum_atan( word, word );
extern void primitive_flonum_sqrt( word, word );
extern void primitive_flonum_sinh( word, word );
extern void primitive_flonum_cosh( word, word );
extern void primitive_flonum_atan2( word, word, word );
extern void primitive_stats_dump_on( word );
extern void primitive_stats_dump_off( void );
extern void primitive_stats_dump_stdout( void );
extern void primitive_gcctl_np( word, word, word );
extern void primitive_block_signals( word );
extern void primitive_allocate_nonmoving( word, word );
extern void primitive_object_to_address( word );
extern void primitive_sysfeature( word v );
extern void primitive_sro( word ptrtag, word hdrtag, word limit );
extern void primitive_exit( word );
#endif


/* In Rts/Sys/sro.c */
extern word sro( gc_t *gc, int p_tag, int h_tag, int limit );


/* In "Rts/Sys/ldebug.c" */

extern void localdebugger( void );
extern void debugvsm( void );

/* In Rts/Sys/osdep-*.c */

#include "osdep.h"

/* In "Rts/Sys/version.c" */

#ifndef GC_INTERNAL
extern int  larceny_major_version;
extern int  larceny_minor_version;
extern char *larceny_version_qualifier;
extern char *user;
extern char *date;
extern char *osname;
extern char *larceny_system_name;
extern char *larceny_heap_name;
extern char *larceny_architecture;
#endif

/* In "Rts/Sys/argv.c" */

#ifndef GC_INTERNAL
extern word allocate_argument_vector( gc_t *gc, int argc, char **argv );
#endif

/* In "Rts/Sys/malloc.c" */

extern void *must_malloc( unsigned bytes );
extern void *must_realloc( void *ptr, unsigned size );

/* In "Rts/Sys/signals.c" */

void setup_signal_handlers( void );

/* In "Rts/Sys/ffi.c" */

void larceny_C_ffi_apply( word trampoline_bytevector,
			  word argument_descriptor,
			  word return_descriptor,
		          word actuals );
void larceny_C_ffi_dlopen( word w_path );
void larceny_C_ffi_dlsym( word w_handle, word w_sym );
void larceny_C_ffi_getaddr( word w_key );
void larceny_C_ffi_convert_and_call( word *proc, word **args, void *result,
				    word *adesc, int rdesc, int argc );
void larceny_peek_bytes( word w_addr, word w_bytevector, word w_count );
void larceny_poke_bytes( word w_addr, word w_bytevector, word w_count );

/* In Rts/$MACHINE/syscall.c */

void larceny_segment_code_address( word w_id, word w_number );

/* In Rts/Sys/syscall.c */

void larceny_syscall( int nargs, int nproc, word *args );

/* In Rts/Sys/callback.c */

void larceny_call( word proc, int argc, word *argv, word *result );

/* In "Rts/Sys/util.c" */

word copy_object( gc_t *gc, word obj );
word box_double( double d );
word box_int( int i );
word box_uint( unsigned u );
unsigned unbox_uint( word w );
int unbox_int( word w );
/* Some of the following may be supplied by the system libraries. */
double rint( double f );
double aint( double f );
int strncasecmp( const char *s1, const char *s2, size_t n );
#if !defined(DEBIAN_STRDUP_WEIRDNESS)
char *strdup( const char * );
#endif

/* Target-specific */
/* NOTE!  For the time being, the SPARC version uses a global
   variable 'globals' and may ignore the globals arguments
   to these functions.  Thus, use only one.
   */

void scheme_init( word *globals );
  /* Initialize the things that need to be initialized once.  May allocate
     memory on the heap; should be called after the entire RTS has been
     initialized, though the heap image need not have been loaded.
     */

void scheme_start( word *globals );
  /* Scheme_start runs a Scheme procedure in the context of the
     given globals vector.  The caller must allocate a stack frame and
     must also place arguments in the register save area in globals, 
     and must set up the argument count in globals[ G_RESULT ].  The
     procedure slot (REG0) must hold a Scheme procedure.

     Scheme_start will initialize the return address slot of the
     frame, and call the procedure.  If the procedure returns, then 
     scheme_start returns to its caller.

     Input:  As explained above.
     Output: Any values left in globals by the Scheme procedure.
     */


/* In "Rts/$MACHINE/cache.c" */

#ifndef GC_INTERNAL
extern void cache_setup( void );
#endif

/* Out-of-memory exception handling */
/* In Rts/Sys/larceny.c */

extern int memfail( int code, char *fmt, ... );

#define MF_MALLOC   0     /* malloc() failed */
#define MF_HEAP     1     /* gclib_alloc_heap() failed */
#define MF_REALLOC  2     /* realloc() failed */
#define MF_CALLOC   3     /* calloc() failed */
#define MF_RTS      4     /* gclib_alloc_rts() failed */

/* Defaults */

/* STACK_ROOM is the number of bytes to add onto memory requests during
 * GC to make sure there is also room for the stack after the collection.
 * It needs to be large enough to accomodate a biggish frame, so that
 * the likelyhood of failure is slight.  The collectors must still work
 * correctly if the allocation double-faults because the frame is really
 * huge.
 */
#define STACK_ROOM              1024

/* Remembered set defaults (not tuned) */
#if 0
#define DEFAULT_REMSET_POOLSIZE   8192     /*  8K elements = 64KB */
#else
#define DEFAULT_REMSET_POOLSIZE   1024     /*  1K elements = 8KB */
#endif
#define DEFAULT_REMSET_TBLSIZE   16384     /* 16K elements = 64KB */
#define DEFAULT_SSB_SIZE         16384     /* 16K elements = 64KB */

/* Selectors for getheaplimit/setheaplimit */
#define HL_TBOT 0
#define HL_TTOP 1
#define HL_TLIM 2
#define HL_SBOT 3
#define HL_STOP 4

/* There are some limits even in Larceny :-) 
 *
 * The size of the largest object is determined by the size field in
 * a vector, bytevector, or procedure header.  In the 32-bit implementation,
 * this field is 24 bits, so the largest object can be 2^24-1 bytes long.
 */

#define LARGEST_OBJECT    16777215
#define BYTE_ALIGNMENT    8
#define WORD_ALIGNMENT    2

/* debugmsg( char *fmt, ... ); */

#ifdef DEBUG
#define debugmsg   consolemsg
#else
#define debugmsg   1?(void)0:(void)
#endif

/* debug2msg( char *fmt, ... ); */

#ifdef DEBUG2
#define debug2msg  consolemsg
#else
#define debug2msg  1?(void)0:(void)
#endif

#endif /* if INCLUDED_LARCENY_H */

/* eof */
