/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- main header file.
 */

#ifndef INCLUDED_LARCENY_H
#define INCLUDED_LARCENY_H

#include <limits.h>
#include "config.h"
#include "larceny-types.h"
#include "macros.h"
#include "cdefs.h"
#include "assert.h"

#ifdef GC_INTERNAL
#define NOGLOBALS      /* globals[] array is not declared in this file */
#endif

/* In "Build/table.s" */

#ifndef NOGLOBALS
extern word globals[];
#endif

/* In the garbage collector (Rts/Sys/memmgr.c or Rts/Sys/bdw-collector.c) */

#ifndef GC_INTERNAL
extern const char *larceny_gc_technology;
#endif

/* In "Rts/Sys/larceny.c" */

extern int  panic( const char *fmt, ... );
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
extern char *gctype( void );
extern int  create_memory_manager( gc_param_t *params );
extern word *alloc_from_heap( int nbytes );
extern word *alloc_bv_from_heap( int nbytes );
extern word allocate_nonmoving( int length, int tag );
extern word standing_room_only( int p_tag, int h_tag, int limit );
extern void garbage_collect3( int gen, int request_bytes );
extern void compact_ssb( void );
extern void init_stats( int show_stats );
extern void policy_control( int heap, int rator, unsigned rand );
extern word creg_get( void );
extern void creg_set( word k );
extern void stack_overflow( void );
extern void stack_underflow( void );

extern int  load_heap_image_from_file( const char *filename );
extern int  dump_heap_image_to_file( const char *filename );
extern int  reorganize_and_dump_static_heap( const char *filename );
#endif

/* In "Rts/Sys/cglue.c", called only from millicode */

#ifndef GC_INTERNAL
extern void C_allocate( word request );
extern void C_garbage_collect( void );
extern void C_compact_ssb( void );
extern void C_stack_overflow( void );
extern void C_creg_get( void );
extern void C_creg_set( void );
extern void C_restore_frame( void );
extern void C_panic( char *fmt, ... );
extern void C_varargs( void );
extern void C_exception( word code, word pc );
extern void C_break( void );
extern void C_singlestep( word s );
extern void C_syscall( void );
extern void C_wb_compact( int generation );

#endif /* not GC_INTERNAL */

/* In "Rts/Sys/unix.c", called only as syscalls */

#ifndef GC_INTERNAL
extern void UNIX_openfile( word, word, word );
extern void UNIX_unlinkfile( word );
extern void UNIX_closefile( word );
extern void UNIX_readfile( word, word, word );
extern void UNIX_writefile( word, word, word, word );
extern void UNIX_getresourceusage( word );
extern void UNIX_dumpheap( word, word );
extern void UNIX_exit( word );
extern void UNIX_mtime( word, word );
extern void UNIX_access( word, word );
extern void UNIX_rename( word, word );
extern void UNIX_pollinput( word );
extern void UNIX_getenv( word );
extern void UNIX_garbage_collect( word, word );
extern void UNIX_iflush( word );
extern void UNIX_flonum_exp( word, word );
extern void UNIX_flonum_log( word, word );
extern void UNIX_flonum_sin( word, word );
extern void UNIX_flonum_cos( word, word );
extern void UNIX_flonum_tan( word, word );
extern void UNIX_flonum_asin( word, word );
extern void UNIX_flonum_acos( word, word );
extern void UNIX_flonum_atan( word, word );
extern void UNIX_flonum_sqrt( word, word );
extern void UNIX_flonum_sinh( word, word );
extern void UNIX_flonum_cosh( word, word );
extern void UNIX_flonum_atan2( word, word, word );
extern void UNIX_stats_dump_on( word );
extern void UNIX_stats_dump_off( void );
extern void UNIX_gcctl_np( word, word, word );
extern void UNIX_block_signals( word );
extern void UNIX_system( word );
extern void UNIX_allocate_nonmoving( word, word );
extern void UNIX_object_to_address( word );
extern void UNIX_sysfeature( word v );
extern void UNIX_sro( word ptrtag, word hdrtag, word limit );
#endif

/* In "Rts/Sys/ldebug.c" */

extern void localdebugger( void );

/* In "Rts/Sys/stats.c" */

typedef enum { STATS_PROMOTE, STATS_COLLECT, STATS_IGNORE } stats_gc_t;

#ifndef GC_INTERNAL
extern void stats_init( gc_t *gc, int generations, int show_heapstats );
extern word stats_fillvector( word w_buf );
extern int  stats_opendump( const char *filename );
extern void stats_closedump( void );
#endif
extern void stats_before_gc( void );
extern void stats_gc_type( int gen, stats_gc_t type );
extern void stats_after_gc( void );
extern unsigned stats_rtclock( void );
extern void stats_add_gctime( long s, long ms );

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
extern word allocate_argument_vector( int argc, char **argv );
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

/* In "Rts/$MACHINE/glue.s" */

#ifndef GC_INTERNAL
extern void scheme_start( word *globals );
#endif

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

/* GC policy defaults (not tuned) */
#if 0
/* Old policy */
#define DEFAULT_EWATERMARK 50     /* espace > 50% full => tenure */
#define DEFAULT_TOFLOWATERMARK 75 /* tspace > 75% full => promote */
#define DEFAULT_THIWATERMARK 75   /* tspace > 75% full => expand */
#define DEFAULT_TLOWATERMARK 50   /* tspace < 50% full => contract */
#define DEFAULT_RWATERMARK 75     /* remset-pool > 75% full => tenure */
#define DEFAULT_SC_HIWATERMARK 75  /* stop+copy high watermark */
#define DEFAULT_SC_LOWATERMARK 30  /* stop+copy low watermark */
#define DEFAULT_NP_HIWATERMARK    80    /* NP expansion watermark */
#define DEFAULT_NP_LOWATERMARK    30    /* NP contraction watermark */
#define DEFAULT_NP_OFLOWATERMARK  80    /* NP promotion watermark */
#else
/* New policy, tuned by Will, although the 0 entries must be fixed. */
#define DEFAULT_EWATERMARK 25     /* espace > 25% full => tenure */
#define DEFAULT_TOFLOWATERMARK 66 /* tspace > 66% full => promote */
#define DEFAULT_THIWATERMARK 66   /* tspace > 66% full => expand */
#define DEFAULT_TLOWATERMARK 0    /* tspace <  0% full => contract */
#define DEFAULT_RWATERMARK 75     /* remset-pool > 75% full => tenure */
#define DEFAULT_SC_HIWATERMARK 66  /* stop+copy high watermark */
#define DEFAULT_SC_LOWATERMARK  0  /* stop+copy low watermark */
#define DEFAULT_NP_HIWATERMARK    80    /* NP expansion watermark */
#define DEFAULT_NP_LOWATERMARK     0    /* NP contraction watermark */
#define DEFAULT_NP_OFLOWATERMARK  80    /* NP promotion watermark */
#endif

#define OLDSPACE_EXPAND_BYTES   (1024*256)  /* 256KB chunks */

/* STACK_ROOM is the number of bytes to add onto memory requests during
 * GC to make sure there is also room for the stack after the collection.
 * It needs to be large enough to accomodate a biggish frame, so that
 * the likelyhood of failure is slight.  The collectors must still work
 * correctly if the allocation double-faults because the frame is really
 * huge.
 */
#define STACK_ROOM              1024

/* Remembered set defaults (not tuned) */
#define DEFAULT_REMSET_POOLSIZE   8192     /*  8K elements = 64KB */
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

#ifndef PATH_MAX
# ifdef _POSIX_PATH_MAX
#  define PATH_MAX _POSIX_PATH_MAX
# else
#  define PATH_MAX 1024
# endif
#endif

#endif /* if INCLUDED_LARCENY_H */

/* eof */
