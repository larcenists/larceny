/* Rts/Sys/larceny.h
 * Larceny run-time system -- main header file
 *
 * $Id: larceny.h,v 1.12 1997/02/23 01:15:32 lth Exp $
 */

#ifndef INCLUDED_LARCENY_H
#define INCLUDED_LARCENY_H

#ifdef GC_INTERNAL
#define NOGLOBALS      /* globals[] array is not declared in this file */
#endif

/* Fundamental data type. */

typedef unsigned word;

#ifndef GC_INTERNAL
#include "gc.h"
#endif

/* In "Build/table.s" */

#ifndef NOGLOBALS
extern word globals[];
#endif

/* In "Rts/Sys/larceny.c" */

extern int  panic( const char *fmt, ... );
extern int  panic_abort( const char *fmt, ... );
extern void annoyingmsg( const char *fmt, ... );
extern void consolemsg( const char *fmt, ... );
extern void hardconsolemsg( const char *fmt, ... );

/* In "Rts/Sys/heapio.c" */

#ifndef GC_INTERNAL
extern void openheap( char *filename );
extern unsigned heap_ssize( void );
extern unsigned heap_tsize( void );
extern void closeheap( void );
extern void load_heap_image( word *sbase, word *tbase, word *globals );
extern int dump_heap_image( char *filename );
#endif

/* In "Rts/Sys/gc.c" -- an old-looking front-end for the new collector */

#ifndef GC_INTERNAL
extern int  allocate_heap( unsigned esize, unsigned emark, 
			   unsigned ssize, 
			   unsigned rhash, unsigned ssb,
			   unsigned oldgen,
			   old_param_t *info,
			   int np_gc,
			   unsigned np_steps,
			   unsigned np_stepsize 
			  );
extern word *alloc_from_heap( unsigned );
extern void garbage_collect( int, unsigned );
extern void garbage_collect3( unsigned, unsigned, unsigned );
extern int  compact_ssb( void );
extern void load_heap( void );
extern int  dump_heap( char *filename );
extern void init_stats( int show_stats );
extern word creg_get( void );
extern void creg_set( word k );
extern void stack_overflow( void );
extern void stack_underflow( void );
#endif

/* In "Rts/Sys/cglue.c", called only from millicode */

#ifndef GC_INTERNAL
extern void C_garbage_collect( word type, word request );
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

#if SIMULATE_NEW_BARRIER
typedef struct {
  unsigned array_assignments;
  unsigned lhs_young_or_remembered;
  unsigned rhs_constant;
  unsigned cross_gen_check;
  unsigned transactions;
} simulated_barrier_stats_t;
extern void C_simulate_new_barrier( void );
extern void simulated_barrier_stats( simulated_barrier_stats_t * );
#endif

#endif

/* In "Rts/Sys/unix.c", called only as syscalls */

#ifndef GC_INTERNAL
extern void UNIX_openfile();
extern void UNIX_unlinkfile();
extern void UNIX_closefile();
extern void UNIX_readfile();
extern void UNIX_writefile();
extern void UNIX_getresourceusage();
extern void UNIX_dumpheap();
extern void UNIX_mtime();
extern void UNIX_access();
extern void UNIX_rename();
extern void UNIX_pollinput();
extern void UNIX_getenv();
extern void UNIX_garbage_collect();
extern void UNIX_iflush();
extern void UNIX_flonum_exp();
extern void UNIX_flonum_log();
extern void UNIX_flonum_sin();
extern void UNIX_flonum_cos();
extern void UNIX_flonum_tan();
extern void UNIX_flonum_asin();
extern void UNIX_flonum_acos();
extern void UNIX_flonum_atan();
extern void UNIX_flonum_atan2();
extern void UNIX_flonum_sqrt();
extern void UNIX_stats_dump_on();
extern void UNIX_stats_dump_off();
#endif

/* In "Rts/Sys/ldebug.c" */

#ifndef GC_INTERNAL
extern void localdebugger();
#endif

/* In "Rts/Sys/stats.c" */

typedef enum { STATS_PROMOTE, STATS_COLLECT } stats_gc_t;

#ifndef GC_INTERNAL
extern void stats_init( gc_t *gc, int generations, int show_heapstats );
extern word stats_fillvector( void );
extern int  stats_opendump( const char *filename );
extern void stats_closedump( void );
#endif
extern void stats_before_gc( void );
extern void stats_gc_type( int gen, stats_gc_t type );
extern void stats_after_gc( void );
extern unsigned stats_rtclock( void );

/* In "Rts/Sys/version.c" */

#ifndef GC_INTERNAL
extern char *version;
extern char *user;
extern char *date;
extern char *osname;
#endif

/* In "Rts/Sys/argv.c" */

#ifndef GC_INTERNAL
extern word allocate_argument_vector( int argc, char **argv );
#endif

/* In "Rts/$MACHINE/glue.s" */

#ifndef GC_INTERNAL
extern void scheme_start( void );
#endif

/* In "Rts/$MACHINE/cache.c" */

#ifndef GC_INTERNAL
extern void cache_setup( void );
#endif

/* Out-of-memory exception handling */

extern int memfail( int code, char *fmt, ... );

#define MF_MALLOC   0     /* malloc() failed */
#define MF_HEAP     1     /* gclib_alloc_heap() failed */
#define MF_REALLOC  2     /* realloc() failed */
#define MF_CALLOC   3     /* calloc() failed */
#define MF_RTS      4     /* gclib_alloc_rts() failed */

/* Defaults */

/* Heap defaults (appropriate for compiler-less heap) */
#define DEFAULT_ESIZE (1024*1024)     /* default espace size = 1MB */
#define DEFAULT_TSIZE (1024*1024*2)   /* default tspace size = 2MB */
#define DEFAULT_SSIZE 0               /* default static size = 0 */

/* GC policy defaults (not tuned) */
#define DEFAULT_EWATERMARK 50     /* espace > 50% full => tenure */
#define DEFAULT_THIWATERMARK 75   /* tspace > 75% full => expand */
#define DEFAULT_TLOWATERMARK 50   /* tspace < 50% full => contract */
#define DEFAULT_RWATERMARK 75     /* remset-pool > 75% full => tenure */

#define OLDSPACE_EXPAND_BYTES   (1024*256)  /* 256KB chunks */

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
