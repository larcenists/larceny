/*
 * This is the file Sys/larceny.h
 *
 * Larceny run-time system (Unix) -- main header file.
 *
 * History
 *   July 1 - 14, 1994 / lth (v0.20)
 *     Major changes for the new version.
 */

#ifndef INCLUDED_LARCENY_H
#define INCLUDED_LARCENY_H

/* basic data type */

typedef unsigned word;

/* In "Build/table.s" */

extern word globals[];

/* In "Sys/larceny.c" */

extern int  panic( /* char *fmt, ... */ );
extern void consolemsg( /* char *fmt, ... */ );
extern void hardconsolemsg( /* char *fmt, ... */ );

/* In "Sys/policy.c" */

extern char *gctype;
extern int  allocate_heap( /* uint esize, 
			      uint tsize, 
			      uint ssize, 
			      uint emark, 
			      uint thimark, 
			      uint tlomark */ );
extern int  free_espace();
extern int  size_espace();
extern int  used_espace();
extern int  free_tspace();
extern int  size_tspace();
extern int  used_tspace();
extern int  size_sspace();
extern word *getheaplimit( /* int which */ );
extern void setheaplimit( /* int which, word *p */ );
extern word *alloc_from_heap( /* int nbytes */ );
extern void garbage_collect( /* int type, int request */ );
extern void enumerate_roots( /* void (*f)() */);

/* In "Sys/gc.c" */

extern word *minor_collection( /* word *oldlo, word *oldhi, word *newlo */ );
extern word *major_collection( /* word *oldlo, word *oldhi, word *newlo */ );

/* In "Sys/remset.c" */

extern int create_remset( /* uint tblsize, uint poolsize, uint ssbsize */ );
extern void clear_remset();
extern int compact_ssb();
extern void enumerate_remset( /* void(*f)() */ );
extern void remset_update_memstats();

/* In "Sys/heapio.c" */

extern void openheap( /* char *filename */ );
extern unsigned heap_ssize();
extern unsigned heap_tsize();
extern void load_heap();
extern void closeheap();
extern int dump_heap( /* char *filename */ );

/* In "Sys/stack.c" */

extern int create_stack();
extern void clear_stack();
extern void flush_stack();
extern int restore_frame();

/* In "Sys/malloc.c" */

extern char *malloc( /* size_t request */ );
extern char *realloc( /* char *obj, size_t newsize */ );
extern char *calloc( /* size_t size, size_t count */ );
extern void free( /* char *obj */ );

/* In "Sys/cglue.c", called only from millicode */

extern void C_garbage_collect( /* fixnum type, fixnum request */ );
extern void C_compact_ssb();
extern void C_stack_overflow();
extern void C_creg_get();
extern void C_creg_set();
extern void C_restore_frame();
extern void C_panic( /* char *fmt, ... */ );
extern void C_varargs();
extern void C_exception( /* fixnum code */ );
extern void C_break();
extern void C_singlestep( /* word s */ );
extern void C_syscall();

/* In "Sys/unix.c", called only as syscalls */

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

/* In "Sys/ldebug.c" */

extern void localdebugger();

/* In "Sys/memstats.c" */

extern void memstat_init( /* int show_heapstats */ );
extern void memstat_before_gc( /* int type */ );
extern void memstat_after_gc();
extern void memstat_framesflushed( /* unsigned n */ );
extern void memstat_transactions_allocated( /* unsigned n */ );
extern void memstat_transactions_scanned( /* unsigned n */ );
extern unsigned memstat_rtclock();
extern void memstat_fillvector( /* word *vp */ );

/* In "Sys/version.c" */

extern char *version;
extern char *user;
extern char *date;
extern char *osname;

/* In "$MACHINE/glue.s" */

extern void scheme_start();

/* In "$MACHINE/cache.c" */

extern void cache_setup();

/* In "Sys/argv.c" */

extern word allocate_argument_vector(/* int argc, char **argv */);

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

/* Remembered set defaults (not tuned) */
#define DEFAULT_REMSET_POOLSIZE   8192     /* 8K elements = 64KB */
#define DEFAULT_REMSET_TBLSIZE   16384     /* 16K elements = 64KB */
#define DEFAULT_SSB_SIZE         16384     /* 16K elements = 64KB */

/* Selectors for getheaplimit/setheaplimit */
#define HL_TBOT 0
#define HL_TTOP 1
#define HL_TLIM 2
#define HL_SBOT 3
#define HL_STOP 4

#endif /* if INCLUDED_LARCENY_H */

/* eof */
