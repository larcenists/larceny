/* Rts/Sys/gc.c
 * Larceny run-time system -- RTS/GC glue code for 0.26.alpha
 * 
 * $Id: gc.c,v 1.14 1997/02/27 16:40:26 lth Exp $
 *
 * The code in this file presents an interface to the new GC that looks
 * mostly like the interface to the old GC.  The purpose of the deception
 * is to allow more gradual changes to the RTS overall.  Eventually, the
 * RTS will be modified to deal with the new GC, and much or all of this file
 * will go away.
 */

#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "gc.h"

static gc_t *gc;
static int  generations;

int allocate_heap( unsigned esize, unsigned ewatermark,
		   unsigned ssize,
		   unsigned rhash, unsigned ssb,
		   unsigned old_generations,
		   old_param_t *old_info,
		   int np_gc, unsigned np_steps, unsigned np_stepsize 
		  )
{
  gc = create_gc( esize, ewatermark, ssize, rhash, ssb, old_generations,
		  old_info, 
		  np_gc, np_steps, np_stepsize,
		  globals,
		  &generations );
  gc->initialize( gc );
  return 1;
}

char *gctype( void )
{
  return gc->id;
}

void init_stats( int show_stats )
{
  stats_init( gc, generations, show_stats );
}

word *alloc_from_heap( unsigned bytes )
{
  return gc->allocate( gc, bytes );
}

void garbage_collect( int type, unsigned request_bytes )
{
  debugmsg( "[debug] Warning: call to obsolete procedure garbage_collect()" );

  switch (type) {
    case EPHEMERAL_COLLECTION :
      gc->collect( gc, 0, GC_COLLECT, request_bytes );
      break;
    case TENURING_COLLECTION :
      gc->collect( gc, 1, GC_PROMOTE, request_bytes );
      break;
    case FULL_COLLECTION :
      gc->collect( gc, 1, GC_COLLECT, request_bytes );
      break;
    default:
      panic( "garbage_collect: bogus type: %d", type );
  }
}

/* If type == 0 then the meaning is: collect in the named generation
 * if type == 1 then the meaning is: promote into the named generation
 */
void garbage_collect3( unsigned gen, unsigned type, unsigned request_bytes )
{
  switch (type) {
  case 0: 
    gc->collect( gc, gen, GC_COLLECT, request_bytes );
    break;
  case 1:
    gc->collect( gc, gen, GC_PROMOTE, request_bytes );
    break;
  default:
    panic( "garbage_collect3: bogus type: %d", type );
  }
}

word creg_get( void ) { return gc->creg_get( gc ); }
void creg_set( word c ) { gc->creg_set( gc, c ); }
void stack_underflow( void ) { gc->stack_underflow( gc ); }

int compact_ssb( void )
{
  return gc->compact_all_ssbs( gc ); 
}

void stack_overflow( void ) 
{
  /* FIXME: this should really be made more abstract! */
  garbage_collect3( 0, 0, 64 );
}

void load_heap( void )
{
  word *sbase = 0;
  word *tbase = 0;

  if (heap_ssize() > 0) {
    sbase = gc->data_load_area( gc, heap_ssize() );
    if (sbase == 0)
      panic( "Static heap too small to load image." );
  }
  tbase = gc->data_load_area( gc, heap_tsize() );
  if (tbase == 0)
    panic( "Dynamic heap too small to load image." );
  load_heap_image( sbase, tbase, globals );
}

int dump_heap( char *filename )
{
#if 0
  /* gc-before-dump is more subtle than it used to be, and with the
   * new heap format, maybe we don't want to do it at all.
   */
  garbage_collect_before_dump();
#endif

  hardconsolemsg("Heap dumping not currently supported (out of chocolate).");
  return -1;
}

/* This is useful mainly for the simulated barrier. */
int isremembered( word p )
{
  return gc->isremembered( gc, p );
}

/* eof */
