/* Rts/Sys/gc-interface.h
 * Larceny run-time system -- garbage collector interface (internal).
 * 
 * $Id: memmgr.h,v 1.8 1997/05/15 00:58:49 lth Exp lth $
 *
 * A garbage collector is an ADT of type gc_t; see the file Rts/Sys/gc.h.
 * 
 * A garbage collector is created by calling the garbage collector creation
 * function, which can have any name, and the argument list of which
 * is not fixed but rather dependent on the external environment in which
 * the system is running.  The RTS initialization code and the gc must
 * simply agree on its name and arguments.  The creation function is declared 
 * in the file Rts/Sys/gc.h.
 * 
 * A garbage collector manages one young heap and zero or more old heaps;
 * young heaps are ADTs of type young_heap_t and old heaps are ADTs of type
 * old_heap_t.  The GC also manages a set of remembered sets, one remembered
 * set for each older generation (_not_ for each heap; see below).  The 
 * remembered sets are ADTs of type remset_t.  The write barrier adds entries
 * to the remembered sets, but the write barrier specification and 
 * implementation are outside the scope of the garbage collector.
 * 
 * Each of the ADTs are represented as structures that contain function
 * pointers for the heap operations.
 * 
 * Each heap implementation defines a (global) heap creation function, which,
 * when called, returns the appropriate ADT.  These functions can have any
 * name; in the interface below, they are called
 * 
 *      create_young_heap()     create Cheney heap (fixed size)
 *      create_old_heap()       create Cheney heap (adjustable size)
 * 
 * There may be more than one generation per heap.  The heap must manage
 * all internal generations itself: promotion can only take place into the
 * youngest generation of a heap, and the heap's collect() method can only
 * be called for the heap's youngest generation.  Multiple generations are
 * allowed in order to make things easier for the non-predictive collector.
 * 
 * The high-level interface to the GC is implemented in Rts/Sys/memmgr.c.  
 * 
 * The following are some comments about protocol and control flow in the
 * collector; you will need to look at the GC innards to appreciate it fully.
 * 
 * 1) When the running program fails to allocate memory, a call-out to C is
 *    made and the call eventually ends up in the collect() method of the
 *    current gc object, call this gc::collect() (see memmgr.c:collect()).
 * 
 * 2) gc::collect() makes a call to young_heap::collect() to force a
 *    collection in the young heap and and free up space.  
 * 
 * 2.1) young_heap::collect() can either collect or ask to have its objects
 *      promoted, based on internal state.  
 * 
 * 2.1.1) If young_heap::collect() collects, then control returns to
 *        gc::collect(); goto 2.2.
 * 
 * 2.1.2) If young_heap::collect() decides to promote, then the way it does
 *        this is by calling gc::promote_out_of(), passing its generation 
 *        number (0) as an argument.  Goto 3.
 * 
 * 2.2) If the collection freed up enough space to satisfy the request, then
 *      control returns to the running program.  Otherwise, gc::collect()
 *      will decide to promote the young objects to make space.  It does this
 *      by calling gc::promote_out_of(), passing the young heap's generation 
 *      number (0).  Goto 3.
 * 
 * 3) gc::promote_out_of() will call the before_promotion() method on all
 *    heaps as young as or younger than the one passed as an argument, then
 *    call the next older heap's promote() method (goto 4), and then call
 *    the after_promotion() methods.  However, as will become apparent, 
 *    older generations can re-enter gc::promote_out_of() due to policy-based
 *    decisions about when to promote, and gc::promote_out_of() has to be
 *    careful to call before_promotion() and after_promotion() only once
 *    per heap in a collection cycle (which ends when objects are copied by
 *    a collection or a promotion).
 * 
 * 4) The promote() method of a heap must copy all live objects from
 *    younger heaps into its heap.  However, it may decide to collect instead
 *    (collecting all younger generations at the same time, thereby pulling
 *    in all live objects), if, for example, the heap is already full (see 5). 
 *    If the promotion takes place, objects are copied into the heap (overflow
 *    _must_ be handled; choosing to handle overflow by calling the promote
 *    method on a yet older generation is verboten), and control returns to 
 *    gc::promote_out_of().
 * 
 * 5) When the promote() method choses to collect, it (probably) calls its
 *    internal collection method, but that method may decide that things 
 *    need to be promoted (if, for example, the heap was over the watermark
 *    after the previous collection).  It does this by calling 
 *    gc::promote_out_of() (goto 3).  If not, collection takes place, and 
 *    control returns to gc::promote_out_of.
 *
 * The garbage collector implementation makes use of a low-level library
 * that handles copying, memory allocation, and so on.  The interface to this
 * library is defined in the file Rts/Sys/gclib.h.
 *
 *
 * Idea for an even better structure:
 *
 * Currently control-flow among collector modules is somewhat convoluted
 * because individual collectors make policy decisions by calling back into
 * the memmgr code to pass control to the collectors of older generations.
 * A better structure would be the following: each collector has a method
 * for policy decisions, and that method is called by the memmgr and gets
 * to decide whether it wants to promote, collect, or pass the buck.  The
 * memmgr can then just call the correct method in the pertinent heap after
 * all the policy decisions have been made.
 */

#ifndef INCLUDED_GC_INTERFACE_H
#define INCLUDED_GC_INTERFACE_H

#include "gc.h"

typedef struct young_heap young_heap_t;
typedef struct old_heap old_heap_t;
typedef struct static_heap static_heap_t;
typedef struct remset remset_t;
typedef struct remset_stats remset_stats_t;


/***************************************************************************
 *
 * Young Heaps.
 *
 * All allocation happens in the young heap, and the young heap also manages
 * the stack of continuation frames (if any).
 *
 * The function create_young_heap() is defined in Rts/Sys/young-heap.c.
 *
 * Interface:
 *
 * The slot "collector" is a pointer to the garbage collector that manages
 * the particular young heap.
 *
 * initialize() is called after all heaps have been allocated, to finalize
 * initialization.
 *
 * allocate() allocates the requested number of bytes and returns an untagged
 * pointer to the first word of the allocated block.
 *
 * collect() starts a garbage collection.  The "type" parameter is the
 * generation to collect (that and all younger generations will be collected);
 * the "nbytes" parameter is the number of bytes that the collection must
 * free up for the program to be able to continue execution.
 *
 * before_promotion() must be called before promotion of objects out of
 * the young heap.
 *
 * after_promotion() must be called after promotion of objects out of the
 * young heap has taken place.
 *
 * free_space() returns the number of free bytes in the young heap.
 *
 * stats() fills in the statistics structure with GC statistics.
 *
 * data_load_area() returns a pointer to a data load area; see the definition
 * of data_load_area() in Rts/Sys/memmgr.c for more information.
 *
 *
 * The stack functions are somewhat closely tied to the rest of the RTS:
 * 
 * clear_stack() clears the stack of continuation frames.
 *
 * flush_stack() copies all stack frames from the stack into the heap
 * (a continuation capture).
 *
 * restore_frame() restores one stack frame from the heap into the stack
 * cache.
 */

young_heap_t *
create_young_heap( int *gen_no, int heap_no,
		   unsigned size_bytes, unsigned watermark, word *globals );
young_heap_t *
create_sc_heap( int *gen_no, int heap_no, 
	        unsigned size_bytes, unsigned himark, unsigned lomark,
	        word *globals );

struct young_heap {
  gc_t     *collector;
  char     *id;

  int      (*initialize)( young_heap_t *heap );
  word     *(*allocate)( young_heap_t *heap, unsigned nbytes );
  void     (*collect)( young_heap_t *heap, unsigned nbytes );
  void     (*assert_free_space)( young_heap_t *heap, unsigned nbytes );
  void     (*before_promotion)( young_heap_t *heap );
  void     (*after_promotion)( young_heap_t *heap );

  /* Statistics */
  unsigned (*free_space)( young_heap_t *heap );
  void     (*stats)( young_heap_t *heap, heap_stats_t *stats );

  /* Heap loading support */
  word     *(*data_load_area)( young_heap_t *heap, unsigned nbytes );

  /* Continuation management */
  word     (*creg_get)( young_heap_t *heap );
  void     (*creg_set)( young_heap_t *heap, word k );
  void     (*stack_underflow)( young_heap_t *heap );
  void     (*stack_overflow)( young_heap_t *heap );

  /* Private */
  void     *data;
};


/**************************************************************************
 *
 * Old Heaps.
 *
 * An old heap is a heap that receives new objects by promotion from
 * younger heaps, not by direct allocation.  Promotion from younger heaps
 * into an older heap is handled by the older heap itself: it scans
 * itself and all remembered sets of older heaps and copies younger 
 * objects into the heap in the process.
 *
 * create_old_heap() creates the heap but may leave some initialization
 * undone until after all heaps have been created.
 *
 * Interface:
 *
 * The field "collector" is a pointer to the garbage collector that
 * manages this heap.
 *
 * The field "oldest" is 1 iff this is the oldest heap in the system
 * (and therefor may not promote out).
 *
 * initialize() is called after all heaps have been created and must
 * finish the initialization of the heap.
 *
 * collect() must garbage collect the heap, promoting all live objects
 * from younger heaps into the heap.  Before collect() is called,
 * the before_promotion() method has been called on all younger heaps.
 *
 * promote_from_younger() promotes all objects from younger heaps into 
 * this heap by copying them.  Before promote() is called, the 
 * before_promotion() method has been called on all younger heaps.
 *
 * before_promotion() must save any internal state of the heap in well
 * defined locations so that promotion can take place.
 *
 * after_promotion() recreates any internal state the heap has from
 * values stored in well defined locations.
 *
 * stats() fills the passed structure with statistics about the heap.
 *
 * data_load_area() returns a pointer to a data load area; see the definition
 * of data_load_area() in Rts/Sys/memmgr.c for more information.
 */

old_heap_t *
create_old_heap( int *gen_no, int heap_no,
		unsigned size_bytes,
		unsigned hiwatermark,
		unsigned lowatermark,
		unsigned oflowatermark );

old_heap_t *
create_old_np_sc_heap( int *gen_no, int heap_no, 
		       unsigned size_bytes, unsigned chunksize_bytes,
		       unsigned hi_mark, unsigned lo_mark, unsigned oflo_mark
		      );

struct old_heap {
  gc_t *collector;
  char *id;

  int  oldest;
  int  (*initialize)( old_heap_t *heap );
  void (*collect)( old_heap_t *heap );
  void (*before_promotion)( old_heap_t *heap );
  void (*after_promotion)( old_heap_t *heap );
  void (*promote_from_younger)( old_heap_t *heap );
  void (*stats)( old_heap_t *heap, int generation, heap_stats_t *stats );
  word *(*data_load_area)( old_heap_t *heap, unsigned nbytes );

  /* Private */
  void *data;
};


/***************************************************************************
 *
 * Static heaps.
 */

static_heap_t *
create_static_heap( int heap_no, int gen_no, unsigned size_bytes );

struct static_heap {
  char *id;

  int  (*initialize)( static_heap_t * );
  void (*stats)( static_heap_t *, heap_stats_t * );
  word *(*data_load_area)( static_heap_t *, unsigned );

  void *data;
};

/*************************************************************************
 *
 * Remembered Sets.
 *
 * Remembered sets are full-fledged ADTs with operations encoded as
 * function pointers in the instance.  
 *
 * Remembered sets remember objects.  The basic assumption in the current
 * implementation is that there will be one set for each generation, and 
 * it will remember objects in the generation that may contain pointers 
 * to younger generations.
 */


/* Remembered set statistics
 */

struct remset_stats {
  unsigned ssb_recorded;     /* SSB entries recorded */
  unsigned hash_recorded;    /* Hash table entries recorded */
  unsigned hash_scanned;     /* Hash table entries scanned */
  unsigned words_scanned;    /* Words of objects scanned for pointers */
  unsigned hash_removed;     /* Hash table entries removed */
};


/* 
 * The Remembered Set.
 *
 * Allocate a remembered set by calling create_remset(); the parameters
 * are sizes of the internal data structures in number of entries.  A value
 * of 0 means "use default".
 *
 * The remembered set object has some public fields that are needed
 * by the write barrier; don't mess with these!
 *
 * clear() clears the set of its contents; this is needed after data are
 * promoted out of the generations the remembered set belongs to.
 *
 * compact() compacts the SSB and should be called by the write barrier
 * implementation only (and possibly by GC policy code, I haven't decided).
 * It returns 1 if the remembered set overflowed as a result of the flush.
 *
 * enumerate() takes a scanner function and calls it once with each object
 * pointer remembered by the set; the scanner function can then scan
 * the object looking for pointers into younger generations.  If the scanner
 * returns 1, the remembered pointer is retained; if it returns 0, the
 * pointer may be removed from the set.
 *
 * stats() fills the statistics structure.
 *
 * has_overflowed() returns 1 if the remembered set has filled to overflowing
 * since the last time the set was cleared.
 */

remset_t *
create_remset( unsigned tbl_ent, unsigned pool_ent, unsigned ssb_ent,
	       word **ssb_bot_loc, word **ssb_top_loc, word **ssb_lim_loc );

struct remset {
  void (*clear)( remset_t *remset );
  int  (*compact)( remset_t *remset );
  void (*enumerate)(remset_t *remset, int (*scanner)(word,void*, unsigned*),
		    void*data);
  void (*stats)( remset_t *remset, remset_stats_t *stats );
  int  (*has_overflowed)( remset_t *remset );
  int  (*isremembered)( remset_t *remset, word w );
  void (*assimilate)( remset_t *borg, remset_t *earth );

  /* For the write barrier. */
  word **ssb_bot;
  word **ssb_top;
  word **ssb_lim;

  /* Private */
  void *data;
};


/***************************************************************************
 *
 * Stacks: see Rts/Sys/stack.c.
 */

int  stk_create( word *globals );
void stk_clear( word *globals );
void stk_flush( word *globals, unsigned *frames_flushed, 
	        unsigned *bytes_flushed );
int  stk_restore_frame( word *globals );

#endif /* INCLUDED_GC_INTERFACE_H */

/* eof */
