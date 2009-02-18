/* Copyright 2008 Felix S Klock II        -*- indent-tabs-mode: nil -*-
 *
 * $Id: smircy.c 5790 2008-09-08 17:25:21Z pnkfelix $
 *
 * Incremental snapshot marking machine for the regional collector,
 * using Yuasa-style concurrent SATB.
 * 
 * "smircy" is pronounced "smirky" (though "s'mercy" is forgivable).
 * 
 * Objective: concurrently/incrementally generate a snapshot of the
 * heap at a particular point in time without causing unbounded pauses
 * in the mutator computation.
 * 
 * The marking process will (eventually) run concurrently with the
 * mutator, but *not* with the collector.  Therefore the marking
 * process must allow itself to be interrupted by the collector.
 * 
 * The marking machine maintains a mapping from each region to its
 * portion of the mark stack; i.e. its portion of the frontier (of the
 * developing snapshot of the heap).
 *
 * After the mark bitmap is complete, it is used to refine the
 * remembered sets, so that objects that were dead in the snapshot are
 * no longer remembered.
 * 
 * Open Issues: 
 * 
 * 0. Are the low-level memory allocation primitives thread-safe?
 * 
 *    My memory is that they are not.  I want to get incremental
 *    marking done before I investigate that, though.
 * 
 * 1. Objects allocated after the snapshot has been started need to be
 *    treated as marked.  The markthread/ SVN branch accomplished this
 *    by adding a callback to the low-level block allocation routine;
 *    should I do the same here?
 * 
 *    (I do not immediately know what my alternatives are; but it may
 *    not even suffice to do the above... see (2) below.)
 *
 * 2. Objects migrated and reclaimed by the collector need to have
 *    their state in the bitmap and mark stack propogated accordingly.
 *    How should this responsibility be divided between the SMIRCY
 *    machine and the rest of the collector?
 * 
 *    First draft: have the collector call into the SMIRCY machine on
 *    *every* object forwarding.  (The only immediate alternative I
 *    can see is to perform a sweep-like pass after each collection,
 *    but I am trying to avoid anything that takes time proportional
 *    to the freed storage rather than live storage.)
 * 
 * 3. Should this maintain a mark bitmap for each region, or one
 *    bitmap for the entire heap?
 *
 *    First draft: maintain a bitmap for the entire heap since
 *    msgc-core does and thus is supported by GC infrastructure.
 * 
 *    If each region had a single continguous array of words, with
 *    arbitrary space between regions, then it would seem best to have
 *    a bitmap per region.  However, right now the memory allocation
 *    allows for pages of memory to be mixed between regions; the
 *    address space of regions can overlap, which means that having a
 *    bitmap per region could end up using asymptotically more space
 *    than a single bitmap for the whole heap.
 *
 *    (When we move to a 64-bit architecture, we may not have the
 *    option of continuing to use a bitmap for the whole heap.)
 *
 * 4. Should this be used to address the popular object problem?
 *
 *    It could do so by gathering information about how large the
 *    summary for a popular region would be (and then, after proving
 *    that the region is no longer popular, reconstructing the
 *    remembered set infromation via the snapshot marking traversal).
 * 
 * 5. In msgc-core, Lars chose to mark objects when *popping* them
 *    from the stack.  Thus if we encounter the same object multiple
 *    times during heap traversal, it will be pushed onto the stack
 *    multiple times (and the decision to not traverse it will be
 *    made *after* popping it).  This design decision matters and
 *    is addressed on page 78 of Jones & Lins.  Lars' code traverses
 *    *arcs*, while the alternative (checking the mark bit before
 *    pushing) traverses *nodes*; the former tends to produce deeper
 *    mark stacks than the latter.
 *    The choice may not have mattered much to Lars, since I do not
 *    believe the msgc-core code was a crucial component in his
 *    algorithms.  But this marker is a crucial component to RROF, so
 *    I need to weigh my options carefully.
 */

#define GC_INTERNAL

#define dbmsg( format, args... ) if (0) consolemsg( format, ## args )

#define bounded_decrement( x, y ) if (x > 0) x = max( 0, x - (y) );

#include "larceny.h"
#include "gc_t.h"
#include "gclib.h"
#include "smircy.h"

#include "smircy_internal.h"

/* Important ideas to copy from msgc-core.c:
 * - scan large objects incrementally, by keeping a separate stack of 
 *   cursors into the middle of large objects.
 */

/* All objects are double word aligned */
#define MIN_BYTES_PER_OBJECT (2*sizeof(word))

#define BITS_PER_WORD (8*sizeof(word))

/* See pg. 78 of Jones and Lins.
 * (Felix will probably commit to MARK_ON_PUSH soon.) */
#define MARK_ON_PUSH 1
#define MARK_ON_POP (! MARK_ON_PUSH)

/* Calculates ceil(x/y); (unlike quotient, which does floor). */
#define CEILDIV(x,y) (roundup((x),(y))/(y))

#if 0
#define CHECK_REP(context)                      \
  do { smircy_check_rep(context); } while (0)
#else 
#define CHECK_REP(context)                      \
  do {                            } while (0)
#endif

void smircy_check_rep( smircy_context_t *context );

static void print_stack( smircy_context_t *context )
{
  obj_stack_t *obj_stack;
  los_stack_t *los_stack;
  obj_stackseg_t *seg;
  obj_stack_entry_t *stkp;
  obj_stack_entry_t *stkbot;
  word w;
  {
    obj_stack = &context->stack.obj;
    los_stack = &context->stack.los;
    seg = obj_stack->seg;
    stkp = obj_stack->stkp;
    stkbot = obj_stack->stkbot;
    consolemsg(    "stack[    ]:    [0x%08x 0x%08x]", stkbot, stkp);
    while (stkp > stkbot) {
      stkp--;
      w = stkp->val;
      consolemsg(  "     0x%08x", w);
      if (stkp == stkbot && seg->next != NULL) {
        seg = seg->next;
        stkp = seg->data+OBJ_STACK_SIZE;
        stkbot = seg->data;
        consolemsg("     ---------- [0x%08x 0x%08x]", stkbot, stkp);
      }
    }
  }
}

static void print_stack_sizes( smircy_context_t *context ) 
{
  obj_stack_t *obj_stack;
  los_stack_t *los_stack;
  obj_stackseg_t *objseg;
  los_stackseg_t *losseg;
  word w;
  int full_objseg_count, full_losseg_count;
  {
    obj_stack = &context->stack.obj;
    full_objseg_count = 0;
    objseg = obj_stack->seg;
    if (objseg != NULL) {
      while (objseg->next != NULL) {
        full_objseg_count += 1;
        objseg = objseg->next;
      }
    }

    los_stack = &context->stack.los;
    full_losseg_count = 0;
    losseg = los_stack->seg;
    if (losseg != NULL) {
      while (losseg->next != NULL) {
        full_losseg_count += 1;
        losseg = losseg->next;
      }
    }
    if (full_objseg_count == 0 && full_losseg_count == 0) {
      consolemsg( "OBJSTK[    ]:             % 5d " 
                  "LOSSTK[    ]:             % 5d ",
                  obj_stack->stkp - obj_stack->stkbot,
                  los_stack->stkp - los_stack->stkbot);
    } else if (full_objseg_count == 0) {
      consolemsg( "OBJSTK[    ]:             % 5d " 
                  "LOSSTK[    ]: % 4d*%d + % 5d ",
                  obj_stack->stkp - obj_stack->stkbot,
                  full_losseg_count, LOS_STACK_SIZE, los_stack->stkp - los_stack->stkbot);
    } else if (full_losseg_count == 0) {
      consolemsg( "OBJSTK[    ]: % 4d*%d + % 5d " 
                  "LOSSTK[    ]:             % 5d ",
                  full_objseg_count, OBJ_STACK_SIZE, obj_stack->stkp - obj_stack->stkbot,
                  los_stack->stkp - los_stack->stkbot);
    } else {
      consolemsg( "OBJSTK[    ]: % 4d*%d + % 5d " 
                  "LOSSTK[    ]: % 4d*%d + % 5d ",
                  full_objseg_count, OBJ_STACK_SIZE, obj_stack->stkp - obj_stack->stkbot,
                  full_losseg_count, LOS_STACK_SIZE, los_stack->stkp - los_stack->stkbot);
    }
  }
}

int smircy_stack_size( smircy_context_t *context ) 
{
  obj_stack_t *obj_stack;
  obj_stackseg_t *objseg;
  int objcount_first;
  int objcount_rest;
  int full_objseg_count;

  obj_stack = &context->stack.obj;
  full_objseg_count = 0;
  objseg = obj_stack->seg;
  if (objseg != NULL) {
    while (objseg->next != NULL) {
      full_objseg_count += 1;
      objseg = objseg->next;
    }
  }
  objcount_first = obj_stack->stkp - obj_stack->stkbot;
  objcount_rest = OBJ_STACK_SIZE*full_objseg_count;

  return objcount_first + objcount_rest;
}

int smircy_stack_count( smircy_context_t *context ) 
{
  int objcount;
  obj_stack_t *obj_stack;
  obj_stackseg_t *objseg;
  obj_stack_entry_t *stkp, *stkbot;
  int objcount_first;
  int objcount_rest;
  int full_objseg_count;

  objcount = 0;
  obj_stack = &context->stack.obj;
  full_objseg_count = 0;
  objseg = obj_stack->seg;
  stkp = obj_stack->stkp;
  stkbot = obj_stack->stkbot;
  stkp--;
  while (objseg != NULL) {
    while (stkp >= stkbot) {
      if (stkp->val != 0x0)
        objcount++;
      stkp--;
    }
    objseg = objseg->next;
    if (objseg != NULL) {
      stkbot = objseg->data;
      stkp = objseg->data+OBJ_STACK_SIZE;
    }
  }

  return objcount;
}

static void *my_gclib_alloc_rts( int bytes, unsigned attribute, char *where )
{
#if 0
  consolemsg( "gclib_alloc_rts from: %22s bytes=%8d {frag: %4d approx 0.%02d}",
              where, bytes, 
              (roundup_page(bytes)-bytes), 
              (roundup_page(bytes)-bytes)*100 / max(PAGESIZE,bytes) );
#endif
  return gclib_alloc_rts( bytes, attribute );
}

static obj_stack_entry_t **alloc_obj_stk_entries( int n ) 
{
  return my_gclib_alloc_rts( sizeof(obj_stack_entry_t)*n, 0, 
                             "alloc_obj_stk_entries" );
}
static void free_obj_stk_entries( obj_stack_entry_t **entries, int n )
{
  gclib_free( entries, n * sizeof(obj_stack_entry_t) );
}

static large_object_cursor_t **alloc_los_stk_entries( int n )
{
  return my_gclib_alloc_rts( sizeof(large_object_cursor_t)*n, 0, 
                             "alloc_los_stk_entries" );
}
static void free_los_stk_entries( large_object_cursor_t **entries, int n ) 
{
  gclib_free( entries, n );
}
static obj_stackseg_t *alloc_obj_stackseg() {
  return my_gclib_alloc_rts( sizeof( obj_stackseg_t ), 0, "alloc_obj_stackseg" );
}
static void free_obj_stackseg( obj_stackseg_t *obj ) 
{
  gclib_free( obj, sizeof( obj_stackseg_t ) );
}
static los_stackseg_t *alloc_los_stackseg() {
  return my_gclib_alloc_rts( sizeof( los_stackseg_t ), 0, "alloc_los_stackseg" );
}
static void free_los_stackseg( los_stackseg_t *los ) 
{
  gclib_free( los, sizeof( los_stackseg_t ) );
}
static word* alloc_bitmap( int words_in_bitmap )
{
  return my_gclib_alloc_rts( words_in_bitmap * sizeof(word), 0, "alloc_bitmap" );
}
static void free_bitmap( word *bitmap, int words_in_bitmap )
{
  gclib_free( bitmap, words_in_bitmap * sizeof(word) );
}

static void init_from_old( word *bitmap_old, word *lo_addr_old, word *hi_addr_old, int words_in_old, 
                           word *bitmap_new, word *lo_addr_new, word *hi_addr_new, int words_in_new );
static int allocate_bitmap( smircy_context_t *context );

/* expands the mark bitmap in context to cover current address space.
 * copies all old bitmap state into new bitmap, and sets remaining
 * bits in new bitmap to 1 (ie marked).  Returns size difference. */
static int expand_bitmap_core( smircy_context_t *context, 
                               word *lowest_heap_address_new, 
                               word *highest_heap_address_new ) 
{
  word *lowest_heap_address_old;
  word *highest_heap_address_old;
  int words_in_bitmap_old, words_in_bitmap_new;
  word *bitmap_new, *bitmap_old;

  int offset_bit_idx;

  lowest_heap_address_old = context->lowest_heap_address;
  highest_heap_address_old = context->highest_heap_address;
  bitmap_old = context->bitmap;
  words_in_bitmap_old = context->words_in_bitmap;

  dbmsg( "expand_bitmap_core( context, [0x%08x,0x%08x] ) from [0x%08x,0x%08x]",
         lowest_heap_address_new, highest_heap_address_new,
         lowest_heap_address_old, highest_heap_address_old );

  words_in_bitmap_new = allocate_bitmap( context ); /* (mutates context) */
  bitmap_new = context->bitmap;
  highest_heap_address_new = max( highest_heap_address_new, context->highest_heap_address );
  lowest_heap_address_new = min( lowest_heap_address_new, context->lowest_heap_address );

  init_from_old( bitmap_old, lowest_heap_address_old, highest_heap_address_old, words_in_bitmap_old, 
                 bitmap_new, lowest_heap_address_new, highest_heap_address_new, words_in_bitmap_new );

  free_bitmap( bitmap_old, words_in_bitmap_old );
  return (words_in_bitmap_new - words_in_bitmap_old);
}

static void *smircy_enumerate_whole_stack3( smircy_context_t *context,
                                            void *visit( word *pw, int *pgno, void *data ),
                                            void *orig_data );

static void *increment_gnos_geq( word *pw, int *pgno, void *data ) 
{
  int shift_geq_gno = *(int*)data;
  word w = *pw;
  if (*pgno >= shift_geq_gno) {
    dbmsg("increment_gnos_geq: 0x%08x (%d) incr %d", w, gen_of(w), *pgno );
    *pgno = *pgno+1;
  }
  return data;
}

/* expands the mark context to cover current regions and address space.
 * returns size difference of mark bitmap.
 */
static void expand_context_plus_rgn( smircy_context_t *context, int shift_geq_gno ) 
{
  int num_rgns, num_rgns_old;
  obj_stack_entry_t     **rgn_to_obj_entry_old;
  obj_stack_entry_t     **rgn_to_obj_entry_new;
  large_object_cursor_t **rgn_to_los_entry_old;
  large_object_cursor_t **rgn_to_los_entry_new;
  num_rgns = context->gc->remset_count;
  num_rgns_old = context->num_rgns;

  dbmsg( "expand_context_plus_rgn( context, %d ) "
         "num_rgns_old: %d num_rgns_new: %d ",
         shift_geq_gno, num_rgns_old, num_rgns );

  rgn_to_obj_entry_old = context->rgn_to_obj_entry;
  rgn_to_los_entry_old = context->rgn_to_los_entry;
  rgn_to_obj_entry_new = alloc_obj_stk_entries( num_rgns+1 );
  rgn_to_los_entry_new = alloc_los_stk_entries( num_rgns+1 );

  /* XXX FIXME redundant zero-init's followed by copy-init's */
  memset( rgn_to_obj_entry_new, 0, 
          sizeof(obj_stack_entry_t*)*(num_rgns+1) );
  memset( rgn_to_los_entry_new, 0, 
          sizeof(large_object_cursor_t*)*(num_rgns+1) );

  assert( shift_geq_gno > num_rgns || rgn_to_obj_entry_new[ shift_geq_gno ] == NULL ); 
  assert( shift_geq_gno+1 > num_rgns || rgn_to_obj_entry_new[ shift_geq_gno+1 ] == NULL );
  {                                           /* copy unshifted state */
    int len = min(shift_geq_gno,(num_rgns_old+1));
    assert( len < num_rgns );
    dbmsg( "expand_context_plus_rgn unshifted [0,%d)", len );
    assert( rgn_to_obj_entry_new[len] == NULL );
    memcpy( &rgn_to_obj_entry_new[0], &rgn_to_obj_entry_old[0], 
            sizeof(obj_stack_entry_t*)*len );
    assert( rgn_to_obj_entry_new[len] == NULL );
    memcpy( &rgn_to_los_entry_new[0], &rgn_to_los_entry_old[0], 
            sizeof(large_object_cursor_t*)*len );
  }
  assert( shift_geq_gno > num_rgns || rgn_to_obj_entry_new[ shift_geq_gno ] == NULL );
  assert( shift_geq_gno+1 > num_rgns || rgn_to_obj_entry_new[ shift_geq_gno+1 ] == NULL );

  if ( (num_rgns_old+1 - shift_geq_gno) > 0 ) { /* copy shifted state */
    int len = num_rgns_old+1 - shift_geq_gno;
    dbmsg( "expand_context_plus_rgn 1-shifted [%d,%d)", shift_geq_gno, shift_geq_gno+len);
    memcpy( &rgn_to_obj_entry_new[shift_geq_gno+1], &rgn_to_obj_entry_old[shift_geq_gno], 
            sizeof(obj_stack_entry_t*)*len );
    memcpy( &rgn_to_los_entry_new[shift_geq_gno+1], &rgn_to_los_entry_old[shift_geq_gno], 
            sizeof(large_object_cursor_t*)*len );
  }

  /* update gno's in the stack XXX won't the gno's go away eventually? */
  {
    smircy_enumerate_whole_stack3( context, 
                                   increment_gnos_geq, 
                                   &shift_geq_gno );
  }

  assert( shift_geq_gno > num_rgns || 
          rgn_to_obj_entry_new[ shift_geq_gno ] == NULL ||
          (rgn_to_obj_entry_new[ shift_geq_gno ]->gno == shift_geq_gno) );
  assert( shift_geq_gno+1 > num_rgns ||
          rgn_to_obj_entry_new[ shift_geq_gno+1 ] == NULL ||
          (rgn_to_obj_entry_new[ shift_geq_gno+1 ]->gno == shift_geq_gno+1 ));

  free_obj_stk_entries( rgn_to_obj_entry_old, (num_rgns_old+1) );
  free_los_stk_entries( rgn_to_los_entry_old, (num_rgns_old+1) );

  context->num_rgns = num_rgns;
  context->rgn_to_obj_entry = rgn_to_obj_entry_new;
  context->rgn_to_los_entry = rgn_to_los_entry_new;
}

static void expand_context( smircy_context_t *context ) 
{
  /* pass gno greater than any region's gno, so that all internal
   * mapping remains the same. */
  expand_context_plus_rgn( context, context->num_rgns+1 );
}

/* allocates new bitmap that covers current address range.
 * does *not* initialize the bitmap.
 */
static word *allocate_new_bitmap( char **lowest_recv, char **highest_recv,
                                  int *words_in_bitmap_recv ) {
  char *lowest, *highest;
  int max_obj_count;
  int words_in_bitmap; 
  word *bitmap;

  gclib_memory_range( &lowest, &highest );
  /* Upper bound on number of objects that fit in memory range. */
  max_obj_count = CEILDIV(highest-lowest,MIN_BYTES_PER_OBJECT);
  words_in_bitmap = CEILDIV(max_obj_count,BITS_PER_WORD);
  bitmap = alloc_bitmap( words_in_bitmap );

  *lowest_recv = lowest;
  *highest_recv = highest;
  *words_in_bitmap_recv = words_in_bitmap;
  return bitmap;
}

/* Initializes bitmap_new, copying all bits from old to new (assuming
 * that old[0] corresponds to new[offset]); any indices outside of 
 * old's range are initialized to 1.
 * Returns (new_size - old_size)
 */
static void init_from_old( word *bitmap_old, word *lo_addr_old, word *hi_addr_old, int words_in_old, 
                           word *bitmap_new, word *lo_addr_new, word *hi_addr_new, int words_in_new ) {
  int i;
  int offset_bit_idx = ((char*)lo_addr_old - (char*)lo_addr_new) >> BIT_IDX_SHIFT;
  int offset_word_idx = offset_bit_idx >> BIT_IDX_TO_WORD_IDX;
  word testobj;
  assert( words_in_old > 0 );
  assert( offset_word_idx+words_in_old <= words_in_new );
  for (i = 0; i < offset_word_idx; i++) {
    bitmap_new[i] = (~0);
  }
  for (i = 0; i < words_in_old; i++) {
    bitmap_new[i+offset_word_idx] = bitmap_old[i];
    testobj = ((word)(&(lo_addr_old[2*i]))) | PAIR_TAG;
    assert( isptr(testobj) && lo_addr_old <= ptrof(testobj) && ptrof(testobj) < hi_addr_old );
  }
  for (i = words_in_old+offset_word_idx; i < words_in_new; i++) {
    bitmap_new[i] = (~0);
  }
}

static int allocate_bitmap( smircy_context_t *context ) {
  char *lowest, *highest;
  int words_in_bitmap;
  word *bitmap;
  bitmap = allocate_new_bitmap( &lowest, &highest, &words_in_bitmap );
  context->bitmap = bitmap;
  context->lowest_heap_address = (word*)lowest;
  context->highest_heap_address = (word*)highest;
  context->words_in_bitmap = words_in_bitmap;
  return words_in_bitmap;
}

/* (setting this to 0 might catch problems when debugging.) */
#define ATTEMPT_SEGMENT_REUSE 1

/* Pushes new segment on top of obj, and returns the new seg.
 * 
 * If *freed is non-null, the new segment may be drawn from *freed, in
 * which case freed is updated to point to its successor segment.
 */
static obj_stackseg_t *push_obj_segment( obj_stackseg_t *obj, 
                                         obj_stackseg_t **freed,
                                         int gno_owner)
{
  obj_stackseg_t *sp;

#if !ATTEMPT_SEGMENT_REUSE
  assert(*freed == NULL);
#endif

  if (*freed == NULL) {
    sp = alloc_obj_stackseg();

    dbmsg( "SMIRCY push_obj_segment( 0x%08x, [0x%08x] ) => 0x%08x [%d]", 
           obj, *freed, sp, gno_owner );

    sp->next = obj;
    assert( sp != NULL );
    return sp;
  } else {
    sp = *freed;

    dbmsg( "SMIRCY push_obj_segment( 0x%08x, [0x%08x] ) => 0x%08x [%d]", 
           obj, *freed, sp, gno_owner );

    *freed = sp->next;
    sp->next = obj;

    assert( sp != NULL );
    return sp;
  }
}

/* Pops obj, returning its successor in the stack.  The popped segment
 * is either immediately deallocated or put on top of *freed.
 * (The client is obligated to eventually deallocate all elements of
 * freed, but there is no guarantee that the popped segment is
 * actually put on freed.) */
static obj_stackseg_t *pop_obj_segment( obj_stackseg_t *obj, 
                                        obj_stackseg_t **freed ) 
{
  obj_stackseg_t *sp;

  assert( obj != NULL );
  sp = obj->next;

  dbmsg( "SMIRCY  pop_obj_segment( 0x%08x, [0x%08x] ) => 0x%08x", 
         obj, *freed, sp );

#if ATTEMPT_SEGMENT_REUSE
  obj->next = *freed;
  *freed = obj;
#else
  free_obj_stackseg( obj );
#endif
  return sp;
}

/* Pushes new segment on top of los, and returns the new seg.
 * 
 * If *freed is non-null, the new segment may be drawn from *freed, in
 * which case freed is updated to point to its successor segment.
 */
static los_stackseg_t *push_los_segment( los_stackseg_t *los,
                                         los_stackseg_t **freed ) 
{
  los_stackseg_t *sp;

#if !ATTEMPT_SEGMENT_REUSE
  assert( *freed == NULL );
#endif

  if (*freed == NULL) {
    sp = alloc_los_stackseg();

    dbmsg( "SMIRCY push_los_segment( 0x%08x, [0x%08x] ) => 0x%08x", 
           los, *freed, sp );

    sp->next = los;
    return sp;
  } else {
    sp = *freed;

    dbmsg( "SMIRCY push_los_segment( 0x%08x, [0x%08x] ) => 0x%08x", 
           los, *freed, sp );

    *freed = sp->next;
    return sp;
  }
}

/* Pops los, returning its successor in the stack.  The popped segment
 * is either immediately deallocated or put on top of *freed.
 * (Analogous to pop_obj_segment above.) */
static los_stackseg_t *pop_los_segment( los_stackseg_t *los,
                                        los_stackseg_t **freed ) 
{
  los_stackseg_t *sp;

  assert( los != NULL );
  sp = los->next;

  dbmsg( "SMIRCY  pop_los_segment( 0x%08x, [0x%08x] ) => 0x%08x", 
         los, *freed, sp );

#if ATTEMPT_SEGMENT_REUSE
  los->next = *freed;
  *freed = los;
#else
  free_los_stackseg( los );
#endif
  return sp;
}

static int free_obj_stacksegs( obj_stackseg_t *segs ) 
{
  int i = 0;
  if (segs != NULL) {
    /* (should be sufficiently shallow) */
    i = 1 + free_obj_stacksegs( segs->next );
    free_obj_stackseg( segs );
  }
  return i;
}

static int free_los_stacksegs( los_stackseg_t *segs ) 
{
  int i = 0;
  if (segs != NULL) {
    i = 1 + free_los_stacksegs( segs->next );
    free_los_stackseg( segs );
  }
  return i;
}

static bool mark_object( smircy_context_t *context, word obj );

static void push( smircy_context_t *context, word obj, word src ) 
{
  int gno;
  obj_stack_t *stack;
  bool already_marked;

  if (isptr(obj)) {

#if MARK_ON_PUSH
    already_marked = mark_object( context, obj );
    if (already_marked) return;
#endif

    gno = gen_of(obj);
    assert( gno != 0 ); /* we do not push objects in the nursery */
    assert( gno >= 0 );  /* we should only encounter objects with valid gnos */
    stack = &(context->stack.obj);
    if (stack->stkp == stack->stklim) {
      stack->seg = push_obj_segment( stack->seg, &context->freed_obj, gno );
      stack->stkbot = stack->seg->data;
      stack->stklim = stack->seg->data+OBJ_STACK_SIZE;
      stack->stkp = stack->stkbot;
    }

    stack->stkp->val = obj;
    stack->stkp->gno = gno;
    stack->stkp->next_in_rgn = context->rgn_to_obj_entry[gno];
    context->rgn_to_obj_entry[gno] = stack->stkp;
    stack->stkp++;
  }
}

static void los_push( smircy_context_t *context, word index, word obj ) 
{
  int gno;
  los_stack_t *stack;
  assert(isptr(obj));
  gno = gen_of(obj);
  stack = &(context->stack.los);
  if (stack->stkp == stack->stklim) {
    stack->seg = push_los_segment( stack->seg, &context->freed_los );
    stack->stkbot = stack->seg->data;
    stack->stklim = stack->seg->data+LOS_STACK_SIZE;
    stack->stkp = stack->stkbot;
  }
  stack->stkp->object = obj;
  stack->stkp->index = index;
  stack->stkp->next_in_rgn = context->rgn_to_los_entry[gno];
  context->rgn_to_los_entry[gno] = stack->stkp;
  stack->stkp++;
}

/* Attempts to enqueue entries on object stack for gno by dequeuing them
 * from the los cursor stack for gno. */
static bool fill_from_los_stack( smircy_context_t *context )
{
  obj_stack_t *obj_stack;
  los_stack_t *los_stack;
  word obj;
  int i, window_start, window_size, window_lim, objwords;

  dbmsg("fill_from_los_stack( context )" );

  obj_stack = &context->stack.obj;
  los_stack = &context->stack.los;
  if (los_stack->seg == NULL) {
    return FALSE;
  } 
  assert( los_stack->stkp >= los_stack->stkbot );
  if (los_stack->stkp == los_stack->stkbot) {
    los_stack->seg = pop_los_segment( los_stack->seg, &context->freed_los );
    if (los_stack->seg == NULL) {
      los_stack->stkp = 0;
      los_stack->stkbot = 0;
      los_stack->stklim = 0;
      return FALSE;
    } else {
      los_stack->stkbot = los_stack->seg->data;
      los_stack->stklim = los_stack->seg->data+LOS_STACK_SIZE;
      los_stack->stkp = los_stack->stklim;
    }
  }
  /* at this point, los_stack->{stkbot,stklim,stkp} are established
   * and stkp > stkbot */
  los_stack->stkp--;
  if (los_stack->stkp->object == 0x0) {
    /* dead entry; move on. */
    return fill_from_los_stack( context );
  }
  obj = los_stack->stkp->object;
  assert2( context->rgn_to_los_entry[ gen_of( obj ) ] == los_stack->stkp );
  context->rgn_to_los_entry[ gen_of( obj ) ] = los_stack->stkp->next_in_rgn;
  window_start = los_stack->stkp->index;
  objwords = bytes2words( sizefield( *ptrof(obj) ));
  window_size = min( objwords-window_start, WINDOW_SIZE_LIMIT );
  window_lim = window_start+window_size;
  for( i = window_start; i < window_lim; i++ )
    push( context, vector_ref( obj, i ), obj );
  if (window_lim < objwords)
    los_push( context, window_lim, obj );
  return TRUE;
}

smircy_context_t *smircy_begin( gc_t *gc, int num_rgns ) 
{
  smircy_context_t *context;
  int words_in_bitmap;

  context = must_malloc( sizeof( smircy_context_t ) );
  context->gc = gc;
  context->num_rgns = num_rgns;

  words_in_bitmap = allocate_bitmap( context );
  memset( context->bitmap, 0, words_in_bitmap*sizeof(word) );

  context->stack.obj.seg = NULL;
  context->stack.obj.stkp = NULL;
  context->stack.obj.stkbot = NULL;
  context->stack.obj.stklim = NULL;
  context->stack.los.seg = NULL;
  context->stack.los.stkp = NULL;
  context->stack.los.stkbot = NULL;
  context->stack.los.stklim = NULL;

  context->rgn_to_obj_entry = alloc_obj_stk_entries( num_rgns+1 );
  memset( context->rgn_to_obj_entry, 0, sizeof(obj_stack_entry_t*)*(num_rgns+1) );
  context->rgn_to_los_entry = alloc_los_stk_entries( num_rgns+1 );
  memset( context->rgn_to_los_entry, 0, sizeof(large_object_cursor_t*)*(num_rgns+1) );

  context->freed_obj = NULL;
  context->freed_los = NULL;

  context->total_traced = 0;
  context->total_marked = 0;
  context->total_words_marked = 0;

  dbmsg( "smircy_begin( gc, %d ) bitmap: 0x%08x ", 
         num_rgns, context->bitmap );

  CHECK_REP( context );

  return context;
}

static void push_root( word *loc, void *data ) 
{
  smircy_context_t *context = (smircy_context_t*) data;
  if (isptr(*loc)) {
    dbmsg("smircy push_root( [0x%08x (%d)], context )", *loc, gen_of(*loc));
  }
  push( context, *loc, 0x0 );
}

void smircy_push_roots( smircy_context_t *context )
{
  CHECK_REP( context );

  gc_enumerate_roots( context->gc, push_root, (void*)context );

  CHECK_REP( context );
}

static bool push_remset_entry( word obj, void *data, unsigned *stats ) 
{
  smircy_context_t *context = (smircy_context_t*)data;
  if (isptr(obj)) {
    dbmsg("smircy push_remset_entry( 0x%08x (%d), context, stats )", obj, gen_of(obj));
  }
  push( context, obj, 0x0 );
  return TRUE;
}

void smircy_push_remset( smircy_context_t *context, remset_t *rs ) 
{
  CHECK_REP( context );

  rs_enumerate( rs, push_remset_entry, context );

  CHECK_REP( context );
}

/* Marks obj in bitmap.  Returns true iff obj already marked in bmp. */
static bool mark_word( word *bitmap, word obj, word first )
{
  bool retval;
  word bit_idx, word_idx, bit_in_word;

  /* Note: marks object "header" only, not entire range it occupies. */
  bit_idx     = (obj - first) >> BIT_IDX_SHIFT;
  word_idx    = bit_idx >> BIT_IDX_TO_WORD_IDX;
  bit_in_word = 1 << (bit_idx & BIT_IN_WORD_MASK);
  retval      = (bool)( bitmap[ word_idx ] & bit_in_word );
  bitmap[ word_idx ] |= bit_in_word;
  return retval;
}
/* Unmarks obj in bitmap.  Returns true iff obj previously marked in bmp. */
static bool unmark_word( word *bitmap, word obj, word first )
{
  bool retval;
  word bit_idx, word_idx, bit_in_word, set_in_word;

  /* Note: unmarks object "header" only, not entire range it occupies. */
  bit_idx     = (obj - first) >> BIT_IDX_SHIFT;
  word_idx    = bit_idx >> BIT_IDX_TO_WORD_IDX;
  set_in_word = 1 << (bit_idx & BIT_IN_WORD_MASK);
  bit_in_word = ~set_in_word;
  retval      = (bool)( bitmap[ word_idx ] & set_in_word );
  bitmap[ word_idx ] &= bit_in_word;
  return retval;
}
static bool is_address_in_bitmap( smircy_context_t *context, word obj ) {
  return ( context->lowest_heap_address <= ptrof( obj ) &&
           ptrof( obj ) < context->highest_heap_address );
}

static bool mark_object( smircy_context_t *context, word obj )
{
  bool rtn;
  dbmsg( "smircy mark_object( context, 0x%08x (%d) )", obj, gen_of(obj) );
  if ( context->lowest_heap_address <= ptrof( obj ) &&
       ptrof( obj ) < context->highest_heap_address ) {
    rtn = mark_word( context->bitmap, obj, (word)context->lowest_heap_address );
  } else {
    rtn = TRUE;
  }
  assert2( smircy_object_marked_p( context, obj ));
  return rtn;
}

static bool unmark_object( smircy_context_t *context, word obj ) {
  bool rtn;
  dbmsg( "smircy unmark_object( context, 0x%08x (%d) )", obj, gen_of(obj) );
  assert( context->lowest_heap_address <= ptrof( obj ) &&
          ptrof( obj ) < context->highest_heap_address );
  rtn = unmark_word( context->bitmap, obj, (word)context->lowest_heap_address );
  assert2( ! smircy_object_marked_p( context, obj ));
  return rtn;
}

static int push_constituents( smircy_context_t *context, word w )
{
  int i, n;

  switch( tagof( w )) {
  case PAIR_TAG: 
    push( context, pair_cdr( w ), w ); /* Do the CDR last */
    push( context, pair_car( w ), w ); /* Do the CAR first */
    return 2;
  case VEC_TAG:
    assert2(  (tagof(w) == VEC_TAG) == (header( *ptrof(w) ) == VEC_HDR) );
  case PROC_TAG:
    n = bytes2words( sizefield(*ptrof(w)) );
    if (n > WINDOW_SIZE_LIMIT) {
      los_push( context, 0, w );
    } else {
      for( i=0; i < n; i++ ) {
        push( context, vector_ref( w, i ), w );
      }
    }
    return n+1;
  default: 
    return 0;
  }
}

static void reestablish_rgn_to_obj_entry_post_pop( smircy_context_t *context,
                                                   obj_stack_entry_t *stkp )
{ 
  int rgn;
  for( rgn = 1; rgn <= context->num_rgns; rgn++ ) {
    if (context->rgn_to_obj_entry[rgn] == stkp) {
      context->rgn_to_obj_entry[rgn] = stkp->next_in_rgn;
    }
  }
}

void smircy_progress( smircy_context_t *context, 
                      int mark_max, int trace_max, int mark_words_max,
                      int *marked_recv, int *traced_recv, 
                      int *words_marked_recv )
{
  word w;
  int w_gno;
  smircy_stack_t *stack;
  int mark_budget, trace_budget, mark_words_budget;
  int traced = 0, marked = 0, words_marked = 0, constituents;
  bool already_marked;

  CHECK_REP( context );

  mark_budget = mark_max;
  trace_budget = trace_max;
  mark_words_budget = mark_words_max;

  stack = &context->stack;
  dbmsg("smircy process stack, "
        "stk{bot,p,lim}:{0x%08x,0x%08x,0x%08x}", 
        stack->obj.stkbot, stack->obj.stkp, stack->obj.stklim );
  {
    {
      while(1) {
        /* pop */
        if (stack->obj.stkp == stack->obj.stkbot) { /* underflow */
          if (stack->obj.seg == NULL) {
            if (fill_from_los_stack( context )) 
              continue; /* restart processing of stacks[rgn] */
            else
              break; /* done with stacks[rgn] for now */
          } else {
            stack->obj.seg = pop_obj_segment( stack->obj.seg, 
                                              &context->freed_obj );
            if (stack->obj.seg != NULL) {
              stack->obj.stkbot = stack->obj.seg->data;
              stack->obj.stklim = stack->obj.seg->data+OBJ_STACK_SIZE;
              stack->obj.stkp   = stack->obj.stklim;
            } else {
              stack->obj.stkbot = 0x0;
              stack->obj.stklim = 0x0;
              stack->obj.stkp   = 0x0;

              if (fill_from_los_stack( context ))
                continue; /* restart processing of stacks[rgn] */
              else
                break; /* done with stacks[rgn] for now */
            }
          }
        }

        assert( stack->obj.stkp > stack->obj.stkbot );

        stack->obj.stkp--;
        w = stack->obj.stkp->val;

        if (w == 0x0) { // dead entry
          continue;
        }

        assert2( isptr( w ));
        w_gno = gen_of(w);
        assert( w_gno == stack->obj.stkp->gno );
        assert( w_gno > 0 ); /* gno valid and non-nursery */
        if (context->rgn_to_obj_entry[ w_gno ] != stack->obj.stkp ) {
          consolemsg("w: 0x%08x (%d) "
                     "context->rgn_to_obj_entry[ w_gno ]: 0x%08x "
                     "*stack->obj.stkp: [0x%08x [%d],0x%08x]",
                     w, w_gno, 
                     context->rgn_to_obj_entry[ w_gno ], 
                     stack->obj.stkp->val,
                     stack->obj.stkp->gno, 
                     stack->obj.stkp->next_in_rgn );
        }
        assert( context->rgn_to_obj_entry[ w_gno ] == stack->obj.stkp );
        context->rgn_to_obj_entry[ w_gno ] = stack->obj.stkp->next_in_rgn;

        dbmsg( "SMIRCY popped 0x%08x off of stack", w );

        traced++;
        context->total_traced++;
        bounded_decrement( trace_budget, 1 );

#if MARK_ON_POP
        already_marked = mark_object( context, w );
        if (already_marked) continue;
#endif
        assert2( smircy_object_marked_p( context, w ));

        marked++;
        context->total_marked++;
        bounded_decrement( mark_budget, 1 );

        constituents = push_constituents( context, w );
        words_marked += constituents;
        context->total_words_marked += constituents;
        bounded_decrement( mark_words_budget, constituents );

        if (mark_budget == 0 || trace_budget == 0 || mark_words_budget == 0)
          goto budget_exhausted;
      }
    }
  }

  budget_exhausted:
  done:
  *marked_recv = marked;
  *traced_recv = traced;
  *words_marked_recv = words_marked;

  CHECK_REP( context );
}

#define FORWARD_HDR 0xFFFFFFFE /* XXX eek!  Factor from cheney.h elsewhere! */

void smircy_expand_gnos( smircy_context_t *context, int gno ) 
{
  // note that at this point in the control flow, the stack invariants
  // do not necessarily hold.  The point of this function is to try
  // to reestablish them, but the caller may have further fixup's to do
  // before our global invariants are established.
  dbmsg( "  smircy_expand_gnos( context, %d )", gno );
  expand_context_plus_rgn( context, gno );
}

void smircy_swap_gnos( smircy_context_t *context, int gno1, int gno2 ) 
{
  obj_stack_entry_t *obj_stack_entry1, *obj_stack_entry2;
  large_object_cursor_t *los_stack_entry1, *los_stack_entry2;

  assert( gno1 <= context->num_rgns );
  assert( gno2 <= context->num_rgns );

  obj_stack_entry2 = context->rgn_to_obj_entry[gno1];
  obj_stack_entry1 = context->rgn_to_obj_entry[gno2];
  context->rgn_to_obj_entry[gno1] = obj_stack_entry1;
  context->rgn_to_obj_entry[gno2] = obj_stack_entry2;

  los_stack_entry2 = context->rgn_to_los_entry[gno1];
  los_stack_entry1 = context->rgn_to_los_entry[gno2];
  context->rgn_to_los_entry[gno1] = los_stack_entry1;
  context->rgn_to_los_entry[gno2] = los_stack_entry2;

  while (obj_stack_entry1 != NULL) {
    obj_stack_entry1->gno = gno1;
    obj_stack_entry1 = obj_stack_entry1->next_in_rgn;
  }
  while (obj_stack_entry2 != NULL) {
    obj_stack_entry2->gno = gno2;
    obj_stack_entry2 = obj_stack_entry2->next_in_rgn;
  }
}

bool smircy_stack_empty_p( smircy_context_t *context ) 
{
  obj_stack_t *obj_stk;
  los_stack_t *los_stk;
  {
    obj_stk = &context->stack.obj;
    if ((obj_stk->stkp > obj_stk->stkbot) || (obj_stk->seg != NULL))
      return FALSE;
    los_stk = &context->stack.los;
    if ((los_stk->stkp > los_stk->stkbot) || (los_stk->seg != NULL))
      return FALSE;
  }
  return TRUE;
}

bool smircy_object_marked_p( smircy_context_t *context, word obj )
{
  word *lowest_heap_address;
  word *highest_heap_address;
  word *bitmap;
  word bit_idx, word_idx, bit_in_word;

  lowest_heap_address = context->lowest_heap_address;
  highest_heap_address = context->highest_heap_address;
  bitmap = context->bitmap;

  assert2( isptr( obj ));
  if ( lowest_heap_address <= ptrof( obj ) &&
       ptrof( obj ) < highest_heap_address ) {
    bit_idx = (obj - (word)lowest_heap_address) >> BIT_IDX_SHIFT;
    word_idx = bit_idx >> BIT_IDX_TO_WORD_IDX;
    bit_in_word = 1 << (bit_idx & BIT_IN_WORD_MASK);
    return (bool)(bitmap[ word_idx ] & bit_in_word);
  } else {
    return TRUE; /* all objects outside bitmap considered marked. */
  }
}

void smircy_end( smircy_context_t *context )
{
  /* deallocate state of context */
  obj_stackseg_t *obj, *obj_tmp;
  los_stackseg_t *los, *los_tmp;
  int n;

  free_bitmap( context->bitmap, context->words_in_bitmap );
  free_obj_stk_entries( context->rgn_to_obj_entry, context->num_rgns+1 );
  free_los_stk_entries( context->rgn_to_los_entry, context->num_rgns+1 );

  n = free_obj_stacksegs( context->freed_obj );
  if (n > 1)
    consolemsg( "  Warning: deep mark stack: >%d elements.", n*OBJ_STACK_SIZE );

  n = free_los_stacksegs( context->freed_los );
  if (n > 1)
    consolemsg( "  Warning: deep mark stack: >%d elements.", n*LOS_STACK_SIZE );

  free( context );
}

static void forcibly_push( smircy_context_t *context, word w, word src )
{
  /* marked objects won't be pushed if MARK_ON_PUSH is turned on.
   * Dealing with this by first removing the mark before pushing.
   */
  unmark_object( context, w );
  push( context, w, src );
}
void *smircy_enumerate_stack_of_rgn( smircy_context_t *context, 
                                     int rgn, 
                                     void (*visit)(word *w, void *data),
                                     void *orig_data )
{
  obj_stack_entry_t     *obj_entry;
  obj_stack_entry_t     *obj_entry_next;
  large_object_cursor_t *los_entry;
  word old_word, new_word;

  CHECK_REP( context );

  obj_entry = context->rgn_to_obj_entry[ rgn ];
  los_entry = context->rgn_to_los_entry[ rgn ];
  while (obj_entry != NULL) {
    if (!  ( obj_entry->gno ==  -1 || obj_entry->gno == rgn )) {
      consolemsg("smircy_enumerate_stack_of_rgn "
                 "rgn: %d obj_entry gno: %d val: 0x%08x (%d)",
                 rgn, obj_entry->gno, obj_entry->val, 
                 isptr(obj_entry->val)?gen_of(obj_entry->val):-1);
    }
    assert2( obj_entry->gno ==  -1 || obj_entry->gno == rgn );
    assert2( obj_entry->val == 0x0 || isptr( obj_entry->val ));

    if (!  ( !isptr(obj_entry->val) || gen_of(obj_entry->val) == rgn )) {
      consolemsg("smircy_enumerate_stack_of_rgn "
                 "rgn: %d obj_entry gno: %d val: 0x%08x (%d)",
                 rgn, obj_entry->gno, obj_entry->val, 
                 isptr(obj_entry->val)?gen_of(obj_entry->val):-1);
    }
    /* FSK: sigh.  I cannot assert this in general, because 
     * during a majorgc of rgn R, I use enumerate_stack_of_rgn 
     * to traverse the stack of R (to process the elements on it
     * that may be collected during the collection 
     * process, to ensure snapshot sanity).
     * But if I do that *after* the roots/remsets have otherwise
     * been processed, then the gno's will not be consistent
     * (in the particular case where the stack of R points to 
     * object X that is also pointed to by the roots or remsets)
     *
     * As a band-aid, since I do not want to remove this assertion
     * until I absolutely must, I am going to try doing the mark
     * stack processing *before* the other roots/remsets.)
     */
    assert2( !isptr(obj_entry->val) || gen_of(obj_entry->val) == rgn );

    if (obj_entry->val != 0x0) {
      old_word = obj_entry->val;
      obj_entry->val = 0x0;
      obj_entry->gno = -1;
      visit( &old_word, orig_data );
      new_word = old_word;
      if (gen_of(new_word) != rgn) { /* moved to different region; cleanup. */
        obj_entry->val = 0x0;
        obj_entry->gno = -1;
        forcibly_push( context, new_word, 0x0 );
        /* XXX should actually shift prev ptr downward; but I can
         * avoid that for this first draft, I think ... */
      } else {
        /* XXX in the future, I'll need to deal with this case too; once
         * I start shifting the prev ptr downward, I'll need to update
         * the prev ptr here since I'm keeping this new new_word
         * entry... */
        forcibly_push( context, new_word, 0x0 );
      }
    }
    obj_entry = obj_entry->next_in_rgn;
  }

  while (los_entry != NULL) {
    if (los_entry->object != 0x0) {
      assert2( isptr( los_entry->object ));
      assert2( gen_of(los_entry->object) == rgn );
      old_word = los_entry->object;
      visit( &old_word, orig_data );
      new_word = old_word;
      if (gen_of(new_word) != rgn) { /* moved to different region; cleanup. */
        los_entry->object = 0x0;
        los_push( context, los_entry->index, new_word );
        /* XXX see above notes for standard object stack.
         * Same probably applies here. */
      } else {
        los_entry->object = 0x0;
        los_push( context, los_entry->index, new_word );
        /* XXX see above notes for standard object stack.
         * Same probably applies here. */
      }
    }
    los_entry = los_entry->next_in_rgn;
  }

  CHECK_REP( context );
}

void smircy_jit_process_stack_for_rgn( smircy_context_t *context, int rgn )
{
  obj_stack_entry_t *obj_entry;
  large_object_cursor_t *los_entry;
  word obj_word;
  bool already_marked;
  bool whole_stack_clean;
  smircy_stack_t *stack;

  CHECK_REP( context );

  stack = &context->stack;

  whole_stack_clean = FALSE;
  while ( ! whole_stack_clean ) {
    whole_stack_clean = TRUE; /* assume until proven otherwise */

    while (1) {
      /* This uses the "standard" stack processing as long as the top
       * entry is in rgn -- it should /only/ use the mappings provded by
       * rgn_to_{obj,los}_entry when the top entry is in a different
       * rgn.  (Right now its always using the rgn_to_{obj,los}_entry
       * mappings, which means it is not popping the stack and thus
       * memory usage is higher than it should be.) */
      if (stack->obj.stkp == stack->obj.stkbot) { /* underflow */
        if (stack->obj.seg == NULL) {
          break; /* let code below snag entries from LOS */
        } else {
          stack->obj.seg = pop_obj_segment( stack->obj.seg, 
                                            &context->freed_obj );
          if (stack->obj.seg != NULL) {
            stack->obj.stkbot = stack->obj.seg->data;
            stack->obj.stklim = stack->obj.seg->data+OBJ_STACK_SIZE;
            stack->obj.stkp   = stack->obj.stklim;
          } else {
            stack->obj.stkbot = 0x0;
            stack->obj.stklim = 0x0;
            stack->obj.stkp   = 0x0;

            break; /* let code below snag entries from LOS */
          }
        }
      }
      assert( stack->obj.stkp > stack->obj.stkbot );
      obj_word = stack->obj.stkp[-1].val;
      if (obj_word == 0x0) {
        stack->obj.stkp--; /* pop the dead entry */
        reestablish_rgn_to_obj_entry_post_pop( context, stack->obj.stkp );
        continue;
      }
      if ( gen_of( obj_word ) == rgn ) {
        stack->obj.stkp--;
        assert( context->rgn_to_obj_entry[rgn] == stack->obj.stkp );
        context->rgn_to_obj_entry[rgn] = stack->obj.stkp->next_in_rgn;
        whole_stack_clean = FALSE;
#if MARK_ON_POP
        already_marked = mark_object( context, obj_word );
        if (already_marked) continue;
#endif
        assert2( smircy_object_marked_p( context, obj_word ));
        push_constituents( context, obj_word );
      } else {
        break; /* let code below search for stack entries from rgn */
      }
    }

    obj_entry = context->rgn_to_obj_entry[ rgn ];
    for (; obj_entry != NULL; obj_entry = obj_entry->next_in_rgn) {
      if (obj_entry->val != 0x0) {
        whole_stack_clean = FALSE;
        obj_word = obj_entry->val;
        obj_entry->val = 0x0; /* kill entry directly */
        assert2( isptr(obj_word) );
#if MARK_ON_POP
        already_marked = mark_object( context, obj_word );
        if (already_marked) continue;
#endif
        assert2( smircy_object_marked_p( context, obj_word ));
        push_constituents( context, obj_word );
      }
    }

    los_entry = context->rgn_to_los_entry[ rgn ];
    while (los_entry != NULL && los_entry->object == 0x0) {
      los_entry = los_entry->next_in_rgn;
    }
    if (los_entry != NULL) {
      word obj;
      int window_lim, window_size, window_start, objwords, i;
      whole_stack_clean = FALSE;
      obj = los_entry->object;
      los_entry->object = 0x0; /* kill entry directly */
      window_start = los_entry->index;
      objwords = bytes2words( sizefield( *ptrof(obj) ));
      window_size = min( objwords-window_start, WINDOW_SIZE_LIMIT );
      window_lim = window_start + window_size;
      for (i = window_start; i < window_lim; i++ )
        push( context, vector_ref( obj, i ), obj );
      if (window_lim < objwords)
        los_push( context, window_lim, obj );
    }
  }

  CHECK_REP( context );
}

void smircy_when_object_forwarded( smircy_context_t *context, 
                                   word obj_orig, int gen_orig,
                                   word obj_new, int gen_new )
{
  bool old_is_marked, new_is_marked;

  if (obj_orig == obj_new) {
    dbmsg( "smircy_when_object_forwarded( context, 0x%08x (%d):%s, 0x%08x (%d):%s )",
           obj_orig, gen_orig, smircy_object_marked_p( context, obj_orig )?"M":"U" , 
           obj_new, gen_new,   smircy_object_marked_p( context, obj_new  )?"M":"U" );
  }

  /* Absurdly conservative behavior here. Revisit later after I've
   * ironed out other bugs... */
  if (! is_address_in_bitmap( context, obj_new ) ||
      ! is_address_in_bitmap( context, obj_orig ) ) {
    dbmsg( "smircy_when_object_forwarded( context, 0x%08x (%d), 0x%08x (%d) ) "
           " expanding bitmap",
           obj_orig, gen_orig, obj_new, gen_new );
    expand_bitmap_core( context,
                        min( context->lowest_heap_address, 
                             min( ptrof(obj_orig), ptrof(obj_new) )), 
                        max( context->highest_heap_address, 
                             1+max( ptrof(obj_orig), ptrof(obj_new) )));
  }

  if (is_address_in_bitmap( context, obj_new )) {
    if (smircy_object_marked_p( context, obj_orig )) {
      mark_object( context, obj_new );
    } else if ( gen_orig == 0 ) {
      mark_object( context, obj_new );
    } else { /* obj_orig is unmarked. */
      /* Unmark new address, since old object was not marked 
       * (either because it was dead, or because marker has not
       *  gotten around to it yet).
       */
      unmark_object( context, obj_new );
    }
  } else { 
    if (smircy_object_marked_p( context, obj_orig ) || ( gen_orig == 0 )) {
      /* obj_orig was marked (or is in nursery and assumed to be
       * live); obj_new lies outside bitmap range and will be treated
       * as marked without taking any explicit steps here. */
    } else {
      /* obj_orig not marked and not in nursery ==> keep bit clear. */
      int oldsz; 
      int newsz;
      oldsz = context->words_in_bitmap;
      assert2(! smircy_object_marked_p( context, obj_orig )); /* start sane? */
      /* expensive; hopefully uncommon! */
      expand_bitmap_core( context, 
                          min( context->lowest_heap_address, 
                               min( ptrof( obj_orig ), ptrof( obj_new ))),
                          max( context->highest_heap_address,
                               1+max( ptrof( obj_orig ), ptrof( obj_new ))));
      expand_context( context ); /* expensive; hopefully uncommon! */
      assert2(! smircy_object_marked_p( context, obj_orig )); /* still sane? */
      newsz = context->words_in_bitmap;
      dbmsg("forwarding unmarked 0x%08x (%d) to 0x%08x (%d) in new area; "
            "expanding mark bitmap, old: %d new: %d delta: %d",
            obj_orig, gen_orig, obj_new, gen_new, 
            oldsz, newsz, (newsz-oldsz));

      /* Unmark new address, since old object was not marked 
       * (either because it was dead, or because marker has not
       *  gotten around to it yet).
       */
      unmark_object( context, obj_new );
    }
  }
}

void smircy_set_object_visitor( smircy_context_t *context, 
                                void* (*visitor)( word obj, 
                                                  word src, 
                                                  void *data ),
                                void *visit_data )
{
  assert(0); /* not implemented yet (maybe never). */
}

void* smircy_get_object_visitor_data( smircy_context_t *context )
{
  assert(0); /* not implemented yet (maybe never). */
}

int smircy_objs_marked( smircy_context_t *context ) {
  return context->total_marked;
}
int smircy_arcs_traced( smircy_context_t *context ) {
  return context->total_traced;
}
int smircy_words_marked( smircy_context_t *context ) {
  return context->total_words_marked;
}

void smircy_push_elems( smircy_context_t *context, word *bot, word *top ) 
{
  word *p, *q, w;
  int gno;

  CHECK_REP( context );

  p = bot;
  q = top;
  while (q > p) {
    q--;
    w = *q;
    assert(w != 0x0);
    gno = gen_of(w);
    push( context, w, 0x0 );
  }

  CHECK_REP( context );
}

void smircy_drop_cleared_stack_entries( smircy_context_t *context, int gno ) 
{
  obj_stack_entry_t **p_obj_cursor;
  obj_stack_entry_t *obj_cursor;
  large_object_cursor_t **p_los_cursor;
  large_object_cursor_t *los_cursor;

  CHECK_REP( context );

  p_obj_cursor = &context->rgn_to_obj_entry[gno];
  obj_cursor = *p_obj_cursor;
  while (obj_cursor != NULL) {
    if (obj_cursor->val == 0x0) {
      *p_obj_cursor = obj_cursor->next_in_rgn;
    } else {
      p_obj_cursor = &obj_cursor->next_in_rgn;
    }
    obj_cursor = *p_obj_cursor;
  }

  p_los_cursor = &context->rgn_to_los_entry[gno];
  los_cursor = *p_los_cursor;
  while (los_cursor != NULL) {
    if (los_cursor->object == 0x0) {
      *p_los_cursor = los_cursor->next_in_rgn;
    } else {
      p_los_cursor = &los_cursor->next_in_rgn;
    }
    los_cursor = *p_los_cursor;
  }

  CHECK_REP( context );
}

static void *smircy_enumerate_whole_stack3( smircy_context_t *context,
                                            void *visit( word *pw, int *pgno, void *data ),
                                            void *orig_data ) 
{
  obj_stack_t *obj_stack;
  los_stack_t *los_stack;
  obj_stackseg_t *seg;
  obj_stack_entry_t *stkp;
  obj_stack_entry_t *stkbot;
  word w;
  void *data = orig_data;

  obj_stack = &context->stack.obj;
  los_stack = &context->stack.los;
  seg = obj_stack->seg;
  stkp = obj_stack->stkp;
  stkbot = obj_stack->stkbot;
  while (stkp > stkbot) {
    stkp--;
    w = stkp->val;
    data = visit(&w, &stkp->gno, data);
    if (stkp == stkbot && seg->next != NULL) {
      seg = seg->next;
      stkp = seg->data+OBJ_STACK_SIZE;
      stkbot = seg->data;
    }
  }
  return data;
}

void smircy_check_rep( smircy_context_t *context ) 
{
  /* for each region R, rgn_to_obj_entry[R] points to first entry associated
   * with R in stack.obj, and rgn_to_los_entry[R] points to first entry associated
   * with R in stack.los.  Furthermore, following the thread of ->next pointers
   * in each will trace through the successor elements associated with R in the
   * corresponding stack. 
   */
  int r;
  obj_stack_entry_t *obj_entry;
  large_object_cursor_t *los_entry;
  obj_stackseg_t *obj_seg;
  obj_stack_entry_t *obj_stkp;
  obj_stack_entry_t *obj_stkbot;
  los_stackseg_t *los_seg;
  large_object_cursor_t *los_stkp;
  large_object_cursor_t *los_stkbot;

  for ( r = 1; r <= context->num_rgns; r++ ) {
    obj_entry  = context->rgn_to_obj_entry[r];
    obj_seg    = context->stack.obj.seg;
    obj_stkp   = context->stack.obj.stkp;
    obj_stkbot = context->stack.obj.stkbot;

    obj_stkp--;
    while (obj_entry != NULL && obj_seg != NULL) {
      while (obj_stkp != obj_entry) {
        obj_stkp--;
        if (obj_stkp < obj_stkbot) {
          obj_seg = obj_seg->next;
          if (obj_seg != NULL) {
            obj_stkp   = obj_seg->data+OBJ_STACK_SIZE;
            obj_stkbot = obj_seg->data;
          } else {
            consolemsg("Smircy obj stack entry for region %d "
                       "not in stack.", r);
            assert(FALSE);
          }
        }
      }
      obj_entry = obj_entry->next_in_rgn;
      assert( obj_stkp != obj_entry );
      obj_stkp--;
    }
    assert( obj_entry == NULL );
    while (obj_seg != NULL) {
      while (obj_stkp >= obj_stkbot) {
        assert( obj_stkp->val == 0x0 || 
                (gen_of( obj_stkp->val ) == obj_stkp->gno) );
        assert( obj_stkp->gno != r );
        obj_stkp--;
      }
      obj_seg = obj_seg->next;
      if (obj_seg != NULL) {
        obj_stkbot = obj_seg->data;
        obj_stkp   = obj_seg->data+OBJ_STACK_SIZE;
        obj_stkp--;
      }
    }

    los_entry  = context->rgn_to_los_entry[r];
    los_seg    = context->stack.los.seg;
    los_stkp   = context->stack.los.stkp;
    los_stkbot = context->stack.los.stkbot;

    los_stkp--;
    while (los_entry != NULL && los_seg != NULL) {
      while (los_stkp != los_entry) {
        los_stkp--;
        if (los_stkp < los_stkbot) {
          los_seg = los_seg->next;
          if (los_seg != NULL) {
            los_stkp   = los_seg->data+LOS_STACK_SIZE;
            los_stkbot = los_seg->data;
          } else {
            consolemsg("Smircy los stack entry for region %d "
                       "not in stack.", r);
            assert(FALSE);
          }
        }
      }
      los_entry = los_entry->next_in_rgn;
      assert( los_stkp != los_entry );
      los_stkp--;
    }
    assert( los_entry == NULL );
    while (los_seg != NULL) {
      while (los_stkp >= los_stkbot) {
        assert( los_stkp->object == 0x0 || (gen_of(los_stkp->object) != r));
        los_stkp--;
      }
      los_seg = los_seg->next;
      if (los_seg != NULL) {
        los_stkbot = los_seg->data;
        los_stkp   = los_seg->data+OBJ_STACK_SIZE;
        los_stkp--;
      }
    }
  }
}

/* eof */
