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
 * The marking machine maintains a mark stack for each region (rather
 * than a single stack for the entire heap).
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

/* XXX To get something working, layering this on top of msgc for now.
 * Its obviously the wrong thing and should never be checked in (or
 * admitted to).
 */
#include "msgc-core.h"

/* Important ideas to copy from msgc-core.c:
 * - scan large objects incrementally, by keeping a separate stack of 
 *   cursors into the middle of large objects.
 */

#if defined(BITS_32)
# define BIT_IDX_SHIFT       3  /* shift to get doubleword bit address */
# define BIT_IDX_TO_WORD_IDX 5  /* shift to get word addr from bit addr */
# define BIT_IN_WORD_MASK   31  /* mask to get bit shift */
# define BITS_IN_WORD       32
#else
# error "Must define SMIRCY macros for non-32 bit systems."
#endif

typedef struct smircy_stack        smircy_stack_t;
typedef struct obj_stackseg        obj_stackseg_t;
typedef struct los_stackseg        los_stackseg_t;
typedef struct obj_stack           obj_stack_t;
typedef struct los_stack           los_stack_t;
typedef struct large_object_cursor large_object_cursor_t;

#define OBJ_STACK_SIZE 4094
/* Should be large and chosen so that obj_stackseg occupies integral
 * number of pages. */

#define LOS_STACK_SIZE 2047
/* Should be large and chosen so that los_stackseg occupies integral
 * number of pages.  (Since a los cursor is > 1 word, I may need to
 * add explicit padding to los_stack to get that effect...) */

#define WINDOW_SIZE_LIMIT 1024
/* The max number of entries that we'll extract from a large object
 * and push onto the mark stack before processing the stack. */

/* All objects are double word aligned */
#define MIN_BYTES_PER_OBJECT (2*sizeof(word))

#define BITS_PER_WORD (8*sizeof(word))

/* Calculates ceil(x/y); (unlike quotient, which does floor). */
#define CEILDIV(x,y) (roundup((x),(y))/(y))

struct obj_stack {
  obj_stackseg_t *seg;
  word           *stkp;
  word           *stkbot;
  word           *stklim;
};

struct obj_stackseg {
  obj_stackseg_t *next;
  int             gno_owner;
  word            data[OBJ_STACK_SIZE];
};

struct los_stack {
  los_stackseg_t      *seg;
  large_object_cursor_t *stkp;
  large_object_cursor_t *stkbot;
  large_object_cursor_t *stklim;
};

struct large_object_cursor {
  word       object;
  int        index; /* Resumption point for scan (from start of object) */
};

struct los_stackseg {
  struct large_object_cursor data[LOS_STACK_SIZE];
  los_stackseg_t            *next;
};

struct smircy_stack {
  int            gno_owner;
  obj_stack_t    obj;         /* Object stack */
  los_stack_t    los;         /* LOS cursor stack */
};

struct smircy_context {
  gc_t               *gc;
  int                num_rgns;
  word               *lowest_heap_address;
  word               *highest_heap_address;
  int                words_in_bitmap;
  word               *bitmap;
  smircy_stack_t     *stacks;     /* One stack per region */
  int                stk_cursor;  /* Index of stack we are manipulating */
  obj_stackseg_t     *freed_obj;  /* obj segments available for reuse */
  los_stackseg_t     *freed_los;  /* los segments available for reuse */
  int                total_traced;
  int                total_marked;
  int                total_words_marked;
};

static void print_stacks( smircy_context_t *context )
{
  int gno;
  obj_stack_t *obj_stack;
  los_stack_t *los_stack;
  obj_stackseg_t *seg;
  word *stkp;
  word *stkbot;
  word w;
  for( gno = 0; gno <= context->num_rgns; gno++ ) {
    obj_stack = &context->stacks[gno].obj;
    los_stack = &context->stacks[gno].los;
    seg = obj_stack->seg;
    stkp = obj_stack->stkp;
    stkbot = obj_stack->stkbot;
    consolemsg(    "stack[% 4d]:    [0x%08x 0x%08x]", gno, stkbot, stkp);
    while (stkp > stkbot) {
      stkp--;
      w = *stkp;
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
  int gno;
  obj_stack_t *obj_stack;
  los_stack_t *los_stack;
  obj_stackseg_t *objseg;
  los_stackseg_t *losseg;
  word w;
  int full_objseg_count, full_losseg_count;
  for( gno = 0; gno <= context->num_rgns; gno++ ) {
    obj_stack = &context->stacks[gno].obj;
    full_objseg_count = 0;
    objseg = obj_stack->seg;
    if (objseg != NULL) {
      while (objseg->next != NULL) {
        full_objseg_count += 1;
        objseg = objseg->next;
      }
    }

    los_stack = &context->stacks[gno].los;
    full_losseg_count = 0;
    losseg = los_stack->seg;
    if (losseg != NULL) {
      while (losseg->next != NULL) {
        full_losseg_count += 1;
        losseg = losseg->next;
      }
    }
    if (full_objseg_count == 0 && full_losseg_count == 0) {
      consolemsg( "OBJSTK[% 3d]:             % 5d " 
                  "LOSSTK[% 3d]:             % 5d ",
                  gno, obj_stack->stkp - obj_stack->stkbot,
                  gno, los_stack->stkp - los_stack->stkbot);
    } else if (full_objseg_count == 0) {
      consolemsg( "OBJSTK[% 3d]:             % 5d " 
                  "LOSSTK[% 3d]: % 4d*%d + % 5d ",
                  gno, obj_stack->stkp - obj_stack->stkbot,
                  gno, full_losseg_count, LOS_STACK_SIZE, los_stack->stkp - los_stack->stkbot);
    } else if (full_losseg_count == 0) {
      consolemsg( "OBJSTK[% 3d]: % 4d*%d + % 5d " 
                  "LOSSTK[% 3d]:             % 5d ",
                  gno, full_objseg_count, OBJ_STACK_SIZE, obj_stack->stkp - obj_stack->stkbot,
                  gno, los_stack->stkp - los_stack->stkbot);
    } else {
      consolemsg( "OBJSTK[% 3d]: % 4d*%d + % 5d " 
                  "LOSSTK[% 3d]: % 4d*%d + % 5d ",
                  gno, full_objseg_count, OBJ_STACK_SIZE, obj_stack->stkp - obj_stack->stkbot,
                  gno, full_losseg_count, LOS_STACK_SIZE, los_stack->stkp - los_stack->stkbot);
    }
  }
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
    sp = gclib_alloc_rts( sizeof( obj_stackseg_t ), 0 );
    sp->gno_owner = gno_owner;

    dbmsg( "SMIRCY push_obj_segment( 0x%08x, [0x%08x] ) => 0x%08x [%d]", 
           obj, *freed, sp, gno_owner );

    sp->next = obj;
    assert( sp != NULL );
    return sp;
  } else {
    sp = *freed;
    sp->gno_owner = gno_owner;

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

  dbmsg( "SMIRCY  pop_obj_segment( 0x%08x, [0x%08x] ) => 0x%08x [%d]", 
         obj, *freed, sp, obj->gno_owner );

#if ATTEMPT_SEGMENT_REUSE
  obj->next = *freed;
  *freed = obj;
#else
  gclib_free( obj, sizeof( obj_stackseg_t ) );
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
    sp = gclib_alloc_rts( sizeof( los_stackseg_t ), 0 );

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
  gclib_free( los, sizeof( los_stackseg_t ) );
#endif
  return sp;
}

static void free_obj_stacksegs( obj_stackseg_t *segs ) 
{
  if (segs != NULL) {
    free_obj_stacksegs( segs->next ); /* (should be sufficiently shallow) */
    gclib_free( segs, sizeof( obj_stackseg_t ) );
  }
}

static void free_los_stacksegs( los_stackseg_t *segs ) 
{
  if (segs != NULL) {
    free_los_stacksegs( segs->next );
    gclib_free( segs, sizeof( los_stackseg_t ) );
  }
}

static void push( smircy_context_t *context, word obj, word src ) 
{
  int gno;
  obj_stack_t *stack;

  if (isptr(obj)) {

    gno = gen_of(obj);
    stack = &(context->stacks[gno].obj);
    if (stack->stkp == stack->stklim) {
      stack->seg = push_obj_segment( stack->seg, &context->freed_obj, gno );
      stack->stkbot = stack->seg->data;
      stack->stklim = stack->seg->data+OBJ_STACK_SIZE;
      stack->stkp = stack->stkbot;
    }

    *stack->stkp = obj;
    stack->stkp++;
  }
}

static void los_push( smircy_context_t *context, word index, word obj ) 
{
  int gno;
  los_stack_t *stack;
  assert(isptr(obj));
  gno = gen_of(obj);
  stack = &(context->stacks[gno].los);
  if (stack->stkp == stack->stklim) {
    stack->seg = push_los_segment( stack->seg, &context->freed_los );
    stack->stkbot = stack->seg->data;
    stack->stklim = stack->seg->data+LOS_STACK_SIZE;
    stack->stkp = stack->stkbot;
  }
  stack->stkp->object = obj;
  stack->stkp->index = index;
  stack->stkp++;
}

/* Attempts to enqueue entries on object stack for gno by dequeuing them
 * from the los cursor stack for gno. */
static bool fill_from_los_stack( smircy_context_t *context, int gno )
{
  obj_stack_t *obj_stack;
  los_stack_t *los_stack;
  word obj;
  int i, window_start, window_size, window_lim, objwords;

  dbmsg("fill_from_los_stack( context, %d )", gno );

  obj_stack = &context->stacks[gno].obj;
  los_stack = &context->stacks[gno].los;
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
  obj = los_stack->stkp->object;
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
  char *lowest, *highest;
  int max_obj_count;

  context = must_malloc( sizeof( smircy_context_t ) );
  context->gc = gc;
  context->num_rgns = num_rgns;

  gclib_memory_range( &lowest, &highest );
  context->lowest_heap_address = (word*)lowest;
  context->highest_heap_address = (word*)highest;
  /* Upper bound on number of objects that fit in memory range. */
  max_obj_count = CEILDIV(highest-lowest,MIN_BYTES_PER_OBJECT);
  context->words_in_bitmap = CEILDIV(max_obj_count,BITS_PER_WORD);
  context->bitmap = 
    gclib_alloc_rts( context->words_in_bitmap * sizeof(word), 0 );
  memset( context->bitmap, 0, context->words_in_bitmap*sizeof(word) );
  context->stacks = gclib_alloc_rts( sizeof(smircy_stack_t)*(num_rgns+1), 0 );
  context->stk_cursor = 0;
  memset( context->stacks, 0, sizeof(smircy_stack_t)*(num_rgns+1) );
  { 
    int gno;
    for ( gno = 0; gno <= num_rgns; gno++ ) {
      context->stacks[gno].gno_owner = gno;
    }
  }

  context->freed_obj = NULL;
  context->freed_los = NULL;

  context->total_traced = 0;
  context->total_marked = 0;
  context->total_words_marked = 0;

  dbmsg( "smircy_begin( gc, %d ) bitmap: 0x%08x stacks: 0x%08x",
              num_rgns, context->bitmap, context->stacks );

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
  gc_enumerate_roots( context->gc, push_root, (void*)context );
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
  rs_enumerate( rs, push_remset_entry, context );
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
static bool mark_object( smircy_context_t *context, word obj )
{
  bool rtn;
  assert2( context->lowest_heap_address <= ptrof( obj ) &&
           ptrof( obj ) < context->highest_heap_address );
  dbmsg( "smircy mark_object( context, 0x%08x (%d) )", obj, gen_of(obj) );
  rtn = mark_word( context->bitmap, obj, (word)context->lowest_heap_address );
  assert2( smircy_object_marked_p( context, obj ));
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

void *visit_check_smircy_mark( word obj, word src, void *data ) 
{
  smircy_context_t *c = (smircy_context_t*)data;
  if ( isptr(obj) && ! smircy_object_marked_p( c, obj )) {
    if (src == 0x0) {
      dbmsg( "msgc: root ~> obj 0x%08x; smircy: obj %s", 
             obj, smircy_object_marked_p( c, obj )?"Y":"N" );
    } else {
      dbmsg( "msgc: src 0x%08x ~> obj 0x%08x; smircy: src %s obj %s",
             src, obj, 
             smircy_object_marked_p( c, src )?"Y":"N",
             smircy_object_marked_p( c, obj )?"Y":"N" );
    }
    assert( smircy_object_marked_p( c, obj ));
  }
  return data;
}

static bool smircy_assert_conservative_approximation( smircy_context_t *context );

void smircy_progress( smircy_context_t *context, 
                      int mark_max, int trace_max, int mark_words_max,
                      int *marked_recv, int *traced_recv, 
                      int *words_marked_recv )
{
  word w;
  int rgn;
  smircy_stack_t *stack;
  int mark_budget, trace_budget, mark_words_budget;
  int traced = 0, marked = 0, words_marked = 0, constituents;
  bool already_marked;
  bool all_stacks_empty;

  mark_budget = mark_max;
  trace_budget = trace_max;
  mark_words_budget = mark_words_max;

  do {
    /* only assume all stacks are empty if we are starting with the 
     * first one */
    all_stacks_empty = (context->stk_cursor == 0);

    for( rgn = context->stk_cursor; 
         rgn < context->num_rgns; 
         rgn++ ) {
      context->stk_cursor = rgn;
      stack = &context->stacks[rgn];
      dbmsg("smircy process stack for rgn %d, "
            "stk{bot,p,lim}:{0x%08x,0x%08x,0x%08x}", 
            rgn, stack->obj.stkbot, stack->obj.stkp, stack->obj.stklim );

      while(1) {
        /* pop */
        if (stack->obj.stkp == stack->obj.stkbot) { /* underflow */
          if (stack->obj.seg == NULL) {
            if (fill_from_los_stack( context, rgn )) 
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

              if (fill_from_los_stack( context, rgn ))
                continue; /* restart processing of stacks[rgn] */
              else
                break; /* done with stacks[rgn] for now */
            }
          }
        }

        assert( stack->obj.stkp > stack->obj.stkbot );

        all_stacks_empty = FALSE;
        stack->obj.stkp--;
        w = *stack->obj.stkp;

        dbmsg( "SMIRCY popped 0x%08x off of stack[%d]", 
               w, stack->gno_owner );

        /* XXX at some point, I may need to support removing words
         * from stacks, which will probably be implemented by setting
         * the slot holding them to NULL.  But for now that functionality
         * is not needed, so it seems easier to disallow NULL for now 
         * as I debug what is wrong with this implementation. */
        assert( w != 0x0 );

        traced++;
        bounded_decrement( trace_budget, 1 );

        already_marked = mark_object( context, w );
        if (already_marked) continue;

        marked++;
        bounded_decrement( mark_budget, 1 );

        constituents = push_constituents( context, w );
        words_marked += constituents;
        bounded_decrement( mark_words_budget, constituents );

        if (mark_budget == 0 || trace_budget == 0 || mark_words_budget == 0)
          goto budget_exhausted;
      }
    }

    /* finished iterating over all stacks; reset cursor to 0 */
    context->stk_cursor = 0;

  } while (! all_stacks_empty);

  budget_exhausted:
  done:
  *marked_recv = marked;
  *traced_recv = traced;
  *words_marked_recv = words_marked;

#if !NDEBUG2
  if (smircy_stack_empty_p( context )) {
    smircy_assert_conservative_approximation( context );
  }

#if 0
  if (smircy_stack_empty_p( context )) {
    consolemsg("--------------------------------");
  } else {
    print_stack_sizes( context );
  }
#endif
#endif
}

static bool smircy_assert_conservative_approximation( smircy_context_t *context ) 
{
  /* When debugging, if we're done with the marking lets check that it
   * is a conservative approximation of the true mark bitmap (that is,
   * a superset of objects marked by MSGC should be treated as live by
   * SMIRCY.
   * 
   * XXX "treated as live" usually means marked, but Felix anticipates
   * needing special treatment for blocks of memory that lie outside
   * the bitmap... that can wait until things are running more
   * smoothly though...
   */
  {
    msgc_context_t *msgc_ctxt; 
    word *low, *hgh, *ptr, tptr;
    int marked, traced, words_marked;
    msgc_ctxt = msgc_begin( context->gc );
    msgc_set_object_visitor( msgc_ctxt, visit_check_smircy_mark, context );
    msgc_mark_objects_from_roots( msgc_ctxt, &marked, &traced, &words_marked );
    low = context->lowest_heap_address;
    hgh = context->highest_heap_address;
    for ( ptr = low; ptr < hgh; ptr += 2 ) {
      tptr = ((word)ptr) | PAIR_TAG; /* artificial pair tag */
      if (msgc_object_marked_p( msgc_ctxt, tptr )) {
        switch (header(*ptr)) {
        case VEC_HDR: 
          tptr = ((word)ptr) | VEC_TAG; break;
        case BV_HDR:
          tptr = ((word)ptr) | BVEC_TAG; break;
        case PROC_HDR:
          tptr = ((word)ptr) | PROC_TAG; break;
        default:
          tptr = ((word)ptr) | PAIR_TAG; break;
        }
      }
      if (msgc_object_marked_p( msgc_ctxt, tptr )) {
        if (! smircy_object_marked_p( context, tptr )) {
          dbmsg( "SMIRCY: 0x%08x msgc says reachable smircy did not mark "
                 "[low: 0x%08x, hgh: 0x%08x]",
                 tptr, low, hgh );
        }
        assert( smircy_object_marked_p( context, tptr ));
      }
    }
    msgc_end( msgc_ctxt );
  }
}

bool smircy_stack_empty_p( smircy_context_t *context ) 
{
  int gno;
  obj_stack_t *obj_stk;
  los_stack_t *los_stk;
  for( gno = 0; gno <= context->num_rgns; gno++ ) {
    obj_stk = &context->stacks[gno].obj;
    if ((obj_stk->stkp > obj_stk->stkbot) || (obj_stk->seg != NULL))
      return FALSE;
    los_stk = &context->stacks[gno].los;
    if ((los_stk->stkp > los_stk->stkbot) || (los_stk->seg != NULL))
      return FALSE;
  }
  return TRUE;
}

bool smircy_object_marked_p( smircy_context_t *context, word obj )
{
  word bit_idx, word_idx, bit_in_word;

  assert2( isptr( obj ));
  assert2( context->lowest_heap_address <= ptrof( obj ) &&
           ptrof( obj ) < context->highest_heap_address );

  bit_idx = (obj - (word)context->lowest_heap_address) >> BIT_IDX_SHIFT;
  word_idx = bit_idx >> BIT_IDX_TO_WORD_IDX;
  bit_in_word = 1 << (bit_idx & BIT_IN_WORD_MASK);
  return (bool)(context->bitmap[ word_idx ] & bit_in_word);
}

void smircy_end( smircy_context_t *context )
{
  /* deallocate state of context */
  obj_stackseg_t *obj, *obj_tmp;
  los_stackseg_t *los, *los_tmp;

  gclib_free( context->bitmap, context->words_in_bitmap * sizeof(word) );
  gclib_free( context->stacks, (context->num_rgns+1) * sizeof(smircy_stack_t) );
  obj = context->freed_obj;
  while (obj != NULL) {
    obj_tmp = obj;
    obj = obj->next;
    gclib_free( obj_tmp, sizeof( obj_stackseg_t ) );
  }
  los = context->freed_los;
  while (los != NULL) {
    los_tmp = los;
    los = los->next;
    gclib_free( los_tmp, sizeof( los_stackseg_t ) );
  }
  free( context );
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

/* eof */
