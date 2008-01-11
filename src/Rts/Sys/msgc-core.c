/* Copyright 1999 Lars T Hansen           -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Mark/Sweep garbage collection core functionality.
 *
 * This is not a mark/sweep collector, but some routines that
 * the collector could conceivably use.  (Right now this module
 * serves as a subroutine in the whole-heap subcollector in the
 * DOF collector.)
 */

#define GC_INTERNAL

#include "larceny.h"
#include "gc_t.h"
#include "gclib.h"
#include "msgc-core.h"
#include "young_heap_t.h" /* for yh_is_address_mapped */
#include "static_heap_t.h" /* for sh_is_address_mapped */

#define LARGE_OBJECT_LIMIT 1024 /* elements */

#if defined(BITS_32)
# define BIT_IDX_SHIFT     3    /* shift to get doubleword bit address */
# define BITS_TO_WORDS     5    /* shift to get word addr from bit addr */
# define BIT_IN_WORD_MASK  31   /* mask to get bit shift */
# define BITS_IN_WORD      32
#else
# error "Must define MSGC macros for non-32 bit systems."
#endif

typedef struct msgc_stackseg msgc_stackseg_t;
typedef struct msgc_stack msgc_stack_t;

struct msgc_stack {
  msgc_stackseg_t *seg;          /* current segment */
  word            *stkp;
  word            *stkbot;
  word            *stklim;
};

struct msgc_context {
  gc_t         *gc;
  word         *lowest_heap_address;
  word         *highest_heap_address;
  int          words_in_bitmap; /* Size of mark bitmap */
  word         *bitmap;         /* Mark bitmap */
  msgc_stack_t stack;           /* Normal mark stack */
  msgc_stack_t los_stack;       /* Mark stack for large objects */
  int          traced;          /* Pointers traced */
  int          marked;          /* Objects marked */
  int          words_marked;    /* Words marked */
};

#define STACKSIZE  4094        /* Must be an even number of words */
  /* STACKSIZE should be large and should be selected so that 
     msgc_stackseg occupies an integral number of pages,
     to avoid wasted space.  Most programs have shallow stacks
     and the exact fit doesn't matter.
     */

struct msgc_stackseg {
  word            data[STACKSIZE];
  msgc_stackseg_t *prev;           /* Previous segment (older) */
  msgc_stackseg_t *next;           /* Next segment (younger) */
};


static void push_segment( msgc_stack_t *stack )
{
  msgc_stackseg_t *sp;
  
  if (stack->seg != 0 && stack->seg->next != 0)
    sp = stack->seg->next;
  else {
    sp = gclib_alloc_rts( sizeof(msgc_stackseg_t), 0 );
    sp->prev = stack->seg;
    if (stack->seg != 0) stack->seg->next = sp;
    sp->next = 0;
  }
  
  stack->seg = sp;
  stack->stkbot = stack->seg->data;
  stack->stklim = stack->seg->data+STACKSIZE;
  stack->stkp = stack->stkbot;
}

static bool pop_segment( msgc_stack_t *stack )
{
  if (stack->seg->prev == 0) return FALSE;
  
  stack->seg = stack->seg->prev;
  stack->stkbot = stack->seg->data;
  stack->stklim = stack->seg->data+STACKSIZE;
  stack->stkp = stack->stklim;
  
  return TRUE;
}

static int free_stack( msgc_stackseg_t *stack )
{
  int n = 0;

  if (stack != 0) {
    n = 1 + free_stack( stack->next );
    gclib_free( stack, sizeof( msgc_stackseg_t ) );
  }
  return n;
}

static void assert2_basic_invs( msgc_context_t *context, word src, word obj )
{
#ifndef NDEBUG2
  word TMP = obj;
  if (isptr(TMP)) {
    gc_check_remset_invs( context->gc, src, obj );
    assert2( (context)->lowest_heap_address <= (word*)TMP );
    assert2( (word*)TMP < (context)->highest_heap_address );
    if (! gc_is_address_mapped( context->gc, (word*)TMP, FALSE )) {
      assert2(gc_is_address_mapped( context->gc, (word*)TMP, TRUE ));
    }
    if (tagof(TMP) == PAIR_TAG) {
      /* no header on pairs... */
    } else if (tagof(TMP) == VEC_TAG) {
      assert2(ishdr(*ptrof(TMP)));
      assert2(header(*ptrof(TMP)) == header(VEC_HDR));
    } else if (tagof(TMP) == PROC_TAG) {
      assert2(ishdr(*ptrof(TMP)));
      assert2(header(*ptrof(TMP)) == header(PROC_HDR));
    }
  }
#endif
}

static void assert2_address_mapped( msgc_context_t *context, word obj )
{
#ifndef NDEBUG2
  if (! yh_is_address_mapped( context->gc->young_area, obj ) &&
      ! los_is_address_mapped( context->gc->los, obj ) &&
      ! sh_is_address_mapped( context->gc->static_area, obj, FALSE )) {
    assert( gc_is_address_mapped( context->gc, obj, TRUE ));
    assert( yh_is_address_mapped( context->gc->young_area, obj ) ||
            los_is_address_mapped( context->gc->los, obj ) ||
            sh_is_address_mapped( context->gc->static_area, obj, TRUE ));
  }
#endif
}

static void assert2_los_addresses_mapped( msgc_context_t *context, word obj, 
                                          int k, int next ) 
{
#ifndef NDEBUG2
  int i;
  for ( i=0 ; i < k ; i++ ) {
    if (isptr(vector_ref(obj, i+next)) &&
        ! gc_is_address_mapped( context->gc, 
                                (word*) vector_ref(obj, i+next), FALSE )) {
      assert( gc_is_address_mapped( context->gc, obj, TRUE ));
      consolemsg("unmapped address, los vector 0x%08x in gen %d, elem [%d] = 0x%08x",
                 obj, gen_of(obj), i+next, vector_ref(obj, i+next ));
      consolemsg("(remset count: %d)", context->gc->remset_count);
      assert2(0);
    }
  }
#endif
}

static void assert2_pair_addresses_mapped( msgc_context_t *context, word w )
{
    { 
#ifndef NDEBUG2
      if (isptr(pair_cdr(w)) &&
          ! gc_is_address_mapped( context->gc, 
                                  (word*)pair_cdr(w), FALSE )) {
        gc_is_address_mapped( context->gc, (word*)pair_cdr(w), TRUE );
        consolemsg("unmapped address, pair 0x%08x in gen %d, cdr = 0x%08x",
                   w, gen_of(w), pair_cdr(w));
        consolemsg("(remset count: %d)", context->gc->remset_count);
        assert2(0);
      }
      if (isptr(pair_car(w)) &&
          ! gc_is_address_mapped( context->gc, 
                                  (word*)pair_car(w), FALSE )) {
        gc_is_address_mapped( context->gc, (word*)pair_car(w), TRUE );
        consolemsg("unmapped address, pair 0x%08x in gen %d, car = 0x%08x",
                   w, gen_of(w), pair_car(w));
        consolemsg("(remset count: %d)", context->gc->remset_count);
        assert2(0);
      }
#endif
    }
}

static void assert2_tag_hdr_consistency( msgc_context_t *context, word w ) 
{
#ifndef NDEBUG2
    switch (tagof(w)) {
    case VEC_TAG:
      if (!(ishdr(*ptrof(w)) && header(*ptrof(w)) == header(VEC_HDR) )) {
        consolemsg("VEC  w: 0x%08x *ptrof(w): 0x%08x ishdr: %s header(*ptrof(w)): %x", w, *ptrof(w), ishdr(*ptrof(w))?"TRUE":"FALSE", header(*ptrof(w)));
      }
      assert( ishdr(*ptrof(w)) && header(*ptrof(w)) == header(VEC_HDR) );
      break;
    case PROC_TAG:
      if (!( ishdr(*ptrof(w)) && header(*ptrof(w)) == header(PROC_HDR) )) {
        consolemsg("PROC w: 0x%08x *ptrof(w): 0x%08x ishdr: %s header(*ptrof(w)): %x", w, *ptrof(w), ishdr(*ptrof(w))?"TRUE":"FALSE", header(*ptrof(w)));
      }
      assert( ishdr(*ptrof(w)) && header(*ptrof(w)) == header(PROC_HDR) );
      break;
    }
#endif

}

static void assert2_object_contents_mapped( msgc_context_t *context, word w, 
                                            int n )
{
#ifndef NDEBUG2
      int i;
      for ( i=0 ; i < n ; i++ ) {
        if (isptr(vector_ref( w, i )) &&
            ! gc_is_address_mapped( context->gc, 
                                    (word*)vector_ref( w, i ), FALSE )) {
          consolemsg("unmapped address, vector 0x%08x in gen %d, elem [%d] = 0x%08x", 
                     w, gen_of( w ), i, vector_ref( w, i ));
          consolemsg("(remset count: %d)", context->gc->remset_count);
          assert2(0);
        }
      }
#endif
}

static void assert2_root_address_mapped( msgc_context_t *context, word *loc )
{
#ifndef NDEBUG2
  if (isptr(*loc) &&
      ! gc_is_address_mapped( context->gc, (word*)*loc, FALSE )) {
    assert2(0);
  }
#endif
}

#if 1
#define PUSH( context, obj, src )                               \
  do { word TMP = obj;                                          \
       if (isptr(TMP)) {                                        \
         if ((context)->stack.stkp == (context)->stack.stklim)  \
           push_segment( &((context)->stack) );                 \
         *((context)->stack.stkp++) = TMP;                      \
       }                                                        \
  } while(0)

#define LOS_PUSH( context, next, obj )                                  \
  do { if ((context)->los_stack.stkp == (context)->los_stack.stklim)    \
         push_segment( &context->los_stack );                           \
       *((context)->los_stack.stkp++) = next;                           \
       *((context)->los_stack.stkp++) = obj;                            \
  } while(0)
#else
static void PUSH( msgc_context_t *context, word obj, word src ) {
  word TMP = obj;
  assert2_basic_invs( context, src, obj );
  if (isptr(TMP)) {
    if ((context)->stack.stkp == (context)->stack.stklim)
      push_segment( &((context)->stack) );
    *((context)->stack.stkp++) = TMP;
  }
}
static void LOS_PUSH( msgc_context_t *context, word next, word obj ) {
  assert2_address_mapped( context, obj );
  if ((context)->los_stack.stkp == (context)->los_stack.stklim)
    push_segment( &context->los_stack );
  *((context)->los_stack.stkp++) = next;                           
  *((context)->los_stack.stkp++) = obj;
}
#endif

static bool fill_from_los_stack( msgc_context_t *context )
{
  int next, k, n, i;
  word obj;

  if (context->los_stack.stkp == context->los_stack.stkbot) {
    /* underflow */
    if (!pop_segment( &context->los_stack ))
      return FALSE;
  }
  obj = *--context->los_stack.stkp;
  next = (int)*--context->los_stack.stkp;
  n = bytes2words( sizefield( *ptrof(obj) ) );
  k = min( n-next, LARGE_OBJECT_LIMIT );
  assert2_los_addresses_mapped( context, obj, k, next );
  for ( i=0 ; i < k ; i++ )
    PUSH( context, vector_ref( obj, i+next ), obj );
  if (next+k < n)
    LOS_PUSH( context, next+k, obj );
  return TRUE;
}

static int push_pair_constiuents( msgc_context_t *context, word w ) 
{
  PUSH( context, pair_cdr( w ), w ); /* Do the CDR last */
  PUSH( context, pair_car( w ), w ); /* Do the CAR first */
  return 2;
}

static int push_constituents( msgc_context_t *context, word w )
{
  int i, n;

  switch (tagof(w)) {
  case PAIR_TAG :
    return push_pair_constiuents( context, w );
  case VEC_TAG :
  case PROC_TAG :
    assert2_tag_hdr_consistency( context, w );
    n = bytes2words( sizefield(*ptrof(w)) );
    if (n > LARGE_OBJECT_LIMIT)
      LOS_PUSH( context, 0, w );    /* Treat large objects specially */
    else {
      assert2_object_contents_mapped( context, w, n );
      for ( i=0 ; i < n ; i++ ) {
        PUSH( context, vector_ref( w, i ), w );
      }
    }
    return n+1;
  default :
    return 0;
  }
}

/* Marks obj in bitmap.  Returns true if and only if obj was already
 * marked in the bitmap at theoutset. */
static bool mark_word( word *bitmap, word obj, word first ) {
  bool retval; 
  word bit_idx, word_idx, bit;

  /* Note: marks object only, not entire address range occupied
     by object.  A "real" collector must mark the range.
  */
  bit_idx = (obj - first) >> BIT_IDX_SHIFT;
  word_idx = bit_idx >> BITS_TO_WORDS;
  bit = 1 << (bit_idx & BIT_IN_WORD_MASK);
  retval = (bitmap[ word_idx ] & bit);
  bitmap[ word_idx ] |= bit;
  return retval;
}

/* A couple ways to speed this up:
   - inline push_constituents
   - Cache the stack pointer, stack bottom, and stack limit in
     register variables to avoid the loads and stores; compiler is
     unlikely to do this.
   - Back-to-back push/pop pairs, like when pushing the car of a pair
     and then looping around to pop it again can be joined.
   */
static void mark_from_stack( msgc_context_t *context )
{
  word w;
  word first = (word)context->lowest_heap_address;
  word *bitmap = context->bitmap;
  int traced=0, marked=0, words_marked=0;
  bool already_marked;

  while (1) {
    /* Pop */
    if (context->stack.stkp == context->stack.stkbot)  /* Stack underflow */
      if (!pop_segment( &context->stack )) 
        if (fill_from_los_stack( context ))
          continue;
        else
          break;
    w = *--context->stack.stkp;
    traced++;

    /* Mark */
    already_marked = mark_word( bitmap, w, first );
    if (already_marked) continue;
    marked++;

    words_marked += push_constituents( context, w );
  }
  context->traced += traced;
  context->marked += marked;
  context->words_marked += words_marked;
}

static void push_root( word *loc, void *data )
{
  assert2_root_address_mapped( (msgc_context_t*)data, loc );
  PUSH( (msgc_context_t*)data, *loc, 0x0 );
}

bool msgc_object_marked_p( msgc_context_t *context, word obj )
{
  word bit_idx, word_idx, bit;

  assert2( isptr( obj ) );
  assert2( context->lowest_heap_address <= ptrof( obj ) &&
           ptrof( obj ) < context->highest_heap_address );

  bit_idx = (obj - (word)context->lowest_heap_address) >> BIT_IDX_SHIFT;
  word_idx = bit_idx >> BITS_TO_WORDS;
  bit = 1 << (bit_idx & BIT_IN_WORD_MASK);
  return (context->bitmap[ word_idx ] & bit);
}

void msgc_mark_range( msgc_context_t *context, void *bot, void *lim )
{
  unsigned bit_idx_lo, word_idx_lo, bit_idx_hi, word_idx_hi;
  byte *first = (byte*)context->lowest_heap_address;
  byte* botp = (byte*)bot, *limp = (byte*)lim;
  
  bit_idx_lo = ((unsigned)(botp - first)) >> BIT_IDX_SHIFT;
  word_idx_lo = bit_idx_lo >> BITS_TO_WORDS;
  bit_idx_hi = ((unsigned)(limp - 1 - first)) >> BIT_IDX_SHIFT;
  word_idx_hi = bit_idx_hi >> BITS_TO_WORDS;
  
  /* First partial word */
  context->bitmap[ word_idx_lo ] |= ~0 << (bit_idx_lo & BIT_IN_WORD_MASK);
  
  /* Middle segment: whole words */
  memset( &context->bitmap[word_idx_lo+1], 
         ~0,
          ((word_idx_hi-1)-(word_idx_lo+1)+1)*sizeof(word) );

  /* Last partial word */
  if ((bit_idx_hi & BIT_IN_WORD_MASK) == BIT_IN_WORD_MASK)
    context->bitmap[ word_idx_hi ] = ~0;
  else
    context->bitmap[ word_idx_hi ] |= 
      ~0 << (bit_idx_hi & BIT_IN_WORD_MASK)+1 ^ ~0;
}

void msgc_mark_object( msgc_context_t *context, word obj )
{
  word bit_idx, word_idx;

  assert2( context->lowest_heap_address <= ptrof( obj ) &&
           ptrof( obj ) < context->highest_heap_address );

  mark_word( context->bitmap, obj, (word)context->lowest_heap_address );
}

void msgc_push_constituents( msgc_context_t *context, word obj )
{
  push_constituents( context, obj );
}

msgc_context_t *msgc_begin( gc_t *gc )
{
  caddr_t lowest, highest;
  int doublewords;
  msgc_context_t *context;

  context = must_malloc( sizeof( msgc_context_t ) );
  context->gc = gc;

  gclib_memory_range( &lowest, &highest );
  context->lowest_heap_address = (word*)lowest;
  context->highest_heap_address = (word*)highest;
  doublewords = roundup(highest-lowest,(2*sizeof(word)))/(2*sizeof(word));
  context->words_in_bitmap = 
    roundup(doublewords,(8*sizeof(word)))/(8*sizeof(word));
  context->bitmap = 
    gclib_alloc_rts( context->words_in_bitmap * sizeof(word), 0 );

  memset( context->bitmap, 0, context->words_in_bitmap*sizeof(word) );
  context->stack.seg = 0;
  context->stack.stkp = 0;
  context->stack.stkbot = 0;
  context->stack.stklim = 0;
  push_segment( &context->stack );
  context->los_stack.seg = 0;
  context->los_stack.stkp = 0;
  context->los_stack.stkbot = 0;
  context->los_stack.stklim = 0;
  push_segment( &context->los_stack );

  return context;
}

void 
msgc_mark_objects_from_roots( msgc_context_t *context, 
                              int *marked, int *traced, int *words_marked )
{
  context->marked = 0;
  context->traced = 0;
  context->words_marked = 0;
  
  gc_enumerate_roots( context->gc, push_root, (void*)context );
  mark_from_stack( context );
    
  *marked += context->marked;
  *traced += context->traced;
  *words_marked += context->words_marked;
}

void msgc_end( msgc_context_t *context )
{
  int n;
  
  n = free_stack( context->los_stack.seg );
  n += free_stack( context->stack.seg );
  if (n > 2)
    consolemsg( "  Warning: deep mark stack: %d elements.", n*STACKSIZE );

  gclib_free( context->bitmap, context->words_in_bitmap*sizeof(word) );
  context->bitmap = 0;
  free( context );
}

/* Unpublished interface */
/* Assert that the bitmap in context is a conservative approximation
   of actual liveness info.
   */
void msgc_assert_conservative_approximation( msgc_context_t *context )
{
  msgc_context_t *ncontext;
  int marked=0, traced=0, words_marked=0, diffs=0;
  word *b1, *b2;
  int i;
  
  ncontext = msgc_begin( context->gc );
  assert( ncontext->words_in_bitmap == context->words_in_bitmap );
  assert( ncontext->lowest_heap_address == context->lowest_heap_address );

  msgc_mark_objects_from_roots( ncontext, &marked, &traced, &words_marked );

  b1 = context->bitmap;
  b2 = ncontext->bitmap;

  for ( i=0 ; i < ncontext->words_in_bitmap ; i++ ) {
    word w1 = b1[i], w2 = b2[i];
    if (w1 != w2) {
      /* Want w1 superset of w2 */
      if ((w1 & w2) != w2) {
        consolemsg( " gen mark failed@%p: conservative=0x%08x accurate=0x%08x",
                    (void*)(ncontext->lowest_heap_address + 
                            (i*8*BITS_IN_WORD)/sizeof(word)), 
                    w1, w2 );
        diffs++;
      }
    }
  }
  assert( diffs == 0 );
  msgc_end( ncontext );
}

/* eof */
