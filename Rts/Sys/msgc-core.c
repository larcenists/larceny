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

#if defined(BITS_32)
# define BIT_IDX_SHIFT     3    /* shift to get doubleword bit address */
# define BITS_2_WORDS      5    /* shift to get word addr from bit addr */
# define BIT_IN_WORD_MASK 31    /* mask to get bit shift */
#else
# error "Must define MSGC macros for non-32 bit systems."
#endif

typedef struct msgc_stack msgc_stack_t;

struct msgc_context {
  gc_t         *gc;
  word         *lowest_heap_address;
  word         *highest_heap_address;
  int          words_in_bitmap;
  word         *bitmap;
  msgc_stack_t *stack;          /* current segment */
  word         *stkp;
  word         *stkbot;
  word         *stklim;
  int          traced;
  int          marked;
};

#define STACKSIZE  32760        /* Should be large. */

struct msgc_stack {
  word         stack[STACKSIZE];
  msgc_stack_t *prev;           /* Previous segment (older) */
  msgc_stack_t *next;           /* Next segment (younger) */
};

static void push_segment( msgc_context_t *context )
{
  msgc_stack_t *sp;
  
  if (context->stack != 0 && context->stack->next != 0)
    sp = context->stack->next;
  else {
    sp = must_malloc( sizeof(msgc_stack_t) );
    sp->prev = context->stack;
    if (context->stack != 0) context->stack->next = sp;
    sp->next = 0;
  }
  
  context->stack = sp;
  context->stkbot = context->stack->stack;
  context->stklim = context->stack->stack+STACKSIZE;
  context->stkp = context->stkbot;
}

static bool pop_segment( msgc_context_t *context )
{
  if (context->stack->prev == 0) return FALSE;
  
  context->stack = context->stack->prev;
  context->stkbot = context->stack->stack;
  context->stklim = context->stack->stack+STACKSIZE;
  context->stkp = context->stklim;
  
  return TRUE;
}

static int free_stack( msgc_stack_t *stack )
{
  int n = 0;

  if (stack != 0) {
    n = 1 + free_stack( stack->next );
    free( stack );
  }
  return n;
}

#define PUSH( context, obj )                            \
  do { word TMP = obj;                                  \
       if (isptr(TMP)) {                                \
         if ((context)->stkp == (context)->stklim)      \
           push_segment( context );                     \
         *((context)->stkp++) = TMP;                    \
       }                                                \
  } while(0)
  
static void mark_from_stack( msgc_context_t *context )
{
  word w, bit_idx, word_idx, bit;
  word first = (word)context->lowest_heap_address;
  word *bitmap = context->bitmap;
  int i, n, traced=0, marked=0;

  while (1) {
    /* Pop */
    if (context->stkp == context->stkbot)  /* Stack underflow */
      if (!pop_segment( context )) break;  /* No more work */
    w = *--context->stkp;
    traced++;

    /* Mark */
    /* Note: marks object only, not entire address range occupied
       by object.  A "real" collector must mark the range.
       */
    bit_idx = (w - first) >> BIT_IDX_SHIFT;
    word_idx = bit_idx >> BITS_2_WORDS;
    bit = 1 << (bit_idx & BIT_IN_WORD_MASK);
    if (bitmap[ word_idx ] & bit) continue; /* Already marked */
    bitmap[ word_idx ] |= bit;
    marked++;

    /* Process contents */
    switch (tagof(w)) {
    case PAIR_TAG :
      PUSH( context, pair_cdr( w ) ); /* Do the CDR last */
      PUSH( context, pair_car( w ) ); /* Do the CAR first */
      break;
    case VEC_TAG :                    /* FIXME: super bad for long vectors */
    case PROC_TAG :
      n = sizefield( *ptrof(w) ) / sizeof(word);
      for ( i=0 ; i < n ; i++ )
        PUSH( context, vector_ref( w, i ) );
      break;
    }
  }
  context->traced += traced;
  context->marked += marked;
}

static void push_root( word *loc, void *data )
{
  PUSH( (msgc_context_t*)data, *loc );
}

bool msgc_object_marked_p( msgc_context_t *context, word obj )
{
  word bit_idx, word_idx, bit;

  assert2( isptr( obj ) );
  assert2( context->lowest_heap_address <= ptrof( obj ) &&
           ptrof( obj ) < context->highest_heap_address );

  bit_idx = (obj - (word)context->lowest_heap_address) >> BIT_IDX_SHIFT;
  word_idx = bit_idx >> BITS_2_WORDS;
  bit = 1 << (bit_idx & BIT_IN_WORD_MASK);
  return (context->bitmap[ word_idx ] & bit);
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
  context->bitmap = must_malloc( context->words_in_bitmap * sizeof( word ) );

  return context;
}

void 
msgc_mark_objects_from_roots(msgc_context_t *context, int *marked, int *traced)
{
  int n;

  memset( context->bitmap, 0, context->words_in_bitmap*sizeof( word ) );
  context->stack = 0;
  context->stkp = 0;
  context->stkbot = 0;
  context->stklim = 0;
  push_segment( context );
  context->marked = 0;
  context->traced = 0;

  gc_enumerate_roots( context->gc, push_root, (void*)context );
  mark_from_stack( context );

  n = free_stack( context->stack );
  if (n > 1)
    hardconsolemsg( "  Warning: deep mark stack: %d elements.", n*STACKSIZE );
  *marked = context->marked;
  *traced = context->traced;
}

void msgc_end( msgc_context_t *context )
{
  free( context->bitmap );
  context->bitmap = 0;
  free( context );
}

/* eof */
