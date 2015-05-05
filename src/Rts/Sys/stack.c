/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system (Unix) -- stack handling
 *
 * FIXME: the comments and code in this file assume 32-bit words
 *
 * The stack lives at the high end of the current ephemeral area. There
 * are three major advantages to this:
 * - the stack pointer is the heap limit, and vice versa, saving registers.
 * - the stack can be flushed in-place, which is typically much faster than
 *   copying it.
 * - the stack can grow more than would be reasonable with a fixed-size
 *   stack cache.
 *
 * There are also a couple of disadvantages over a separate stack:
 * - multiple stacks is not really possible (although call/cc is cheap,
 *   so it is less of an issue).
 * - the user cannot really adjust the stack cache and the heap separately.
 * - cache locality in the presence of call/cc may be poorer.
 * - the stack cache must be flushed to the heap on a gc.
 *
 * Frame layout:
 * Frames look almost the same in the stack and in the heap; in particular
 * they have the same size and a similar layout.  On some platforms, a
 * stack frame looks like this:
 *
 *   0:  header/size        In heap, a vector header. In stack, a fixnum.
 *   1:  return address     In heap, a fixnum. In stack, an address.
 *   2:  dynamic link       In heap, a vector. In stack, garbage.
 *   3:  saved REG0         Identical
 *   ....
 *
 * On other platforms (notably IAssassin and Fence/ARM), a stack frame
 * looks like this:
 *
 *   0:  header/return      In heap, a vector header. In stack, an address.
 *   1:  offset/size        In heap, a fixnum. In stack, a fixnum.
 *   2:  dynamic link       In heap, a vector. In stack, garbage.
 *   3:  saved REG0         Identical
 *   ....
 *
 * Because the layout varies slightly, all code must refer to the first
 * few slots using appropriate symbolic constants; which constant should
 * be used depends upon whether the frame resides in the stack or heap.
 *
 * When a continuation frame resides within the stack:
 *     STK_CONTSIZE         refers to the header/size or offset/size field
 *     STK_RETADDR          refers to the return address or header/return field
 *     STK_DYNLINK          refers to the dynamic link field
 *     STK_PROC             refers to the saved REG0 field
 *
 * When a continuation frame resides within the heap:
 *     HC_HEADER            refers to the header/size or header/return field
 *     HC_RETOFFSET         refers to the return address or offset/size field
 *     HC_DYNLINK           refers to the dynamic link field
 *     HC_PROC              refers to the saved REG0 field
 *
 * STK_REG0 and STK_SAVED are synonyms for STK_PROC.
 * HC_SAVED is a synonym for HC_PROC.
 *
 * The values of those constants are defined by layouts.cfg.
 *
 * The frames are always 8-byte aligned.  On 32-bit platforms, that means
 * one 4-byte pad word may follow the frame, but the mutator does not
 * need to initialize the pad word. The size field should reflect the
 * actual size of the frame, not including the size field or pad word.
 * A minimal frame occupies four words, and the size field will contain
 * the value '12' (the fixnum 3). In the stack, return address fields
 * are raw machine pointers to code; when copied to the heap, a return
 * address field is the byte offset from byte 0 of the code vector
 * for the procedure in the slot for saved REG0.  Special case:  If the
 * slot for saved REG0 is zero, then the return address field contains
 * a raw machine pointer into code that cannot be relocated by garbage
 * collections.
 */

#define GC_INTERNAL

#include "larceny.h"
#include "stack.h"
#include "stats.h"

static struct {
  int stacks_created;
  int frames_flushed;
  int words_flushed;
} stack_state;                        /* FIXME: hang off GC or globals */

#define STACK_BASE_SIZE    16   /* bytes */

/* Allocates and initializes a stack underflow frame. */

int stk_create( word *globals )
{
  word *stktop;

  assert(    globals[G_STKP] - SCE_BUFFER >= globals[ G_ETOP ]
          && globals[G_STKP] <= globals[ G_ELIM ] );

  stktop = (word*)globals[ G_STKP ];
  stktop -= 4;
  if (stktop < (word*)globals[ G_ETOP ]) {
    supremely_annoyingmsg( "Failed to create stack.");
    return 0;
  }
  
  *(stktop+STK_CONTSIZE) = fixnum(3);           /* header/size field */
  *(stktop+STK_RETADDR)  = 0xDEADBEEF;          /* retaddr: uflow handler */
  *(stktop+STK_DYNLINK) = 0xDEADBEEF;           /* dynamic link field */
  *(stktop+STK_PROC) = 0xDEADBEEF;              /* saved procedure */
  stk_initialize_underflow_frame( stktop );     /* In client space */

  globals[ G_STKP ] = (word)stktop;
  globals[ G_STKBOT ] = (word)stktop;

  stack_state.stacks_created += 1;
  return 1;
}

void stk_clear( word *globals )
{
  globals[ G_STKP ] = globals[ G_STKBOT ];
}

/* Flushes stack frames into the heap. */

void stk_flush( word *globals )
{
  word *stktop, *stkbot, *first, *prev;
  word retaddr, codeaddr, codeptr, proc, size;
  unsigned framecount;

  assert2( tagof( globals[ G_REG0 ]) == PROC_TAG );

  stktop = (word*)globals[ G_STKP ];
  stkbot = (word*)globals[ G_STKBOT ];

  stack_state.words_flushed += (stkbot-stktop);
  first = prev = 0;  
  framecount = 0;
  while (stktop < stkbot) {
    size = *(stktop+STK_CONTSIZE);
    retaddr = *(stktop+STK_RETADDR);

    /* convert header to vector header */
    assert2( size % 4 == 0 );            /* size must be words, a fixnum */
    assert2( (s_word)size >= 12 );    /* 3-word minimum, and nonnegative */
    *(stktop+HC_HEADER) = mkheader( size, VEC_HDR );

    /* convert return address */
    proc = *(stktop+STK_REG0);
    if (proc != 0) {
      assert2( tagof( proc ) == PROC_TAG );
      codeptr = *(ptrof( proc )+PROC_CODEPTR);
      if (tagof( codeptr ) == BVEC_TAG) {
        codeaddr = (word)ptrof( codeptr );
        *(stktop+HC_RETOFFSET) = retaddr-(codeaddr+4);
      } else {
        *(stktop+HC_RETOFFSET) = retaddr;
      }
    } else {
      *(stktop+HC_RETOFFSET) = retaddr;
    }

    /* chain things together */
    if (first == 0)
      first = stktop;
    else
      *(prev+HC_DYNLINK) = (word)tagptr( stktop, VEC_TAG );
    prev = stktop;

    framecount++;

    size = roundup8( size+4 );
    stktop += size / 4;

#if 0
    annoyingmsg("Flush: %d", size );
#endif
  }
  if (prev != 0)
    *(prev+HC_DYNLINK) = globals[ G_CONT ];
  if (first != 0)
    globals[ G_CONT ] = (word)tagptr( first, VEC_TAG );

  globals[ G_STKBOT ] = globals[ G_STKP ];    /* FIXME: seems backwards */

  stack_state.frames_flushed += framecount;
}

/* NOTE:  A copy of this code exists in Sparc/memory.s; if you change 
 * anything here, check that code as well.
 */
int stk_restore_frame( word *globals )
{
  word *stktop, *hframe, *p;
  word retoffs, proc, codeaddr, codeptr, header;
  unsigned size;

  assert2(globals[ G_STKP ] == globals[ G_STKBOT ]);

  hframe = ptrof( globals[ G_CONT ] );
  size = roundup8( sizefield( *hframe ) + 4 );   /* bytes to copy */
  stktop = (word*)globals[ G_STKP ];

  stktop -= size / 4;
  if (stktop < (word*)globals[ G_ETOP ]) {
    supremely_annoyingmsg( "Failed to create stack." );
    return 0;
  }
  globals[ G_STKP ] = (word)stktop;
  globals[ G_STKUFLOW ] += 1;

#if 0
  annoyingmsg("Restore: %d", size);
#endif

  /* copy the frame onto the stack */
  p = stktop;
  while (size) {
    *p++ = *hframe++;
    *p++ = *hframe++;
    size -= 8;
  }

  /* Follow continuation chain. */
  globals[ G_CONT ] = *(stktop+STK_DYNLINK);

  header  = *(stktop+HC_HEADER);
  retoffs = *(stktop+HC_RETOFFSET);
  proc    = *(stktop+HC_PROC);

  /* convert the header back to a fixnum */
  *(stktop+STK_CONTSIZE) = sizefield(header);

  /* convert the return address */
  if (proc != 0) {
    codeptr = *(ptrof( proc )+PROC_CODEPTR);
    if (tagof( codeptr ) == BVEC_TAG) {
      codeaddr = (word)ptrof( codeptr );
      *(stktop+STK_RETADDR) = (codeaddr+4)+retoffs;
    } else {
      *(stktop+STK_RETADDR) = retoffs;
    }
  } else {
    *(stktop+STK_RETADDR) = retoffs;
  }

  return 1;
}

int stk_size_for_top_stack_frame( word *globals )
{
#if OLD_GC_CODE
  return
    nativeint( *(word*)globals[ G_STKP ] )*sizeof( word ) + STACK_BASE_SIZE;
#else
  int frame_size;
  if (globals[ G_STKP ] == globals[ G_STKBOT])
    frame_size = sizefield( *ptrof( globals[ G_CONT ] ) );
  else
    frame_size = *((word*)globals[ G_STKP ] + STK_CONTSIZE);
  return roundup8( frame_size + 4 ) + STACK_BASE_SIZE;
#endif

}

void stk_stats( word *globals, stack_stats_t *stats )
{
  stats->stacks_created = stack_state.stacks_created;
  stats->frames_flushed = stack_state.frames_flushed;
  stats->words_flushed = stack_state.words_flushed;
  stats->frames_restored = globals[ G_STKUFLOW ];

  stack_state.stacks_created = 0;
  stack_state.frames_flushed = 0;
  stack_state.words_flushed = 0;
  globals[ G_STKUFLOW ] = 0;
}

/* eof */
