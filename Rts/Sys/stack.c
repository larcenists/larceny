/* Rts/Sys/stack.c.
 * Larceny run-time system (Unix) -- stack handling
 *
 * $Id$
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
 * they have the same size and layout:
 *
 *   0:  header/size        In heap, a vector header. In stack, a fixnum.
 *   1:  return address     In heap, a fixnum. In stack, an address.
 *   2:  dynamic link       In heap, to previous frame. In stack, garbage.
 *   3:  saved R0           Identical
 *   ....
 *
 * The frames are always double-word aligned, but the mutator needs not 
 * initialize the pad word. The size field should reflect the actual size
 * of the frame, not including the size field or the pad word.  A minimum
 * frame occupies four words, and the size field will contain the value 
 * '12' (the fixnum 3). The return address field is a raw pointer in the
 * stack; in the heap it is the byte offset from byte 0 of the code vector
 * of the procedure in the slot for saved R0.
 */

#define GC_INTERNAL

#include "larceny.h"
#include "stack.h"

#define STACK_BASE_SIZE    16   /* bytes */

int stk_create( word *globals )
{
  extern void mem_stkuflow();
  word *stktop;

  assert(    globals[G_STKP] >= globals[ G_ETOP ]
	  && globals[G_STKP] <= globals[ G_ELIM ] );

  stktop = (word*)globals[ G_STKP ];
  stktop -= 4;
  if (stktop < (word*)globals[ G_ETOP ]) {
    supremely_annoyingmsg( "Failed to create stack.");
    return 0;
  }
  
  *(stktop+0) = fixnum(3);                      /* header/size field */
  *(stktop+1) = (word)mem_stkuflow;             /* retaddr: uflow handler */
  *(stktop+2) = 0xDEADBEEF;                     /* dynamic link field */
  *(stktop+3) = 0;                              /* saved procedure */

  globals[ G_STKP ] = (word)stktop;
  globals[ G_STKBOT ] = (word)stktop;

  return 1;
}

void stk_clear( word *globals )
{
  globals[ G_STKP ] = globals[ G_STKBOT ];
}

void
stk_flush( word *globals, unsigned *frames_flushed, unsigned *bytes_flushed )
{
  word *stktop, *stkbot, *first, *prev;
  word retaddr, codeaddr, proc, size;
  unsigned framecount;

  stktop = (word*)globals[ G_STKP ];
  stkbot = (word*)globals[ G_STKBOT ];

  *bytes_flushed = (stkbot-stktop)*sizeof(word);
  first = prev = 0;  
  framecount = 0;
  while (stktop < stkbot) {
    /* convert header to vector header */
    size = *stktop;
    assert( size % 4 == 0 );	  /* size must be words, a fixnum */
    assert( (s_word)size >= 12 ); /* 3-word minimum, and nonnegative */
#if 0
    debug2msg( "[debug] frame = %08lx words, retaddr = %08lx\n", size / 4,
	       *(stktop+1) );
#endif
    *stktop = mkheader( size, VEC_HDR );

    /* convert return address */
    proc = *(stktop+STK_REG0);
    if (proc != 0) {
      retaddr = *(stktop+STK_RETADDR);
      codeaddr = (word)ptrof( *(ptrof( proc )+PROC_CODEPTR) );
      *(stktop+STK_RETADDR) = retaddr-(codeaddr+4);
    }

    /* chain things together */
    if (first == 0)
      first = stktop;
    else
      *(prev+STK_DYNLINK) = (word)tagptr( stktop, VEC_TAG );
    prev = stktop;

    framecount++;

    size = roundup8( size+4 );
    stktop += size / 4;
  }
  if (prev != 0)
    *(prev+STK_DYNLINK) = globals[ G_CONT ];
  if (first != 0)
    globals[ G_CONT ] = (word)tagptr( first, VEC_TAG );

  globals[ G_STKBOT ] = globals[ G_STKP ];

  *frames_flushed = framecount;
}

/* NOTE:  A copy of this code exists in Sparc/memory.s; if you change 
 * anything here, check that code as well.
 */
int stk_restore_frame( word *globals )
{
  word *stktop, *hframe, *p;
  word retoffs, proc, codeaddr;
  unsigned size;

  assert(globals[ G_STKP ] == globals[ G_STKBOT ]);

  hframe = ptrof( globals[ G_CONT ] );
  size = roundup8( sizefield( *hframe ) + 4 );   /* bytes to copy */
  stktop = (word*)globals[ G_STKP ];

  stktop -= size / 4;
  if (stktop < (word*)globals[ G_ETOP ]) {
    supremely_annoyingmsg( "Failed to create stack." );
    return 0;
  }
  globals[ G_STKP ] = (word)stktop;

#if STACK_UNDERFLOW_COUNTING
  globals[ G_STKUFLOW ] += 1;
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

  /* convert the header back to a fixnum */
  *stktop = sizefield( *stktop );

  /* convert the return address */
  proc = *(stktop+STK_REG0);
  if (proc != 0) {
    retoffs = *(stktop+STK_RETADDR);
    codeaddr = (word)ptrof( *(ptrof( proc )+PROC_CODEPTR) );
    *(stktop+STK_RETADDR) = (codeaddr+4)+retoffs;
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
    frame_size = *(word*)globals[ G_STKP ];
  return roundup8( frame_size + 4 ) + STACK_BASE_SIZE;
#endif

}

/* eof */
