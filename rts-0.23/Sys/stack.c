/* 
 * This is the file Sys/stack.c.
 *
 * Larceny run-time system (Unix) -- stack handling
 *
 * History
 *   June 28 - July 1, 1994 / lth (v0.20)
 *     Moved here from memsupport.c, massively rewritten.
 *
 * The stack lives at the high end of the current ephemeral area. There
 * are three major advantages to this:
 * - the stack pointer is the heap limit, and vice versa, saving registers.
 * - the stack can be flushed in-place, which is typically much faster than
 *   copying it.
 * - the stack can grow more than would be reasonable with a fixed-size
 *   stack cache.
 * There are also a couple of disadvantages over a separate stack:
 * - multiple stacks is not really possible (although call/cc is cheap,
 *   so it is less of an issue).
 * - the user cannot really adjust the stack cache and the heap separately.
 * - cache locality in the presence of call/cc may be poorer.
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
 * The frames are always double-word aligned, but the mutator need not 
 * initialize the pad word. The size field should reflect the actual size
 * of the frame, not including the size field. A minimum frame occupies
 * four words, and the size field will contain the value '12' (the fixnum 3).
 * The return address field is a raw pointer in the stack; in the heap it
 * is the byte offset from byte 0 of the code vector of the procedure in
 * the slot for saved R0.
 */

#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

/* 
 * Create a new, empty stack cache at the high end of the ephemeral
 * area. 
 *
 * Return 1 if the creation succeeded; 0 if the heap is full and a gc
 * must be performed.
 */
int create_stack()
{
  extern void mem_stkuflow();
  word *stktop;

  stktop = (word*)globals[ G_STKP ];
  stktop -= 16;
  if (stktop < (word*)globals[ G_ETOP ]) return 0;
  
  *(stktop+0) = 12;                             /* header/size field */
  *(stktop+1) = (word)mem_stkuflow;             /* retaddr: uflow handler */
  *(stktop+2) = 0xDEADBEEF;                     /* dynamic link field */
  *(stktop+3) = 0;                              /* saved procedure */

  globals[ G_STKP ] = (word)stktop;
  globals[ G_STKBOT ] = (word)stktop;
  return 1;
}

/* Clear the stack cache */
void clear_stack()
{
  globals[ G_STKP ] = globals[ G_STKBOT ];
}

/*
 * Flush the stack cache to the heap by walking it, converting
 * stack frames to heap frames.
 */
void flush_stack()
{
  word *stktop, *stkbot, *first, *prev;
  word retaddr, codeaddr, proc, size;
  unsigned framecount;

  stktop = (word*)globals[ G_STKP ];
  stkbot = (word*)globals[ G_STKBOT ];

  first = prev = 0;  
  framecount = 0;
  while (stktop < stkbot) {
    /* convert header to vector header */
    size = *stktop;
#ifdef DEBUG2
    consolemsg( "[debug] frame = %08lx words, retaddr = %08lx\n",
	        size / 4,
	        *(stktop+1) );
#endif
    *stktop = mkheader( size, VEC_HDR );

    /* convert return address */
    proc = *(stktop+3);
    if (proc != 0) {
      retaddr = *(stktop+1);
      codeaddr = (word)ptrof( *(ptrof( proc )+PROC_CODEPTR) );
      *(stktop+1) = retaddr-(codeaddr+4);
    }

    /* chain things together */
    if (first == 0)
      first = stktop;
    else
      *(prev+2) = (word)tagptr( stktop, VEC_TAG );
    prev = stktop;

    framecount++;

    size = roundup8( size+4 );
    stktop += size / 4;
  }
  if (prev != 0)
    *(prev+2) = globals[ G_CONT ];
  if (first != 0)
    globals[ G_CONT ] = (word)tagptr( first, VEC_TAG );

  globals[ G_STKBOT ] = globals[ G_STKP ];

  memstat_framesflushed( framecount );
}

/*
 * Restore one stack frame from the heap, assuming that the stack pointer
 * currently points to a frame created by create_stack().
 *
 * Returns 1 if the frame could be restored, 0 if the heap is full.
 *
 * A copy of this code exists in Sparc/memory.s; if you change anything
 * here, check that code as well.
 */
int restore_frame()
{
  word *stktop, *hframe, *p;
  word retoffs, proc, codeaddr;
  unsigned size;

#ifdef DEBUG
  if (globals[ G_STKP ] != globals[ G_STKBOT ])
    panic( "internal error in restore_frame()" );
#endif

  hframe = ptrof( globals[ G_CONT ] );
  size = roundup8( sizefield( *hframe ) + 4 );   /* bytes to copy */
  stktop = (word*)globals[ G_STKP ];

  stktop -= size / 4;
  if (stktop < (word*)globals[ G_ETOP ]) return 0;
  globals[ G_STKP ] = (word)stktop;

  /* copy the frame onto the stack */
  p = stktop;
  while (size) {
    *p++ = *hframe++;
    *p++ = *hframe++;
    size -= 8;
  }

  /* Follow continuation chain. */
  globals[ G_CONT ] = *(stktop+2);

  /* convert the header back to a fixnum */
  *stktop = sizefield( *stktop );

  /* convert the return address */
  proc = *(stktop+3);
  if (proc != 0) {
    retoffs = *(stktop+1);
    codeaddr = (word)ptrof( *(ptrof( proc )+PROC_CODEPTR) );
    *(stktop+1) = (codeaddr+4)+retoffs;
  }

  return 1;
}

/* eof */
