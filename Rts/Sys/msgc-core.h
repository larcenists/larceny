/* Copyright 1999 Lars T Hansen
 *
 * $Id$
 *
 * Mark/Sweep garbage collection core functionality.
 */

#include "larceny-types.h"

typedef struct msgc_context msgc_context_t;

extern msgc_context_t *msgc_begin( gc_t *gc );
  /* Create a mark-sweep GC context for the given collector and
     return the context.
     */

extern void msgc_mark_from_roots( msgc_context_t *context );
  /* Mark the entire heap from roots.
     */

extern bool msgc_object_marked_p( msgc_context_t *context, word obj );
  /* OBJ must be a tagged pointer to an object (note: _not_ to an
     arbitrary address) in the heap.  Returns TRUE iff the object
     is marked in the bitmap.
     */
     
extern void msgc_end( msgc_context_t *context );
  /* Free the context data structure and any resources it uses.
     */

/* eof */
