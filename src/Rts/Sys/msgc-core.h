/* Copyright 1999 Lars T Hansen           -*- indent-tabs-mode: nil -*-
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

     No heap memory may be allocated between the call to msgc_begin()
     and the call to msgc_mark_objects_from_roots() because the 
     former computes the heap size and allocates data structures 
     for marking.
     */

extern void msgc_mark_objects_from_roots( msgc_context_t *context, 
                                          int *marked, int *traced,
                                          int *words_marked );
  /* Mark the entire heap from roots, marking only objects (not interior
     locations).  Adds the number of objects marked to *marked, number of
     words marked to *words_marked, and number of pointers traced to *traced.
     */

extern void msgc_mark_objects_from_roots_and_remsets
                                        ( msgc_context_t *context, 
                                          int *marked, int *traced,
                                          int *words_marked );
  /* Mark the entire heap from roots and all remsets, marking only objects
     (not interior locations).  
     Adds the number of objects marked to *marked, number of words marked 
     to *words_marked, and number of pointers traced to *traced.
     */


extern void msgc_mark_range( msgc_context_t *context, void *bot, void *lim );
  /* Mark all words in the range [bot,lim) as reachable.
     */

extern void msgc_mark_object( msgc_context_t *context, word obj );
  /* Mark the object referenced by the tagged _or untagged_ pointer OBJ.
     */

extern void msgc_push_constituents( msgc_context_t *context, word obj );
  /* OBJ must be a taged pointer to an object in the heap.  All
     pointer members of the objects are pushed on the mark stack, 
     resulting in them being used as roots for the next mark phase.
     */
     
extern bool msgc_object_marked_p( msgc_context_t *context, word obj );
  /* OBJ must be a tagged pointer to an object (note: _not_ to an
     arbitrary address) in the heap.  Returns TRUE iff the object
     is marked in the bitmap.
     */
     
extern void msgc_end( msgc_context_t *context );
  /* Free the context data structure and any resources it uses.
     */

extern void msgc_assert_conservative_approximation( msgc_context_t *context );
  /* Check that the mark bitmap in the context is a conservative
     approximation of the actual reachability-based bitmap, and
     trigger an assertion if not.
     
     This operation is not cheap.
     */

extern void msgc_set_object_visitor( msgc_context_t *context, 
                                     void* (*visitor)( word obj, 
                                                       word src, 
                                                       void *data ),
                                     void *visit_data );

extern void* msgc_get_object_visitor_data( msgc_context_t *context );

/* eof */
