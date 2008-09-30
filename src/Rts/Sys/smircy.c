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
 */

#define GC_INTERNAL

#include "larceny.h"
#include "gc_t.h"
#include "gclib.h"
#include "smircy.h"

/* XXX To get something working, layering this on top of msgc for now.
 * Its obviously the wrong thing and should never be checked in (or
 * admitted to).
 */
#include "msgc-core.h"

struct smircy_context {
  gc_t *gc;
  msgc_context_t *msgc;
};

smircy_context_t *smircy_begin( gc_t *gc ) 
{
  smircy_context_t *context;
  context = must_malloc( sizeof( smircy_context_t ) );

  context->gc = gc;
  context->msgc = msgc_begin( gc );

  return context;
}

static void push_root( word *loc, void *data ) 
{
  smircy_context_t *context = (smircy_context_t*) data;
  msgc_push_object( context->msgc, *loc );
}

void smircy_push_roots( smircy_context_t *context )
{
  gc_enumerate_roots( context->gc, push_root, (void*)context );
}

static bool push_remset_entry( word loc, void *data, unsigned *stats ) 
{
  smircy_context_t *context = (smircy_context_t*)data;
  msgc_push_object( context->msgc, loc );
  return TRUE;
}

void smircy_push_remset( smircy_context_t *context, remset_t *rs ) 
{
  rs_enumerate( rs, push_remset_entry, context );
}

void smircy_progress( smircy_context_t *context, 
                      int mark_max, int trace_max, int mark_words_max,
                      int *marked, int *traced, int *words_marked )
{
  assert( mark_max == -1);
  assert( trace_max == -1);
  assert( mark_words_max == -1);
  msgc_stack_pops( context->msgc, marked, traced, words_marked );
}

bool smircy_object_marked_p( smircy_context_t *context, word obj )
{
  return msgc_object_marked_p( context->msgc, obj );
}

void smircy_end( smircy_context_t *context )
{
  msgc_end( context->msgc );
  free( context );
}

void smircy_set_object_visitor( smircy_context_t *context, 
                                void* (*visitor)( word obj, 
                                                  word src, 
                                                  void *data ),
                                void *visit_data )
{
  assert(0);
}

void* smircy_get_object_visitor_data( smircy_context_t *context )
{
  assert(0);
}

/* eof */
