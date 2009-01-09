/* Copyright 2008 Felix S Klock II.          -*- indent-tabs-mode: nil -*-
 *
 * $Id: memmgr_vfy.c 5868 2008-12-18 23:33:21Z pnkfelix $
 *
 * Larceny  -- precise garbage collector, rep inv checking.
 */

#define GC_INTERNAL

#include "larceny.h"
#include "gc.h"
#include "gc_t.h"
#include "gclib.h"
#include "msgc-core.h"
#include "summary_t.h"
#include "summ_matrix_t.h"
#include "seqbuf_t.h"

#include "memmgr_internal.h"

static bool msvfy_object_marked_p( msgc_context_t *c, word x ) {
  return msgc_object_marked_p( c, x );
}
static void msvfy_set_object_visitor( msgc_context_t *c, 
                                      void* (*visitor)( word obj, 
                                                        word src,
                                                        void *data ), 
                                      void *data ) {
  msgc_set_object_visitor( c, visitor, data );
}
static void msvfy_mark_objects_from_roots( msgc_context_t *c ) {
  int marked, traced, words_marked;
  msgc_mark_objects_from_roots( c, &marked, &traced, &words_marked );
}
static void msvfy_mark_objects_from_roots_and_remsets( msgc_context_t *c ) {
  int m, t, wm;
  msgc_mark_objects_from_roots_and_remsets( c, &m, &t, &wm );
}

static void* verify_remsets_msgc_fcn( word obj, word src, void *data ) 
{
  gc_t *gc = (gc_t*)data;
  if (isptr(src) && isptr(obj) &&
      gen_of(src) != gen_of(obj) &&
      ! gc_is_nonmoving( gc, gen_of(obj) )) {
    assert( gen_of(src) >= 0 );
    if (gen_of(src) > 0) {
      assert( *gc->ssb[gen_of(src)]->bot == *gc->ssb[gen_of(src)]->top );
      assert( *gc->ssb[gen_of(obj)]->bot == *gc->ssb[gen_of(obj)]->top );
      if (gen_of(obj) == 0) {
        assert( sm_nursery_summary_contains( DATA(gc)->summaries, src ));
      }
      if (!rs_isremembered( gc->remset[ gen_of(src) ], src ) &&
	  !rs_isremembered( gc->major_remset[ gen_of(src) ], src )) {
	consolemsg( " src: 0x%08x (%d) points to obj: 0x%08x (%d),"
		    " but not in remsets @0x%08x @0x%08x",
		    src, gen_of(src), obj, gen_of(obj), 
		    gc->remset[ gen_of(src) ],
		    gc->major_remset[ gen_of(src) ]);
	assert( gc_is_address_mapped( gc, ptrof(src), TRUE ));
	assert( gc_is_address_mapped( gc, ptrof(obj), TRUE ));
	assert(0);
      }
    }
  }
  return data;
}

struct verify_remsets_traverse_rs_data {
  msgc_context_t *conserv_context;
  gc_t *gc;
  int region;
  bool major;
  bool pointsinto;
};
/* verify that (X in remset R implies X in reachable(roots+remsets));
 * (may be silly to check, except when R = nursery_remset...) */
static bool verify_remsets_traverse_rs( word obj, void *d, unsigned *stats )
{
  struct verify_remsets_traverse_rs_data *data;
  data = (struct verify_remsets_traverse_rs_data*)d;
  assert( msvfy_object_marked_p( data->conserv_context, obj ));
  return TRUE;
}
/* verify that (X in R implies X in minor_remset for X);
 * another invariant for R = nursery_remset. */
static bool verify_nursery_traverse_rs( word obj, void *d, unsigned *stats )
{
  struct verify_remsets_traverse_rs_data *data;
  data = (struct verify_remsets_traverse_rs_data*)d;
  assert( rs_isremembered( data->gc->remset[ gen_of(obj) ], obj ));
  return TRUE;
}

void verify_remsets_via_oracle( gc_t *gc ) 
{
  msgc_context_t *context;
  int marked, traced, words_marked; 
  struct verify_remsets_traverse_rs_data data;
  context = msgc_begin( gc );
  msvfy_set_object_visitor( context, verify_remsets_msgc_fcn, gc );
  msvfy_mark_objects_from_roots( context );
  msgc_end( context );
  context = msgc_begin( gc );
  msvfy_set_object_visitor( context, verify_remsets_msgc_fcn, gc );
  msvfy_mark_objects_from_roots_and_remsets( context );
  data.conserv_context = context;
  data.gc = gc;
  data.region = 0;
  data.major = FALSE;
  data.pointsinto = TRUE;
  sm_nursery_summary_enumerate( DATA(gc)->summaries, verify_nursery_traverse_rs, &data );
  sm_nursery_summary_enumerate( DATA(gc)->summaries, verify_remsets_traverse_rs, &data );
  /* Originally had code to verify_remsets_traverse_rs on all remsets,
   * but that does not seem like an interesting invariant to check. */
  msgc_end( context );
}

void verify_summaries_via_oracle( gc_t *gc ) 
{
  assert(! DATA(gc)->use_summary_instead_of_remsets );
  sm_verify_summaries_via_oracle( DATA(gc)->summaries );
}

