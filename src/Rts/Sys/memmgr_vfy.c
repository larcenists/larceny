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

#include "smircy_checking.h"

#include "stats.h"
#include "uremset_t.h"

#include "memmgr_vfy.h"
#include "memmgr_internal.h"

static bool msvfy_object_marked_p( msgc_context_t *c, word x ) {
  return msgc_object_marked_p( c, x );
}
static void msvfy_set_object_visitor( msgc_context_t *c, 
                                      void* (*visitor)( word obj, 
                                                        word src,
                                                        int offset, 
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

static void* verify_remsets_msgc_fcn( word obj, word src, int offset, void *data ) 
{
  int src_gen, obj_gen;
  gc_t *gc = (gc_t*)data;
  if (!isptr(src))
    return data;
  if (!isptr(obj))
    return data;
 
  src_gen = gen_of(src);
  obj_gen = gen_of(obj);
  if (src_gen < 0 || obj_gen < 0) {
    consolemsg( "verify_remsets_msgc_fcn( "
                "obj=0x%08x (%d), src=0x%08x (%d), data )",
                obj, obj_gen, src, src_gen );
  }
  assert( src_gen >= 0);
  assert( obj_gen >= 0);
  if (((DATA(gc)->remset_undirected && (src_gen != obj_gen))
       || (! DATA(gc)->remset_undirected && (src_gen > obj_gen))) &&
      ! gc_is_nonmoving( gc, obj_gen )) {
    assert( src_gen >= 0 );
    if (src_gen > 0) {
      assert( *gc->ssb[src_gen]->bot == *gc->ssb[src_gen]->top );
      assert( *gc->ssb[obj_gen]->bot == *gc->ssb[obj_gen]->top );
      if (!urs_isremembered( gc->the_remset, src )) {
	consolemsg( " src: 0x%08x (%d) points to obj: 0x%08x (%d),"
		    " but not in remset ",
		    src, src_gen, obj, obj_gen );
	if (! gc_is_address_mapped( gc, ptrof(src), FALSE)) {
          assert( gc_is_address_mapped( gc, ptrof(src), TRUE ));
        }
        if (! gc_is_address_mapped( gc, ptrof(obj), FALSE)) {
          assert( gc_is_address_mapped( gc, ptrof(obj), TRUE ));
        }
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
static bool verify_remsets_traverse_rs( loc_t loc, void *d )
{
  struct verify_remsets_traverse_rs_data *data;
  word *slot;
  data = (struct verify_remsets_traverse_rs_data*)d;
  slot = loc_to_slot(loc);
  if (isptr(*slot)) {
    assert( msvfy_object_marked_p( data->conserv_context, *slot));
  }
  return TRUE;
}
/* verify that (X in R implies X in minor_remset for X);
 * another invariant for R = nursery_remset. */
static bool verify_nursery_traverse_rs( loc_t loc, void *d )
{
  struct verify_remsets_traverse_rs_data *data;
  data = (struct verify_remsets_traverse_rs_data*)d;
  assert( urs_isremembered( data->gc->the_remset, loc_to_obj(loc) ));
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
  FIXME_UNUSED_VARIABLE(data.conserv_context);
  FIXME_UNUSED_VARIABLE(data.gc);
  FIXME_UNUSED_VARIABLE(data.region);
  FIXME_UNUSED_VARIABLE(data.major);
  FIXME_UNUSED_VARIABLE(data.pointsinto);
  msgc_end( context );
}

void verify_nursery_summary_via_oracle( gc_t *gc ) 
{
  msgc_context_t *context;
  int marked, traced, words_marked; 
  struct verify_remsets_traverse_rs_data data;
  context = msgc_begin( gc );
  msvfy_mark_objects_from_roots_and_remsets( context );
  data.conserv_context = context;
  data.gc = gc;
  data.region = 0;
  data.major = FALSE;
  data.pointsinto = TRUE;
  if (DATA(gc)->summaries != NULL) { 
    sm_nursery_summary_enumerate( DATA(gc)->summaries, 
                                  verify_nursery_traverse_rs, &data );
    sm_nursery_summary_enumerate( DATA(gc)->summaries, 
                                  verify_remsets_traverse_rs, &data );
  }
  msgc_end( context ); 
}

void verify_summaries_via_oracle( gc_t *gc ) 
{
  assert(! DATA(gc)->use_summary_instead_of_remsets );
  if (DATA(gc)->summaries != NULL) {
    sm_verify_summaries_via_oracle( DATA(gc)->summaries );
  }
}

void verify_smircy_via_oracle( gc_t *gc ) 
{
  if ((gc)->smircy != NULL) {
    smircy_assert_conservative_approximation( (gc)->smircy );   \
  }
}

struct stop_on_obj_data {
  word obj;
  word src;
};

static bool stop_on_obj( word obj, word src, void *my_data )
{
  struct stop_on_obj_data *data;

  data = (struct stop_on_obj_data*)my_data;
  if ( obj == data->obj ) {
    data->src = src;
    return TRUE;
  } else {
    return FALSE;
  }
}

static bool 
print_path_to( gc_t *gc, word obj, bool via_roots_alone ) 
{
  msgc_context_t *context; 
  word tgt;
  bool new_tgt;
  int ign;
  bool found_tgt;
  struct stop_on_obj_data data;

  found_tgt = FALSE;
  new_tgt = TRUE;
  data.obj = obj;
  data.src = 0x0;

  while (new_tgt) {
    assert( isptr( data.obj ));
    new_tgt = FALSE;
    context = msgc_begin( gc );
    msgc_set_stop_when( context, stop_on_obj, &data );
    if (via_roots_alone) {
      msgc_mark_objects_from_roots( context, &ign, &ign, &ign );
    } else {
      msgc_mark_objects_from_roots_and_remsets( context, &ign, &ign, &ign );
    }
    if (data.src != 0x0) {
      found_tgt = TRUE;
      if (isptr(data.src)) {
        consolemsg( "obj: 0x%08x (%d) <-- src: 0x%08x (%d)", 
                    data.obj, gen_of(data.obj), data.src, gen_of(data.src) );
      } else {
        consolemsg( "obj: 0x%08x (%d) <-- src: 0x%08x     ", 
                    data.obj, gen_of(data.obj), data.src );
      }
      if (isptr(data.src)) {
        new_tgt = TRUE;
        data.obj = data.src;
        data.src = 0x0;
      }
    }
    msgc_end( context );
  }

  return found_tgt;
}

static void check_valid_object( gc_t *gc, word obj, int bad_gno )
{
  bool found_it;
  if ( gen_of(obj) == bad_gno ) {
    consolemsg("invalid object reached: 0x%08x (%d)", obj, gen_of(obj));
    found_it = print_path_to( gc, obj, TRUE );
    if (! found_it) {
      consolemsg("not reachable from roots: 0x%08x (%d)", obj, gen_of(obj));
      found_it = print_path_to( gc, obj, FALSE );
    }
  }
  assert( gen_of(obj) != bad_gno );
}

int verify_fwdfree_via_oracle_gen_no; 
static void* verify_fwdfree_msgc_fcn( word obj, word src, int offset, void *data ) 
{
  gc_t *gc = (gc_t*)data;

  if (isptr(src)) {
    check_valid_object( gc, src, 0 );
    check_valid_object( gc, src, verify_fwdfree_via_oracle_gen_no );
  }
  if (isptr(obj)) {
    check_valid_object( gc, obj, 0 );
    check_valid_object( gc, obj, verify_fwdfree_via_oracle_gen_no );
  }
  return data;
}

void verify_fwdfree_via_oracle( gc_t * gc ) 
{
  msgc_context_t *context;

  context = msgc_begin( gc );
  msvfy_set_object_visitor( context, verify_fwdfree_msgc_fcn, gc );
  msvfy_mark_objects_from_roots( context );
  msgc_end( context );

  context = msgc_begin( gc );
  msvfy_set_object_visitor( context, verify_fwdfree_msgc_fcn, gc );
  msvfy_mark_objects_from_roots_and_remsets( context );
  msgc_end( context );
}
