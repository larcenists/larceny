/* Copyright 2008 Felix S Klock II.          -*- indent-tabs-mode: nil -*-
 *
 * $Id: memmgr_flt.c 5868 2008-12-18 23:33:21Z pnkfelix $
 *
 * Larceny  -- precise garbage collector, float measurement.
 */

#define GC_INTERNAL

#include "larceny.h"
#include "gc.h"
#include "gc_t.h"
#include "old_heap_t.h"
#include "remset_t.h"
#include "gclib.h"
#include "msgc-core.h"
#include "summary_t.h"

#include "memmgr_internal.h"

static bool msfloat_object_marked_p( msgc_context_t *c, word x ) {
  return msgc_object_marked_p( c, x );
}
static void msfloat_mark_objects_from_roots( msgc_context_t *c,
                                             int *marked, int *traced, 
                                             int *words_marked ) {
  msgc_mark_objects_from_roots( c, marked, traced, words_marked );
}
static void msfloat_mark_objects_from_roots_and_remsets( msgc_context_t *c ) {
  int m, t, wm;
  msgc_mark_objects_from_roots_and_remsets( c, &m, &t, &wm );
}

struct float_counts {
  int zzflt; /* float according to remsets and globals */
  int rsflt; /* float according to globals; live according to remsets */
  int total; /* total occupancy count */
};

struct visit_measuring_float_data {
  msgc_context_t *context;
  msgc_context_t *context_incl_remsets;
  struct float_counts words;
  struct float_counts objs;
};

void zero_float_counts( struct float_counts *counts ) 
{
  counts->zzflt = 0;
  counts->rsflt = 0;
  counts->total = 0;
}

void zero_measuring_float_data( struct visit_measuring_float_data *data ) 
{
  zero_float_counts( &data->words );
  zero_float_counts( &data->objs );
}

static void* visit_measuring_float( word *addr, int tag, void *accum ) 
{
  struct visit_measuring_float_data *data = 
    (struct visit_measuring_float_data*)accum;
  word obj; 
  bool marked;
  bool marked_via_remsets;
  int words;
  struct float_counts *type_counts;
  obj = tagptr( addr, tag );
  marked = 
    msfloat_object_marked_p( data->context, obj );
  marked_via_remsets = 
    msfloat_object_marked_p( data->context_incl_remsets, obj );

  data->objs.total += 1 ;
  if (!marked && !marked_via_remsets) {
    data->objs.zzflt += 1;
  }
  if (!marked && marked_via_remsets) {
    data->objs.rsflt += 1;
  }

  switch (tag) {
  case PAIR_TAG:
    words = 2; 
    break;
  case VEC_TAG:
  case BVEC_TAG:
  case PROC_TAG:
    words = roundup8( sizefield( *addr )+4 ) / 4;
    break;
  default:
    assert(0);
  }
  data->words.total += words;
  if (!marked && !marked_via_remsets)
    data->words.zzflt += words;
  if (!marked && marked_via_remsets)
    data->words.rsflt += words;
  return data;
}

#define BAR_LENGTH 20
static int fill_up_to( char *bar, char mark, char altmark, int amt, int max ) {
  int i;
  int count;
  int rtn = max;
  if (max == 0) 
    return rtn;
  else
    count = (int)((amt*BAR_LENGTH)/max);
  assert(count >= 0);
  if (count > BAR_LENGTH) {
    rtn = amt;
    count = BAR_LENGTH;
    mark = altmark;
  }
  assert(count <= BAR_LENGTH);
  for(i = 0; i < count; i++) {
    bar[i] = mark;
  }
  return rtn;
}

static void print_float_stats_for_rgn( char *caller_name, gc_t *gc, int i, 
                                       struct visit_measuring_float_data data )
{
  int rgn;
  int newmax;
  int data_count, easy_float, hard_float;
  { 
    char bars[BAR_LENGTH+2];
    bars[BAR_LENGTH] = '\0';
    bars[BAR_LENGTH+1] = '\0';
    {
      data_count = data.words.total*4;
      easy_float = data.words.zzflt*4+data.words.rsflt*4;
      hard_float = data.words.rsflt*4;
      newmax = DATA(gc)->ephemeral_area[i]->maximum;
      newmax = fill_up_to( bars, ' ', '@', newmax, newmax );
      newmax = fill_up_to( bars, '.', '!', data_count, newmax );
      newmax = fill_up_to( bars, 'Z', 'z', easy_float, newmax );
      newmax = fill_up_to( bars, 'R', 'r', hard_float, newmax );
    }

    { 
      bool rgn_summarized;
      int rgn_summarized_live;
      old_heap_t *heap = DATA(gc)->ephemeral_area[ i ];
      rgn = i+1;
      rgn_summarized_live = sm_summarized_live( DATA(gc)->summaries, rgn );
      oh_synchronize( heap );
      consolemsg( "%scycle % 3d region% 4d "
                  "remset live: %7d %7d %8d lastmajor: %7d "
                  "float{ objs: %7d/%7d words: %7d/%7d %7d }%s %s %s", 
                  caller_name,
                  DATA(gc)->rrof_cycle_count, 
                  rgn, 
                  gc->remset[ rgn ]->live, gc->major_remset[ rgn ]->live, 
                  rgn_summarized_live, 
                  heap->live_last_major_gc/4, 
                  data.objs.zzflt+data.objs.rsflt,
                  data.objs.total,
                  data.words.zzflt+data.words.rsflt,
                  data.words.total, 
                  heap->allocated/4, 
                  (( rgn == DATA(gc)->rrof_to_region &&
                     rgn == DATA(gc)->rrof_next_region ) ? "*" :
                   ( rgn == DATA(gc)->rrof_to_region )   ? "t" :
                   ( rgn == DATA(gc)->rrof_next_region ) ? "n" :
                   ( rgn_summarized_live >= 0 )          ? "s" :
                   ( rgn >  DATA(gc)->region_count     ) ? "e" : 
                   /* else                              */ " "),
                  bars,
                  (DATA(gc)->ephemeral_area[ i ]->
                   has_popular_objects ? "(popular)" : "")
                  );
    }
  }
}

void print_float_stats( char *caller_name, gc_t *gc ) 
{
  /* every collection cycle, lets use the mark/sweep system to 
   * measure how much float has accumulated. */
  {
    msgc_context_t *context;
    msgc_context_t *context_incl_remsets;
    int i, rgn;
    int marked=0, traced=0, words_marked=0; 
    int marked_incl=0, traced_incl=0, words_marked_incl=0; 
    int total_float_words = 0, total_float_objects = 0;
    int estimated_live = 0;
    struct visit_measuring_float_data data;
    context = msgc_begin( gc );
    msfloat_mark_objects_from_roots( context, &marked, &traced, &words_marked );

    context_incl_remsets = msgc_begin( gc );
    msfloat_mark_objects_from_roots_and_remsets( context_incl_remsets );

    for( i=0; i < DATA(gc)->ephemeral_area_count; i++) {
      data.context = context;
      data.context_incl_remsets = context_incl_remsets;
      zero_measuring_float_data( &data );
      DATA(gc)->ephemeral_area[ i ]->enumerate
        ( DATA(gc)->ephemeral_area[ i ], visit_measuring_float, &data );
      print_float_stats_for_rgn( caller_name, gc, i, data );
      total_float_objects += data.objs.zzflt+data.objs.rsflt;
      total_float_words += data.words.zzflt+data.objs.rsflt;
      if (INCLUDE_POP_RGNS_IN_LOADCALC || 
          ! DATA(gc)->ephemeral_area[i]->has_popular_objects)
        estimated_live += DATA(gc)->ephemeral_area[ i ]->live_last_major_gc/sizeof(word);
    }
    consolemsg( "cycle % 3d total float { objs: %dk words: %dK (%3d%%,%3d%%) } nextrefine: %d "
                "live{ est: %dK act: %dK max: %dK } estdelta: %0.2f ",
                DATA(gc)->rrof_cycle_count, 
                total_float_objects/1000, 
                total_float_words/1024, 
                (int)(100.0*(double)total_float_words/(double)words_marked), 
                DATA(gc)->max_live_words?(int)(100.0*(double)total_float_words/(double)DATA(gc)->max_live_words):0, 
                DATA(gc)->rrof_refine_mark_countdown, 
                estimated_live/1024, words_marked/1024, DATA(gc)->max_live_words/1024, 
                estimated_live?(((double)estimated_live)/(double)words_marked):0.0 );

    msgc_end( context_incl_remsets );
    msgc_end( context );
  }
}

