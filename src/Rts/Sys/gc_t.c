/* Copyright 1998 Lars T Hansen.              -*- indent-tabs-mode: nil -*-
 * 
 * $Id$
 *
 * Larceny -- garbage collector structure constructor
 */

#include "larceny.h"
#include "gc_t.h"
#include "gset_t.h"

gc_t 
*create_gc_t(char *id,
	     void *data,
	     int  (*initialize)( gc_t *gc ),
	     word *(*allocate)( gc_t *gc, int nbytes, bool no_gc, bool atomic),
	     word *(*allocate_nonmoving)( gc_t *gc, int nbytes, bool atomic ),
	     void (*make_room)( gc_t *gc ), 
	     void (*collect)( gc_t *gc, int gen, int bytes, gc_type_t req ),
             void (*incremental)( gc_t *gc ),
	     void (*set_policy)( gc_t *gc, int heap, int x, int y ),
	     word *(*data_load_area)( gc_t *gc, int nbytes ),
	     word *(*text_load_area)( gc_t *gc, int nbytes ),
	     int  (*iflush)( gc_t *gc, int generation ),
	     word (*creg_get)( gc_t *gc ),
	     void (*creg_set)( gc_t *gc, word k ),
	     void (*stack_overflow)( gc_t *gc ),
	     void (*stack_underflow)( gc_t *gc ),
	     int  (*compact_all_ssbs)( gc_t *gc ),
#if defined(SIMULATE_NEW_BARRIER)
	     int (*isremembered)( gc_t *gc, word w ),
#endif
	     void (*compact_np_ssb)( gc_t *gc ),
	     void (*np_remset_ptrs)( gc_t *gc, word ***ssbtop, word ***ssblim),
	     int  (*load_heap)( gc_t *gc, heapio_t *h ),
	     int  (*dump_heap)( gc_t *gc, const char *filename, bool compact ),
	     word *(*make_handle)( gc_t *gc, word obj ),
	     void (*free_handle)( gc_t *gc, word *handle ),
	     gno_state_t (*gno_state)( gc_t *gc, int gno ),
	     void (*enumerate_roots)( gc_t *gc, void (*f)( word*, void *),
				     void * ),
	     void (*enumerate_smircy_roots)( gc_t *gc, 
	                                     void (*f)( word*, void *),
	                                     void * ),
	     void (*enumerate_remsets_complement)
	        ( gc_t *gc, gset_t genset,
		  bool (*f)(word, void*),
		  void *data ),
	     void (*enumerate_remembered_locations)
	        ( gc_t *gc, gset_t genset,
	          void (*f)( loc_t, void* ), void*fd,
	          bool (*g)( word, void* ), void*gd),
	     void (*enumerate_hdr_address_ranges)
	        ( gc_t *gc, int gno, 
	          void (*f)( word *s, word *l, void *d), void *d ),
	     semispace_t *(*fresh_space)( gc_t *gc ),
	     semispace_t *(*find_space)( gc_t *gc, unsigned bytes_needed,
					 semispace_t *cur ),
	     int (*allocated_to_areas)( gc_t *gc, gset_t gs ),
	     int (*maximum_allotted)( gc_t *gc, gset_t gs ),
	     bool (*is_nonmoving)( gc_t *gc, int gen_no ), 
	     bool (*is_address_mapped)( gc_t *gc, word *addr, bool noisy ),
	     void (*check_remset_invs)( gc_t *gc, word src, word tgt ),
	     void (*points_across)( gc_t *gc, word lhs, int offset, word rhs ),
	     old_heap_t *(*heap_for_gno)(gc_t *gc, int gen_no ),
	     region_group_t (*region_group_for_gno)(gc_t *gc, int gen_no ),
	     void (*check_invariants_between_fwd_and_free)( gc_t *gc, int gen_no )
	     )
{
  gc_t *gc;
  gc = (gc_t*)must_malloc( sizeof( gc_t ) );

  gc->id = id;
  gc->data = data;

  gc->los = 0;
  gc->young_area = 0;
  gc->static_area = 0; 
  gc->los = 0;
  gc->gno_count = 0;
  gc->smircy = 0;
  gc->smircy_completion = 0;
  gc->np_remset = -1;
  gc->scan_update_remset = 0;

  gc->stat_max_entries_remset_scan = 0;
  gc->stat_total_entries_remset_scan = 0;
  gc->stat_max_remset_scan       = 0;
  gc->stat_max_remset_scan_cpu   = 0;
  gc->stat_total_remset_scan     = 0;
  gc->stat_total_remset_scan_cpu = 0;
  gc->stat_remset_scan_count     = 0;

  gc->words_from_nursery_last_gc = 0;

  gc->initialize = initialize;
  gc->allocate = allocate;
  gc->allocate_nonmoving = allocate_nonmoving;
  gc->make_room = make_room;
  gc->collect = collect;
  gc->incremental = incremental;
  gc->set_policy = set_policy;
  gc->data_load_area = data_load_area;
  gc->text_load_area = text_load_area;

  gc->iflush = iflush;
  gc->creg_get = creg_get;
  gc->creg_set = creg_set;
  gc->stack_overflow = stack_overflow;
  gc->stack_underflow = stack_underflow;

  gc->compact_all_ssbs = compact_all_ssbs;
#if defined(SIMULATE_NEW_BARRIER)
  gc->isremembered = isremembered;
#endif
  gc->compact_np_ssb = compact_np_ssb;

  gc->np_remset_ptrs = np_remset_ptrs;

  gc->load_heap = load_heap;
  gc->dump_heap = dump_heap;

  gc->make_handle = make_handle;
  gc->free_handle = free_handle;
  
  gc->gno_state = gno_state;

  gc->enumerate_roots = enumerate_roots;
  gc->enumerate_smircy_roots = enumerate_smircy_roots;
  gc->enumerate_remsets_complement = enumerate_remsets_complement;
  gc->enumerate_remembered_locations = enumerate_remembered_locations;
  gc->enumerate_hdr_address_ranges = enumerate_hdr_address_ranges;
  gc->fresh_space = fresh_space;
  gc->find_space = find_space;

  gc->allocated_to_areas = allocated_to_areas;
  gc->maximum_allotted = maximum_allotted;
  gc->is_nonmoving = is_nonmoving; 
  gc->is_address_mapped = is_address_mapped;
  gc->check_remset_invs = check_remset_invs;
  gc->points_across = points_across;
  gc->heap_for_gno = heap_for_gno;
  gc->region_group_for_gno = region_group_for_gno;

  gc->check_invariants_between_fwd_and_free = 
    check_invariants_between_fwd_and_free;

  return gc;
}

/* eof */
