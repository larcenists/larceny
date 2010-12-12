/* Copyright 2009 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * A new remset rep based on a (sparsely represented) bitmap of the heap.
 * 
 */
#include "larceny.h"
#include "extbmp_t.h"
#include "gclib.h"
#include "gc_t.h"
#include "remset_t.h"
#include "uremset_t.h"

typedef struct uremset_extbmp_data uremset_extbmp_data_t;

struct uremset_extbmp_data {
  int live_count_len;
  int *live_count;
  extbmp_t *minor_remset;
  extbmp_t *remset;
};

#define DATA(urs) ((uremset_extbmp_data_t*)(urs->data))

static word strip_tag( word w ) {
  return (word)ptrof(w);
}

static int live_count( uremset_t *urs, int gno )
{
  return DATA(urs)->live_count[ gno ];
}
static void checkpoint_stats( uremset_t *urs, int gno )
{ /* no op */ }
static void expand_remset_gnos( uremset_t *urs, int fresh_gno )
{ 
  int i;
  gc_t *gc = urs->collector;
  int new_count_len = DATA(urs)->live_count_len + 1;
  int* new_count = (int*)must_malloc( sizeof( int )*new_count_len );

  extbmp_expand_remset_gnos( DATA(urs)->minor_remset, fresh_gno );
  extbmp_expand_remset_gnos( DATA(urs)->remset, fresh_gno );

  for( i = 0; i < fresh_gno; i++ ) {
    new_count[i] = DATA(urs)->live_count[i];
  }
  new_count[fresh_gno] = 0;
  for( i = fresh_gno+1; i < new_count_len; i++ ) {
    new_count[i] = DATA(urs)->live_count[i-1];
  }

  free( DATA(urs)->live_count );
  DATA(urs)->live_count = new_count;
  DATA(urs)->live_count_len = new_count_len;
}
static void              clear( uremset_t *urs, int gno )
{
  extbmp_clear_members_in( DATA(urs)->minor_remset, gno );
  extbmp_clear_members_in( DATA(urs)->remset, gno );
}

static void              clear_minor( uremset_t *urs )
{
  int i;
  for ( i = 1 ; i < urs->collector->gno_count; i++ )
    extbmp_clear_members_in( DATA(urs)->minor_remset, i );
}
static bool       add_elem_new( uremset_t *urs, word w )
{
  /* (could/should assert2 non-membership here) */
  DATA(urs)->live_count[gen_of(w)] += 1;
  extbmp_add_elem( DATA(urs)->remset, strip_tag(w) );
  return FALSE; /* extbmp does not tell us whether it overflowed. */
}
static bool       add_elem( uremset_t *urs, word w )
{
  bool present;
  present =
    extbmp_add_elem( DATA(urs)->remset, strip_tag(w) );
  if (! present)
    DATA(urs)->live_count[gen_of(w)] += 1;
  return FALSE; /* extbmp does not tell us whether it overflowed. */
}
static bool          add_elems( uremset_t *urs, word *bot, word *top )
{
  word *p, *q, w;
  bool already_present;
  p = bot;
  q = top;
  while (q > p) {
    q--;
    w = *q;
    if ( is_fixnum(w) ) { 
      /* skip fixnums in SSB log */ 
    } else {
#if 0
      consolemsg("uremset_extbmp add_elems add w=0x%08x", w );
#endif
      already_present = 
        extbmp_add_elem( DATA(urs)->minor_remset, strip_tag(w) );
      if (! already_present)
        DATA(urs)->live_count[ gen_of(w) ] += 1;
    }
  }
  return FALSE; /* extbmp does not tell us whether it overflowed. */
}

static bool copy_over_to( word loc, void *data ) 
{
  extbmp_t *that = (extbmp_t*)data;
  extbmp_add_elem( that, loc );
  return TRUE;
}

static void copy_minor_to_major( uremset_t *urs ) 
{
  extbmp_enumerate( DATA(urs)->minor_remset, 
                    FALSE, 
                    copy_over_to, 
                    DATA(urs)->remset );
}

struct wrap_scan_propogating_deletes_to_minor_data {
  uremset_t *urs;
  bool (*underlying_scanner)(word loc, void *data);
  void *underlying_data;
};
static bool wrap_scan_propogating_deletes_to_minor( word loc, void *my_data )
{
  struct wrap_scan_propogating_deletes_to_minor_data *data;
  bool rtn;
  bool (*underlying_scanner)(word, void *);
  void *underlying_data;
  uremset_t *urs;
  data = (struct wrap_scan_propogating_deletes_to_minor_data*)my_data;

  underlying_scanner = data->underlying_scanner;
  underlying_data    = data->underlying_data;
  urs                = data->urs;

  rtn = underlying_scanner( loc, underlying_data );
  if (! rtn) {
    extbmp_del_elem( DATA(urs)->minor_remset, loc );
  }
  return rtn;
}

static void enumerate_gno( uremset_t *urs, 
                           bool incl_tag, 
                           int gno,
                           bool (*scanner)(word loc, void *data), 
                           void *data )
{
  struct wrap_scan_propogating_deletes_to_minor_data wrapper_data;

  /* Copy minor to major so scanner encounters objects at most once */
  extbmp_enumerate_in( DATA(urs)->minor_remset, FALSE, gno, copy_over_to, DATA(urs)->remset );

  wrapper_data.urs = urs;
  wrapper_data.underlying_scanner = scanner;
  wrapper_data.underlying_data = data;
  extbmp_enumerate_in( DATA(urs)->remset, incl_tag, gno, 
                       wrap_scan_propogating_deletes_to_minor, 
                       &wrapper_data );
}

static void enumerate_minor_complement( uremset_t *urs, 
                                        bool incl_tag, 
                                        gset_t gset, 
                                        bool (*scanner)(word loc, void *data), 
                                        void *data )
{
  int i;
  int rs_count; 


  assert( (gset.tag == gs_singleton) && gset_memberp( 0, gset ) );
  extbmp_enumerate( DATA(urs)->minor_remset, incl_tag, scanner, data );
}
static void enumerate_complement( uremset_t *urs, 
                                  bool incl_tag, 
                                  gset_t gset, 
                                  bool (*scanner)(word loc, 
                                                  void *data), 
                                  void *data ) 
{
  int i;
  int ecount = urs->collector->gno_count;

  /* Handle static area (aka highest numbered) first, so that we 
   * worry less about scanner's asynchronous gno_count
   * modifications (aka region allocations) messing things up.
   * Conceptually, the for loop now goes:
   * 
   *    [ecount-1, 1, 2, 3, ..., ecount-2].
   *
   * Note that such modifications can still happen during any
   * invocation of scanner, which is why we need to re-establish
   * static_area_gno in between the two enumerate invocations below.
   * (This ugliness has crept elsewhere: the static area's gno is also
   * handled specially within extbmp_enumerate_in itself.)
   */
  if (urs->collector->static_area != NULL) {
    /* Deliberately not re-using ecount, because we want it to retain
     * its original value during the for loop below. */
    int static_area_gno;

    static_area_gno = urs->collector->gno_count-1;
    enumerate_gno( urs, incl_tag, static_area_gno, scanner, data );
  }
  for( i = 1; i < ecount-1; i++ ) {
    if (! gset_memberp( i, gset )) {
      enumerate_gno( urs, incl_tag, i, scanner, data );
    }
  }
}
static void          enumerate( uremset_t *urs, 
                                bool incl_tag, 
                                bool (*scanner)(word loc, void *data), 
                                void *data )
{
  struct wrap_scan_propogating_deletes_to_minor_data wrapper_data;

  extbmp_enumerate( DATA(urs)->minor_remset, FALSE, copy_over_to, DATA(urs)->remset );

  wrapper_data.urs = urs;
  wrapper_data.underlying_scanner = scanner;
  wrapper_data.underlying_data = data;
  extbmp_enumerate( DATA(urs)->remset, incl_tag, 
                    wrap_scan_propogating_deletes_to_minor, 
                    &wrapper_data );
}
static bool      is_remembered( uremset_t *urs, word w )
{
  return (extbmp_is_member( DATA(urs)->minor_remset, strip_tag(w) )
          || extbmp_is_member( DATA(urs)->remset, strip_tag(w) ));
}


uremset_t *alloc_uremset_extbmp( gc_t *gc, gc_param_t *info )
{
  int i;
  uremset_extbmp_data_t *data;
  uremset_t *urs;

  data = (uremset_extbmp_data_t*)
    must_malloc( sizeof( uremset_extbmp_data_t ) );
  { 
    int i;
    data->live_count_len = gc->gno_count;
    data->live_count = (int*)must_malloc( sizeof(int)*gc->gno_count );
    for( i=0; i<gc->gno_count; i++ ) {
      data->live_count[i] = 0;
    }
  }
  data->minor_remset = create_extensible_bitmap( gc, info );
  data->remset = create_extensible_bitmap( gc, info );

  return create_uremset_t( gc,
                           "extensible",
                           (void*)data, 
                           expand_remset_gnos,
                           clear,
                           0 /*assimilate_and_clear*/, 
                           add_elem_new,
                           add_elem, 
                           add_elems, 
                           enumerate_gno,
                           0 /*enumerate_allbutgno*/, 
                           0 /*enumerate_older*/, 
                           clear_minor, 
                           copy_minor_to_major, 
                           enumerate_minor_complement, 
                           enumerate_complement, 
                           enumerate,
                           is_remembered,
                           live_count, 
                           0 /* init_summary */,
                           checkpoint_stats );
}
