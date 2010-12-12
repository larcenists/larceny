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
static bool       add_elem_new( uremset_t *urs, word w )
{
  /* (could/should assert2 non-membership here) */
  DATA(urs)->live_count[gen_of(w)] += 1;
  extbmp_add_elem( DATA(urs)->remset, strip_tag(w) );
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
}
static void enumerate_gno( uremset_t *urs, 
                           bool incl_tag, 
                           int gno,
                           bool (*scanner)(word loc, void *data), 
                           void *data )
{
  extbmp_enumerate_in( DATA(urs)->minor_remset, gno, scanner, data );
  extbmp_enumerate_in( DATA(urs)->remset, gno, scanner, data );
}

static void enumerate_minor_complement( uremset_t *urs, 
                                        bool incl_tag, 
                                        gset_t gset, 
                                        bool (*scanner)(word loc, void *data), 
                                        void *data )
{
  int i;
  int rs_count; 

  rs_count = urs->collector->gno_count;

  /* See general notes on this strange control flow (and state)
   * written in comments with enumerate_complement below. */
  if (urs->collector->static_area != NULL) {
    extbmp_enumerate_in( DATA(urs)->minor_remset, rs_count-1, scanner, data );
  }
  for( i=1; i < rs_count-1; i++) {
    if (! gset_memberp( i, gset )) {
      extbmp_enumerate_in( DATA(urs)->minor_remset, i, scanner, data );
    }
  }
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
    extbmp_enumerate_in( DATA(urs)->minor_remset, static_area_gno, scanner, data );
    /* Same comments as in main loop below apply here. */
    static_area_gno = urs->collector->gno_count-1;
    extbmp_enumerate_in( DATA(urs)->remset, static_area_gno, scanner, data );
  }
  for( i = 1; i < ecount-1; i++ ) {
    if (! gset_memberp( i, gset )) {
      extbmp_enumerate_in( DATA(urs)->minor_remset, i, scanner, data );
      /* XXX: I may need to filter out members of minor_remset because
       * some components like summ_matrix assume that the enumerate
       * does not duplicate entries... */
      extbmp_enumerate_in( DATA(urs)->remset, i, scanner, data );
    }
  }
}
static void          enumerate( uremset_t *urs, 
                                bool incl_tag, 
                                bool (*scanner)(word loc, void *data), 
                                void *data )
{
  extbmp_enumerate( DATA(urs)->minor_remset, scanner, data );
  extbmp_enumerate( DATA(urs)->remset, scanner, data );
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
                           0 /* add_elem */,
                           add_elems, 
                           enumerate_gno,
                           0 /*enumerate_allbutgno*/, 
                           0 /*enumerate_older*/, 
                           enumerate_minor_complement, 
                           enumerate_complement, 
                           enumerate,
                           is_remembered,
                           live_count, 
                           0 /* init_summary */,
                           checkpoint_stats );
}
