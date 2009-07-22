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
  extbmp_t *minor_remset;
  extbmp_t *remset;
};

#define DATA(urs) ((uremset_extbmp_data_t*)(urs->data))

static word strip_tag( word w ) {
  return (word)ptrof(w);
}

static int live_count( uremset_t *urs, int gno )
{
  return 
    (extbmp_count_members_in( DATA(urs)->minor_remset, gno )
     + extbmp_count_members_in( DATA(urs)->remset, gno ));
}
static void checkpoint_stats( uremset_t *urs, int gno )
{ /* no op */ }
static void expand_remset_gnos( uremset_t *urs, int fresh_gno )
{ /* no op */ }
static void              clear( uremset_t *urs, int gno )
{
  extbmp_clear_members_in( DATA(urs)->minor_remset, gno );
  extbmp_clear_members_in( DATA(urs)->remset, gno );
}
static bool       add_elem_new( uremset_t *urs, word w )
{
  /* (could assert non-membership here) */
  extbmp_add_elem( DATA(urs)->remset, strip_tag(w) );
}
static bool          add_elems( uremset_t *urs, word *bot, word *top )
{
  word *p, *q, w;
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
      extbmp_add_elem( DATA(urs)->minor_remset, strip_tag(w) );
    }
  }
}
static void enumerate_gno( uremset_t *urs, 
                           int gno,
                           bool (*scanner)(word loc, void *data), 
                           void *data )
{
  extbmp_enumerate_in( DATA(urs)->minor_remset, gno, scanner, data );
  extbmp_enumerate_in( DATA(urs)->remset, gno, scanner, data );
}

static void enumerate_minor_complement( uremset_t *urs, 
                                        gset_t gset, 
                                        bool (*scanner)(word loc, void *data), 
                                        void *data )
{
  int i;
  int rs_count; 

  rs_count = urs->collector->gno_count;
  /* static objects die; remset_count includes static remset (thus
   * refinement eliminates corpses with dangling pointers). */
  for( i=1; i < rs_count; i++) {
    if (! gset_memberp( i, gset )) {
      extbmp_enumerate_in( DATA(urs)->minor_remset, i, scanner, data );
    }
  }
}
static void enumerate_complement( uremset_t *urs, 
                                  gset_t gset, 
                                  bool (*scanner)(word loc, 
                                                  void *data), 
                                  void *data ) 
{
  int i;
  int ecount = urs->collector->gno_count;

  for( i = 1; i < ecount; i++ ) {
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
