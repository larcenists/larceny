/* Copyright 2009 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Refactoring previous array of hashtable (ie remset[] and major_remset[]) 
 * representation into implementation of uremset_t.
 *
 */

#include "larceny.h"
#include "gclib.h"
#include "gc_t.h"
#include "remset_t.h"
#include "summary_t.h"
#include "uremset_t.h"
#include "uremset_array_t.h"

typedef struct uremset_array_data uremset_array_data_t;

struct uremset_array_data {
  int remset_count;
  remset_t **remset;
  remset_t **major_remset;
};

#define DATA(urs) ((uremset_array_data_t*)(urs->data))

static void assimilate_and_clear( uremset_t *urs, int g1, int g2 )
{
  assert(0);
}
static int live_count( uremset_t *urs, int gno )
{
  return (DATA(urs)->remset[gno]->live 
          + DATA(urs)->major_remset[gno]->live);
}
static void checkpoint_stats( uremset_t *urs, int gno )
{
  rs_stats( DATA(urs)->remset[gno] );
  rs_stats( DATA(urs)->major_remset[gno] );
}
static void expand_remset_gnos( uremset_t *urs, int fresh_gno )
{
  int i;
  gc_t *gc = urs->collector;
  int new_remset_count = DATA(urs)->remset_count + 1;
  remset_t** new_remset = 
    (remset_t**)must_malloc( sizeof( remset_t* )*new_remset_count );
  remset_t** new_major_remset = 
    (remset_t**)must_malloc( sizeof( remset_t* )*new_remset_count );

  for( i = 0; i < fresh_gno; i++ ) {
    new_remset[i] = DATA(urs)->remset[i];
    new_major_remset[i] = DATA(urs)->major_remset[i];
  }
  new_remset[fresh_gno] = create_remset( 0, 0 );
  new_major_remset[fresh_gno] = create_remset( 0, 0 );
  for( i = fresh_gno+1; i < new_remset_count; i++ ) {
    new_remset[i] = DATA(urs)->remset[i-1];
    new_major_remset[i] = DATA(urs)->major_remset[i-1];
  }
  free( DATA(urs)->remset );
  free( DATA(urs)->major_remset );
  DATA(urs)->remset = new_remset;
  DATA(urs)->major_remset = new_major_remset;
  DATA(urs)->remset_count = new_remset_count;
}
static void              clear( uremset_t *urs, int gno )
{
  rs_clear( DATA(urs)->remset[ gno ] );
  rs_clear( DATA(urs)->major_remset[ gno ] );
}
static void              clear_minor( uremset_t *urs )
{
  int i;
  for( i=1; i < DATA(urs)->remset_count; i++) {
    rs_clear( DATA(urs)->remset[ i ] );
  }
}

static bool copy_over_to( word loc, void *data, unsigned *stats )
{
  remset_t *that = (remset_t*)data;
  rs_add_elem( that, loc );
  return TRUE;
}

static void copy_minor_to_major( uremset_t *urs ) 
{
  int i;
  for( i=1; i < DATA(urs)->remset_count; i++) {
    rs_enumerate( DATA(urs)->remset[ i ], 
                  copy_over_to, 
                  DATA(urs)->major_remset[ i ] );
  }
}

static bool       add_elem_new( uremset_t *urs, word w )
{
  return rs_add_elem_new( DATA(urs)->major_remset[ gen_of(w) ], w );
}
static bool           add_elem( uremset_t *urs, word w )
{
  assert(0);
}
static bool          add_elems( uremset_t *urs, word *bot, word *top )
{
  assert( DATA(urs)->remset != NULL );
  if (bot != top) {
    annoyingmsg("urs_add_elems rs[]=0x%08x bot=0x%08x top=0x%08x",
                (unsigned)(DATA(urs)->remset), (unsigned)bot, (unsigned)top );
    return rs_add_elems_distribute( DATA(urs)->remset, bot, top );
  } else {
    return FALSE;
  }
}

struct apply_scanner_to_rs_data {
  bool (*scanner)( word loc, void *data );
  remset_t *filter_these; /* If not NULL then skip these during enumeration */
  remset_t *mirror_these; /* If not NULL then propogate deletes here during enumeration */
  void *scanner_data;
};
static bool apply_scanner_to_rs( word loc, void *data, unsigned *stats )
{
  struct apply_scanner_to_rs_data *my_data = 
    (struct apply_scanner_to_rs_data*)data;

  if ((my_data->filter_these != NULL) 
      && rs_isremembered( my_data->filter_these, loc )) {
    return TRUE; /* skip */
  } else {
    bool keep_elem; 
    keep_elem = my_data->scanner( loc, my_data->scanner_data );
    if (! keep_elem && (my_data->mirror_these != NULL)) {
      rs_del_elem( my_data->mirror_these, loc );
    }
    return keep_elem;
  }
}
static void enumerate_gno( uremset_t *urs, 
                           bool incl_tag, 
                           int gno, 
                           bool (*scanner)(word loc, void *data), 
                           void *data )
{
  int i;
  struct apply_scanner_to_rs_data wrapper_data;
  wrapper_data.scanner = scanner;
  wrapper_data.scanner_data = data;

  wrapper_data.filter_these = DATA(urs)->major_remset[ gno ];
  wrapper_data.mirror_these = NULL;
  rs_enumerate( DATA(urs)->remset[ gno ], 
                apply_scanner_to_rs, &wrapper_data );
  wrapper_data.filter_these = NULL;
  wrapper_data.mirror_these = DATA(urs)->remset[ gno ];
  rs_enumerate( DATA(urs)->major_remset[ gno ], 
                apply_scanner_to_rs, &wrapper_data );
}
static void enumerate_allbutgno( uremset_t *urs, 
                                 bool incl_tag, 
                                 int gno, 
                                 bool (*scanner)(word loc, void *data), 
                                 void *data )
{
  int i;
  struct apply_scanner_to_rs_data wrapper_data;
  wrapper_data.scanner = scanner;
  wrapper_data.scanner_data = data;

  assert(0);

  /* static objects die; remset_count includes static remset (thus
   * refinement eliminates corpses with dangling pointers). */
  for( i=1; i < DATA(urs)->remset_count; i++) {
    if (i != gno) {
      rs_enumerate( DATA(urs)->remset[ i ], 
                    apply_scanner_to_rs, &wrapper_data );
      rs_enumerate( DATA(urs)->major_remset[ i ], 
                    apply_scanner_to_rs, &wrapper_data );
    }
  }
}
static void enumerate_older( uremset_t *urs, 
                             bool incl_tag, 
                             int gno, 
                             bool (*scanner)(word loc, void *data), 
                             void *data )
{
  int i;
  struct apply_scanner_to_rs_data wrapper_data;
  wrapper_data.scanner = scanner;
  wrapper_data.scanner_data = data;

  assert(0);

  /* static objects die; remset_count includes static remset (thus
   * refinement eliminates corpses with dangling pointers). */
  for( i=gno; i < DATA(urs)->remset_count; i++) {
    rs_enumerate( DATA(urs)->remset[ i ], 
                  apply_scanner_to_rs, &wrapper_data );
    rs_enumerate( DATA(urs)->major_remset[ i ], 
                  apply_scanner_to_rs, &wrapper_data );
  }
}
static void enumerate_minor_complement( uremset_t *urs, 
                                        bool incl_tag, 
                                        gset_t gset, 
                                        bool (*scanner)(word loc, void *data), 
                                        void *data )
{
  int i;
  struct apply_scanner_to_rs_data wrapper_data;
  int rs_count;
  wrapper_data.scanner = scanner;
  wrapper_data.scanner_data = data;
  wrapper_data.filter_these = NULL;
  wrapper_data.mirror_these = NULL;
  
  rs_count = DATA(urs)->remset_count;
  /* static objects die; remset_count includes static remset (thus
   * refinement eliminates corpses with dangling pointers). */
  for( i=1; i < rs_count; i++) {
    if (! gset_memberp( i, gset )) {
      rs_enumerate( DATA(urs)->remset[ i ], 
                    apply_scanner_to_rs, &wrapper_data );
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
  int ecount = DATA(urs)->remset_count;
  struct apply_scanner_to_rs_data wrapper_data;
  wrapper_data.scanner = scanner;
  wrapper_data.scanner_data = data;

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
    i = static_area_gno;
    wrapper_data.filter_these = DATA(urs)->major_remset[ i ];
    wrapper_data.mirror_these = NULL;
    rs_enumerate( DATA(urs)->remset[i], 
                  apply_scanner_to_rs, &wrapper_data );

    static_area_gno = urs->collector->gno_count-1;
    i = static_area_gno;
    wrapper_data.filter_these = NULL;
    wrapper_data.mirror_these = DATA(urs)->remset[ i ];
    rs_enumerate( DATA(urs)->major_remset[i], 
                  apply_scanner_to_rs, &wrapper_data);
  }
  for( i = 1; i < ecount-1; i++ ) {
    if (! gset_memberp( i, gset )) {
      wrapper_data.filter_these = DATA(urs)->major_remset[ i ];
      wrapper_data.mirror_these = NULL;
      rs_enumerate( DATA(urs)->remset[i], 
                    apply_scanner_to_rs, &wrapper_data);
      wrapper_data.filter_these = NULL;
      wrapper_data.mirror_these = DATA(urs)->remset[ i ];
      rs_enumerate( DATA(urs)->major_remset[i], 
                    apply_scanner_to_rs, &wrapper_data);
    }
  }
}

static void          enumerate( uremset_t *urs, 
                                bool incl_tag, 
                                bool (*scanner)(word loc, void *data), 
                                void *data )
{
  int i;
  struct apply_scanner_to_rs_data wrapper_data;
  wrapper_data.scanner = scanner;
  wrapper_data.scanner_data = data;

  /* static objects die; remset_count includes static remset (thus
   * refinement eliminates corpses with dangling pointers). */
  for( i=1; i < DATA(urs)->remset_count; i++) {
    wrapper_data.filter_these = DATA(urs)->major_remset[ i ];
    wrapper_data.mirror_these = NULL;
    rs_enumerate( DATA(urs)->remset[ i ], 
                  apply_scanner_to_rs, &wrapper_data );
    wrapper_data.filter_these = NULL;
    wrapper_data.mirror_these = DATA(urs)->remset[ i ];
    rs_enumerate( DATA(urs)->major_remset[ i ], 
                  apply_scanner_to_rs, &wrapper_data );
  }
}
static bool      is_remembered( uremset_t *urs, word w )
{
  int w_gno = gen_of(w);

  if (w_gno == 0) 
    return FALSE;

  return 
    (rs_isremembered( DATA(urs)->remset[ w_gno ], w )
     || rs_isremembered( DATA(urs)->major_remset[ w_gno ], w ));
}
static void       init_summary( uremset_t *urs, 
                                int gno, 
                                int max_words_per_step, 
                                /* out parameter */ summary_t *s )
{

  assert(0);

}

uremset_t *alloc_uremset_array( gc_t *gc, gc_param_t *info )
{
  int i;
  int remset_count;
  uremset_array_data_t *data;
  uremset_t *urs;

  data = (uremset_array_data_t*)must_malloc( sizeof( uremset_array_data_t ) );

  remset_count = gc->gno_count;

  data->remset_count = remset_count;
  data->remset = 
    (remset_t**)must_malloc( sizeof( remset_t* )*remset_count );
  data->major_remset = 
    (remset_t**)must_malloc( sizeof( remset_t* )*remset_count );

  data->remset[0] = (void*)0xDEADBEEF;
  data->major_remset[0] = (void*)0xDEADBEEF;
  for ( i = 1; i < gc->gno_count; i++ ) {
    data->remset[i] = create_remset( info->rhash, 0 );
    data->major_remset[i] = create_remset( info->rhash, 0 );
  }

  return create_uremset_t( gc, 
                           "word hashset arrays",
                           (void*)data, 
                           expand_remset_gnos,
                           clear,
                           assimilate_and_clear, 
                           add_elem_new,
                           add_elem,
                           add_elems, 
                           enumerate_gno,
                           enumerate_allbutgno, 
                           enumerate_older, 
                           clear_minor, 
                           copy_minor_to_major, 
                           enumerate_minor_complement, 
                           enumerate_complement, 
                           enumerate,
                           is_remembered,
                           live_count, 
                           init_summary,
                           checkpoint_stats );
}
