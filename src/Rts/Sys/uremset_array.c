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
static bool       add_elem_new( uremset_t *urs, word w )
{
  assert(0);
}
static bool           add_elem( uremset_t *urs, word w )
{
  assert(0);
}
static bool          add_elems( uremset_t *urs, word *bot, word *top )
{
  rs_add_elems_distribute( DATA(urs)->remset, bot, top );
}

struct apply_scanner_to_rs_data {
  bool (*scanner)( word loc, void *data );
  void *scanner_data;
};
static bool apply_scanner_to_rs( word loc, void *data, unsigned *stats )
{
  struct apply_scanner_to_rs_data *my_data = 
    (struct apply_scanner_to_rs_data*)data;
  assert(0);

  return my_data->scanner( loc, my_data->scanner_data );
}
static void enumerate_gno( uremset_t *urs, 
                           int gno, 
                           bool (*scanner)(word loc, void *data), 
                           void *data )
{
  int i;
  struct apply_scanner_to_rs_data wrapper_data;
  wrapper_data.scanner = scanner;
  wrapper_data.scanner_data = data;

  assert(0);

  rs_enumerate( DATA(urs)->remset[ gno ], 
                apply_scanner_to_rs, &wrapper_data );
  rs_enumerate( DATA(urs)->major_remset[ gno ], 
                apply_scanner_to_rs, &wrapper_data );
}
static void enumerate_allbutgno( uremset_t *urs, int gno, 
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
static void enumerate_older( uremset_t *urs, int gno, 
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
static void    enumerate_minor( uremset_t *urs, 
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
    rs_enumerate( DATA(urs)->remset[ i ], 
                  apply_scanner_to_rs, &wrapper_data );
  }
}
static void enumerate_complement( uremset_t *urs, 
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

  assert(0);

  for( i = 1; i <= ecount; i++ ) {
    if (! gset_memberp( i, gset )) {
      rs_enumerate( DATA(urs)->remset[i], apply_scanner_to_rs, data);
      /* XXX: I may need to filter out members of gc->remset[i] 
       * because some components like summ_matrix assume that
       * the enumerate does not duplicate entries... */
      rs_enumerate( DATA(urs)->major_remset[i], apply_scanner_to_rs, data);
    }
  }
}

static void          enumerate( uremset_t *urs, 
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
    rs_enumerate( DATA(urs)->remset[ i ], 
                  apply_scanner_to_rs, &wrapper_data );
    rs_enumerate( DATA(urs)->major_remset[ i ], 
                  apply_scanner_to_rs, &wrapper_data );
  }
}
static bool      is_remembered( uremset_t *urs, word w )
{
  int w_gno = gen_of(w);

  assert(0);

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
  uremset_array_data_t *data;
  uremset_t *urs;
  data = (uremset_array_data_t*)must_malloc( sizeof( uremset_array_data_t ) );

  data->remset = 
    (remset_t**)must_malloc( sizeof( remset_t* )*gc->gno_count );
  data->major_remset = 
    (remset_t**)must_malloc( sizeof( remset_t* )*gc->gno_count );

  data->remset[0] = (void*)0xDEADBEEF;
  data->major_remset[0] = (void*)0xDEADBEEF;
  for ( i = 1; i < gc->gno_count; i++ ) {
    data->remset[i] = create_remset( info->rhash, 0 );
    data->major_remset[i] = create_remset( info->rhash, 0 );
  }

  return create_uremset_t( "word hashset arrays",
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
                           enumerate_minor, 
                           enumerate_complement, 
                           enumerate,
                           is_remembered,
                           live_count, 
                           init_summary,
                           checkpoint_stats );
}
