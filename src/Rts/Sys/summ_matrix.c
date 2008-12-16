/* Copyright 2008 Felix S Klock II              -*- indent-tabs-mode: nil -*-
 * 
 * $Id: $
 *
 * Summarization set matrix implementation.
 *
 */ 

#define GC_INTERNAL

#include <stdio.h>

#include "larceny.h"
#include "gclib.h"
#include "summ_matrix_t.h"
#include "remset_t.h"
#include "msgc-core.h"
#include "gc_t.h"
#include "seqbuf_t.h"

typedef struct pool pool_t;
typedef struct summ_cell summ_cell_t;
typedef struct summ_row summ_row_t;
typedef struct summ_col summ_col_t;
typedef struct summ_matrix_data summ_matrix_data_t;

struct pool {
  word *bot;
  word *top;
  word *lim;
  pool_t *next;
};

struct summ_cell {
  pool_t *objects;
  int source_gno;
  int target_gno;
  summ_cell_t *next_row;
  summ_cell_t *next_col;
};

struct summ_row {
  summ_cell_t *cell_lft;
  summ_cell_t *cell_rgt;
  int source_gno;
};

struct summ_col {
  summ_cell_t *cell_top;
  summ_cell_t *cell_bot;
  int target_gno;
  int word_count;
  bool overly_popular;
  bool construction_inprogress;
  bool construction_complete;
};

struct summ_matrix_data {
  double c;
  double p;
  int first_gno;
  int num_rgns;

  summ_row_t **rows;
  summ_col_t **cols;

  int col_complete_first;   /* completed summmary, or -1 if none */
  int col_complete_lim;
  int col_inprogress_first; /* under-construction, or -1 if none */
  int col_inprogress_lim;
};

#define DATA(sm)                ((summ_matrix_data_t*)(sm->data))

static pool_t *allocate_pool_segment( unsigned entries_per_pool_segment ) 
{
  pool_t *p;
  word *heapptr;
  p = (pool_t*) must_malloc( sizeof(pool_t) );
  while (1) {
    heapptr = gclib_alloc_rts( entries_per_pool_segment*sizeof(word), MB_SUMMARY_SETS );
    if (heapptr != 0) break;
    memfail( MF_RTS, "Can't allocate summary matrix pool.");
  }

  p->bot = p->top = heapptr;
  p->lim = heapptr + entries_per_pool_segment;
  p->next = NULL;

  return p;
}

static summ_cell_t* make_cell( int source_gno, int target_gno, int entries_per_pool_segment ) {
  summ_cell_t *e;
  e = (summ_cell_t*) must_malloc( sizeof(summ_cell_t) );
  e->objects = allocate_pool_segment( entries_per_pool_segment );
  e->source_gno = source_gno;
  e->target_gno = target_gno;
  return e;
}

static void print_matrix( summ_matrix_t *sm ) 
{
  int i, j, lim;
  summ_cell_t **col_cursors;
  summ_cell_t *row_cursor, *col_cursor;
  lim  = DATA(sm)->num_rgns - DATA(sm)->first_gno;

  col_cursors = (summ_cell_t**)must_malloc( lim*sizeof(summ_cell_t*) );
  for( i=0; i<lim; i++) {
    col_cursors[i] = DATA(sm)->cols[i]->cell_top;
    if (col_cursors[i] == NULL) {
      printf("col %d: NULL\n", i);
    } else {
      printf( "col %d: 0x%08x {0x%08x, %d~>%d, rgt: 0x%08x, dwn: 0x%08x}\n",
              i, (unsigned)col_cursors[i], (unsigned)col_cursors[i]->objects, 
              col_cursors[i]->source_gno, col_cursors[i]->target_gno, 
              (unsigned)col_cursors[i]->next_row, 
              (unsigned)col_cursors[i]->next_col );
    }
  }

  printf("COLS:          ");
  for ( i = 0; i < lim; i++ ) {
    printf( " 0x%08x", (unsigned)DATA(sm)->cols[i] );
  }
  printf("\n");
  for ( j = 0; j < lim; j++ ) {
    printf( "row: 0x%08x", (unsigned) DATA(sm)->rows[j] );
    row_cursor = DATA(sm)->rows[j]->cell_lft;
    col_cursor = col_cursors[j];
    for ( i = 0; i < lim; i++ ) {
      printf( "    (%2d,%2d)", i, j );
    }
    printf("\n");
    printf( "               " /*, DATA(sm)->rows[j]*/ );
    for ( i = 0; i < lim; i++ ) {
      printf( "        %s%s.", 
              (row_cursor != NULL && row_cursor->source_gno == i && row_cursor->target_gno == j)?"R":" ", 
              (col_cursor != NULL && col_cursor->source_gno == i && col_cursor->target_gno == j)?"C":" ");
    }
    printf("\n");
  }

  free( col_cursors );
}

static summ_row_t* alloc_row(int source_gno)
{
  summ_row_t *row;
  row = (summ_row_t*)must_malloc( sizeof( summ_row_t ));
  row->cell_lft = NULL;
  row->cell_rgt = NULL;
  row->source_gno = source_gno;
  return row;
}

static summ_row_t** alloc_rows(int first_gno, int num_rows) {
  summ_row_t** rows;
  int i;
  int gno = first_gno;
  rows = (summ_row_t**)must_malloc( num_rows*sizeof( summ_row_t* ));
  for(i = 0; i < num_rows; i++) {
    rows[i] = alloc_row(gno);
    gno++;
  }
  return rows;
}

static summ_col_t* alloc_col( int target_gno )
{
  summ_col_t *col;
  col = (summ_col_t*)must_malloc( sizeof( summ_col_t ));
  col->cell_top = NULL;
  col->cell_bot = NULL;
  col->word_count = 0;
  col->target_gno = target_gno;
  col->construction_inprogress = FALSE;
  col->construction_complete = FALSE;
  col->overly_popular = FALSE;
  return col;
}

static summ_col_t** alloc_cols(int first_gno, int num_cols) {
  summ_col_t** cols;
  int i;
  int gno = first_gno;
  cols = (summ_col_t**)must_malloc( num_cols*sizeof( summ_col_t* ));
  for(i = 0; i < num_cols; i++) {
    cols[i] = alloc_col(gno);
    gno++;
  }
  return cols;
}

/* Functions relevant to summary construction: 
 * protocol is:
 * 
 * for each region i
 *   prior_scan(i)
 *   for each object A in i
 *     object_for_scan(A)
 *     for each ref in A
 *        entry_from_scan(gen_of(ref))
 *   after_scan(i)
 */
void sm_prior_scan( summ_matrix_t *summ, int source_gno );

void sm_object_for_scan( summ_matrix_t *summ, word source_obj );

void sm_entry_from_scan( summ_matrix_t *summ, int target_gno );

void sm_after_scan( summ_matrix_t *summ, int source_gno );

static void my_prepare_cols( summ_matrix_t *summ, int col_gno, int col_gno_lim ) 
{
  int i;
  DATA(summ)->col_inprogress_first = col_gno;
  DATA(summ)->col_inprogress_lim = col_gno_lim;

  for( i = col_gno; i < col_gno_lim; i++ ) {
    assert( DATA(summ)->cols[i]->cell_top == NULL );
    assert( DATA(summ)->cols[i]->cell_bot == NULL );
    assert( DATA(summ)->cols[i]->word_count == 0 );
    assert( DATA(summ)->cols[i]->construction_inprogress == FALSE );
    assert( DATA(summ)->cols[i]->construction_complete == FALSE );
    assert( DATA(summ)->cols[i]->overly_popular == FALSE );

    /* We do *not* construct cells at this point; we do that on demand, 
     * as additions to the matrix are needed.
     */
  }
}

static remset_as_summary_t* allocate_remset_as_summary(int gen, int poplimit);

static void  create_refactored_from_memmgr( summ_matrix_t *sm,
                                            int popularity_limit )
{
  sm->remset_summaries = 0;
  sm->remset_summaries_count = 0;
  sm->summarized_genset_valid = FALSE;

  /* data->summaries->region_count = data->region_count; */
  sm->popularity_limit = popularity_limit;

  int len = sm->collector->remset_count+1;
  int i;
  sm->remset_summaries = 
    (remset_as_summary_t**)must_malloc(len*sizeof(remset_as_summary_t*));
  sm->remset_summaries[0] = NULL;
  for( i = 1; i < len; i++ ) {
    sm->remset_summaries[i] = 
      allocate_remset_as_summary( i, popularity_limit );
  }
  sm->remset_summaries_count = len;
}

summ_matrix_t *
create_summ_matrix( gc_t *gc, int first_gno, int initial_num_rgns, 
                    double c, double p, int popularity_limit )
{
  summ_matrix_t *sm; 
  summ_matrix_data_t *data;
  int num_rgns = initial_num_rgns;
  int num_under_construction = (int)(num_rgns * c);


  assert( 0.0 < c && c <= 1.0 );
  assert( p >= 2.0 );

  sm = (summ_matrix_t*)must_malloc( sizeof( summ_matrix_t ));
  data = (summ_matrix_data_t*)must_malloc( sizeof( summ_matrix_data_t ));

  data->c = c;
  data->p = p;
  data->first_gno = first_gno;
  data->num_rgns = num_rgns;

  data->rows = alloc_rows( first_gno, num_rgns );
  data->cols = alloc_cols( first_gno, num_rgns );

  data->col_complete_first = -1;
  data->col_complete_lim = -1;

  data->col_inprogress_first = -1;
  data->col_inprogress_lim = -1;

  sm->collector = gc;
  sm->data = data;

  my_prepare_cols( sm, first_gno, first_gno + num_under_construction );

  create_refactored_from_memmgr( sm, popularity_limit );

  print_matrix( sm );

  return sm;
}

void sm_expand_gnos( summ_matrix_t *summ, int fresh_gno )
{
  print_matrix( summ );
  assert(FALSE);
  print_matrix( summ );
}

void sm_prepare_cols( summ_matrix_t *summ, int col_gno, int col_gno_lim )
{
  print_matrix( summ );
  my_prepare_cols( summ, col_gno, col_gno_lim );
  print_matrix( summ );
}

void sm_dispose_cols( summ_matrix_t *summ, int col_gno, int col_gno_lim )
{
  print_matrix( summ );
  assert(FALSE);
  print_matrix( summ );
}

void sm_construction_progress( summ_matrix_t *summ, 
                               int* word_countdown,
                               int* object_countdown )
{
  print_matrix( summ );
  /* This breaks this specification, since it
   * - assumes that it always starts from a blank slate, and 
   * - ignores the word and object scan limiting parameters.
   * 
   * But I want to get something up and running using a
   * workable interface *now*
   */
  int i;
  int fst, lim; 
  int remset_count;
  gset_t genset;

  fst = DATA(summ)->col_inprogress_first;
  lim = DATA(summ)->col_inprogress_lim;
  genset = gset_range( fst, lim );
  remset_count = summ->collector->remset_count;
  /* Construct 
   *   { x | x in Union all remsets | x has reference into [fst,lim) }
   */ 
  for( i = 1; i < remset_count ; i++ ) {
    assert(FALSE);
  }
  assert(FALSE);
  print_matrix( summ );
}

void sm_enumerate_row( summ_matrix_t *summ,
                       int row_gno, 
                       bool (*scanner)(word loc, void *data),
                       void *data )
{
  print_matrix( summ );
  assert(FALSE);
  print_matrix( summ );
}

void sm_enumerate_col( summ_matrix_t *summ, 
                       int col_gno, 
                       bool (*scanner)(word loc, void *data),
                       void *data )
{
  print_matrix( summ );
  assert(FALSE);
  print_matrix( summ );
}


void sm_add_entry( summ_matrix_t *summ, word source_obj, int target_gno )
{
  print_matrix( summ );
  assert(FALSE);
  print_matrix( summ );
}

void sm_next_summary( summ_matrix_t *summ, summary_t *column ) 
{
  print_matrix( summ );
  assert(FALSE);
  print_matrix( summ );
}

/* Functions below are for use when this structure is being used in a
 * concurrent (ie multi-threaded) regional collector. */

void sm_add_entry_concurrent( summ_matrix_t *summ, 
                              word source_obj, 
                              int target_gno )
{
  assert(FALSE);
}


void sm_construction_concurrent( summ_matrix_t *summ,
                                 int grain_scan_words,
                                 int grain_scan_objects )
{
  assert(FALSE);
}


void sm_interrupt_construction( summ_matrix_t *summ )
{
  assert(FALSE);
}

/* below refactored from memmgr.c */

typedef struct remset_summary_data remset_summary_data_t;
struct remset_summary_data {
  /* _current_ representation: summaries[g] is non-null ==> (g in genset && summaries[g] non-empty). */
  gset_t genset;
  remset_as_summary_t **remset_summaries;
  int objects_visited;
  int objects_added;
  int words_added;
};

static struct {
  remset_t **elem;
  int len;
} remset_pool = { NULL, 0 };

static remset_t* grab_from_remset_pool() 
{
  annoyingmsg("            grab_from_remset_pool");
  { int i; 
    for (i = 0; i < remset_pool.len; i++) {
      if (remset_pool.elem[i] != NULL) {
        remset_t *rtn;
        rtn = remset_pool.elem[i];
        remset_pool.elem[i] = NULL;
        assert2(rtn->live == 0);
        return rtn;
      }
    }
  }
  /* if we get here, then all remsets are in use and we need to expand
   * the pool. */
  {
    int newlen = remset_pool.len + 1;
    remset_t **elem = (remset_t**)must_malloc(newlen*sizeof(remset_t*));
    free(remset_pool.elem);
    remset_pool.elem = elem;
    remset_pool.len = newlen;
    /* we do not store the new remset in the pool yet; that will
     * happen when it is freed. */
    memset(elem, 0, newlen*sizeof(remset_t*));
    return create_remset( 0, 0 );
  }
}

static void return_to_remset_pool( remset_t *rs ) 
{
  int i;
  annoyingmsg("            return_to_remset_pool");
  assert2(rs->live == 0);
  for (i = 0; i < remset_pool.len; i++) {
    if (remset_pool.elem[i] == NULL) {
      remset_pool.elem[i] = rs;
      return;
    }
  }
  consolemsg("no NULL entries in a pool of length %d", remset_pool.len);
  assert(0); /* (should never get here) */
}


static remset_as_summary_t* allocate_remset_as_summary(int gen, int poplimit) 
{
  remset_as_summary_t *rast;
  rast = (remset_as_summary_t*)must_malloc(sizeof(remset_as_summary_t));
  rast->sum_remset = NULL;
  rast->gen        = gen;
  rast->valid      = FALSE;
  rast->words      = 0;
  rast->max_words  = poplimit;
  return rast;
}

static void add_object_to_sum_rs( remset_as_summary_t *rs_sum, 
                                  int gen, 
                                  word ptr ) 
{
  if (rs_sum->words <= rs_sum->max_words) {
    if (rs_sum->sum_remset == NULL) {
      rs_sum->sum_remset = grab_from_remset_pool();
    }
    if (!rs_isremembered( rs_sum->sum_remset, ptr )) {
      if (tagof(ptr) == PAIR_TAG) {
        rs_sum->words += 2;
      } else {
        rs_sum->words += sizefield( *ptrof(ptr) ) / 4;
      }

      if (rs_sum->words > rs_sum->max_words) {
        annoyingmsg("summary for rgn %d overflowed on 0x%08x (%d): %d max %d",
                    gen, ptr, gen_of(ptr), rs_sum->words, rs_sum->max_words);
        rs_clear( rs_sum->sum_remset );
        return_to_remset_pool( rs_sum->sum_remset );
        rs_sum->sum_remset = NULL;
        rs_sum->valid = FALSE;
      } else {
        rs_add_elem( rs_sum->sum_remset, ptr );
      }
    }
  }
}

/* XXX stolen from remset.c; should be factored out to somewhere else */
static word retagptr( word w ) 
{
  if (tagof(w) == 0) {
    switch (header(*(word*)w)) {
    case VEC_HDR :
      return (word)tagptr( w, VEC_TAG );
    case BV_HDR : 
      return 0; /* signal that entry should be removed! */
    case PROC_HDR :
      return (word)tagptr( w, PROC_TAG );
    default:
      panic_abort( "memmgr.c: word is nonptr." );
    }
  } else {
    return w;
  }
}

void sm_add_ssb_elems_to_summary( summ_matrix_t *summ, word *bot, word *top, int g_rhs )
{
  word *p, *q, w;

  if ( summ->summarized_genset_valid
       && gset_memberp( g_rhs, summ->summarized_genset )) {
    p = bot; 
    q = top; 
    while (q > p) {
      q--;
      w = *q;
      w = retagptr(w);
      if (!w) 
        continue;
      add_object_to_sum_rs( summ->remset_summaries[ g_rhs ], g_rhs, w );
    }
  }
}

static void add_object_to_summary( remset_summary_data_t *remsum, int gen, word ptr ) 
{
  assert2( gen_of(ptr) != gen );
  add_object_to_sum_rs( remsum->remset_summaries[ gen ], gen, ptr );
}

static bool scan_object_for_remset_summary( word ptr, void *data, unsigned *count )
{
  word *loc = ptrof(ptr);
  word scanned = 0;
  bool do_enqueue = FALSE;
  remset_summary_data_t *remsum = (remset_summary_data_t*)data;
  gset_t genset = remsum->genset;
  int mygen = gen_of(ptr); 
  /* XXX fixme: the way remset's are scanned, we should not need to reextract this */

  static const bool instrumented = FALSE;

  if (instrumented) {
    annoyingmsg("scan_object_for_remset_summary( 0x%08x (%d), data, count )",
                ptr, gen_of(ptr) );
    switch (genset.tag) {
    case gs_nil: assert(0);
    case gs_singleton: 
      annoyingmsg("  for pointers into { %d }", genset.g1 );
      break;
    case gs_range: 
      annoyingmsg("  for pointers into [%d,%d)", genset.g1, genset.g2 );
      break;
    }
  }
  if (tagof( ptr ) == PAIR_TAG) {
    /* handle car */
    if (isptr(*loc)) {
      int gen = gen_of(*loc);
      if (instrumented) 
        annoyingmsg("scan_object_for_remset_summary "
                    "pair car: 0x%08d (%d)", *loc, gen);
      if (mygen != gen && gset_memberp(gen,genset)) {
        do_enqueue = TRUE;
        add_object_to_summary( remsum, gen, ptr );
      }
    }
    ++loc;
    /* handle cdr */
    if (isptr(*loc)) {
      int gen = gen_of(*loc);
      if (instrumented) 
        annoyingmsg("scan_object_for_remset_summary "
                    "pair cdr: 0x%08d (%d)", *loc, gen);
      if (mygen != gen && gset_memberp(gen,genset)) {
        do_enqueue = TRUE;
        add_object_to_summary( remsum, gen, ptr );
      }
    }
    scanned = 2;
  } else { /* vector or procedure */
    word words;
    assert( (tagof(ptr) == VEC_TAG) || (tagof(ptr) == PROC_TAG) );
    words = sizefield( *loc ) / 4;
    scanned = words;
    while (words--) {
      ++loc;
      if (isptr(*loc)) {
        int gen = gen_of(*loc);
        if (instrumented) 
          annoyingmsg("scan_object_for_remset_summary "
                      "vecproc : 0x%08d (%d)", *loc, gen);
        if (mygen != gen && gset_memberp(gen,genset)) {
          do_enqueue = TRUE;
          add_object_to_summary( remsum, gen, ptr );
        }
      }
    }
  }
  
  remsum->objects_visited += 1;
  if (do_enqueue) {
    remsum->objects_added += 1;
    remsum->words_added += scanned;
  }

  *count += scanned;
  return TRUE; /* don't remove entries from the remembered set we are summarizing! */  
}

void sm_build_remset_summaries( summ_matrix_t *summ, gset_t genset )
{
  remset_summary_data_t remsum;
  int i;
  int remset_count = summ->collector->remset_count;
  int summ_len = gset_max_elem(genset); /* (some entries can be null) */

  /* XXX potentially assert summ->summarized_genset is nullset */

  remsum.genset = genset;
  remsum.remset_summaries = summ->remset_summaries;
  remsum.objects_visited = 0;
  remsum.objects_added = 0;
  remsum.words_added = 0;

  /* Optimistically assume that summarization will succeed for all
   * elems of genset; if one of them overflows, it will be
   * responsibility of scan_object_for_remset_summary to set valid
   * field to FALSE.
   */
  for( i=0 ; i < summ->remset_summaries_count; i++ ) {
    if (gset_memberp( i, genset )) {
      summ->remset_summaries[i]->valid = TRUE;
      summ->remset_summaries[i]->words = 0;
      /* Construction assumes that summaries start off empty. */
      assert2( remsum.remset_summaries[ i ]->sum_remset == NULL ||
               remsum.remset_summaries[ i ]->sum_remset->live == 0);
    }
  }

  for(i = 1; i < remset_count; i++) {
    /* TODO: use rs_enumerate_partial here? XXX */
    rs_enumerate( summ->collector->remset[ i ], 
		  scan_object_for_remset_summary,
		  (void*) &remsum );
    rs_enumerate( summ->collector->major_remset[ i ], 
		  scan_object_for_remset_summary,
		  (void*) &remsum );
  }
  if (genset.tag == gs_singleton) {
    remset_t *rs = remsum.remset_summaries[genset.g1]->sum_remset;
    assert( genset.g1 < summ->remset_summaries_count );
  } else if (genset.tag == gs_range ) {
    remset_t *rs1, *rs2;
    assert( genset.g2 <= summ->remset_summaries_count );
    rs1 = remsum.remset_summaries[genset.g1]->sum_remset;
    rs2 = remsum.remset_summaries[genset.g2-1]->sum_remset;
  } else { assert(0); }

  { /* XXX review me XXX */
    summ->summarized_genset = genset;
    summ->summarized_genset_valid = TRUE;
  }
}

static void* verify_summaries_msgc_fcn( word obj, word src, void *data )
{
  summ_matrix_t *summ = (summ_matrix_t*)data;
  int src_gen, tgt_gen;

  if (isptr(src) && isptr(obj) &&
      ((src_gen = gen_of(src)) != (tgt_gen = gen_of(obj))) &&
      ! gc_is_nonmoving( summ->collector, tgt_gen )) {
    assert( src_gen >= 0 );
    if (src_gen > 0) {
      assert( *summ->collector->ssb[src_gen]->bot == *summ->collector->ssb[src_gen]->top );
      assert( *summ->collector->ssb[tgt_gen]->bot == *summ->collector->ssb[tgt_gen]->top );
      if (summ->summarized_genset_valid &&
          gset_memberp( tgt_gen, summ->summarized_genset ) &&
          summ->remset_summaries[ tgt_gen ]->valid ) {
        assert( (summ->remset_summaries[ tgt_gen ]->sum_remset) != NULL );
        assert( rs_isremembered( summ->remset_summaries[ tgt_gen ]->sum_remset, src ));
      }
    }
  }
  return data;
}

struct verify_summaries_remset_fcn_data {
  msgc_context_t *conserv_context;
  msgc_context_t *aggress_context;
  int summary_for_region; 
};

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

static bool verify_summaries_remset_fcn( word obj, 
					 void *the_data, unsigned *stats )
{
  struct verify_summaries_remset_fcn_data *data;
  data = (struct verify_summaries_remset_fcn_data*)the_data;
  /* Any object in a summary should be reachable from the 
   * union of roots+remsets */
  annoyingmsg( "VERIFY SUMM REMS 0x%08x (%d) marked {agg: %s, con: %s}", 
	       obj, gen_of(obj), 
	       msvfy_object_marked_p( data->aggress_context, obj )?"Y":"N", 
	       msvfy_object_marked_p( data->conserv_context, obj )?"Y":"N" );
  assert( msvfy_object_marked_p( data->conserv_context, obj ));
  assert( gen_of(obj) != data->summary_for_region );
  return TRUE;
}

void sm_verify_summaries_via_oracle( summ_matrix_t *summ )
{
  msgc_context_t *conserv_context;
  msgc_context_t *aggress_context;
  int marked, traced, words_marked;

  if (summ->summarized_genset_valid) {
    conserv_context = msgc_begin( summ->collector );
    msvfy_set_object_visitor( conserv_context, 
                              verify_summaries_msgc_fcn, 
                              summ );
    /* (useful to have a pre-pass over reachable(roots) so that the
       stack trace tells you whether a problem is due solely to a
       reference chain that somehow involves remembered sets.) */
    msvfy_mark_objects_from_roots( conserv_context );
    /* Summaries are based on (conservative) info in remsets; 
       therefore they may have references to "dead" objects 
       that would not be identified as such if we used 
       only msgc_mark_objects_from_roots
    */
    msvfy_mark_objects_from_roots_and_remsets( conserv_context );

    /* a postpass over the summaries to make sure that their contents
       are sane.
    */
    {
      struct verify_summaries_remset_fcn_data data;
      int i;
      msgc_context_t *aggress_context;
      aggress_context = msgc_begin( summ->collector );
      msvfy_mark_objects_from_roots( aggress_context );
      data.conserv_context = conserv_context;
      data.aggress_context = aggress_context;
      for (i = 0; i < summ->collector->remset_count; i++) {
	if (gset_memberp( i, summ->summarized_genset )){
	  assert( summ->remset_summaries[i] != NULL);

	  assert( i < summ->remset_summaries_count );
	  /* we do not grab a remset_t if no entries are added, 
	     so this is a guard rather than an assertion. */
	  if ( summ->remset_summaries[i]->sum_remset != NULL) {
	    data.summary_for_region = i;
	    rs_enumerate( summ->remset_summaries[ i ]->sum_remset, 
	                  verify_summaries_remset_fcn, 
	                  &data );
	  }
	}
      }
      msgc_end( aggress_context );
    }
    msgc_end( conserv_context );
  }
}

static bool scan_refine_remset( word loc, void *data, unsigned *stats )
{
  smircy_context_t *context = (smircy_context_t*)data;
  if (smircy_object_marked_p( context, loc )) {
    return TRUE;
  } else {
    return FALSE;
  }
}

void sm_refine_summaries_via_marksweep( summ_matrix_t *summ ) 
{
  smircy_context_t *context;
  context = summ->collector->smircy;

  /* XXX refining the summaries as well as the remsets based on the
     marksweep info.  This may or may not be necessary in an improved
     version of the refinement code.
  */
  if (summ->summarized_genset_valid) {
    int i;
    for (i = 0; i < summ->collector->remset_count; i++) {
      if (gset_memberp( i, summ->summarized_genset)) {
        assert( summ->remset_summaries[i] != NULL);
        if (summ->remset_summaries[i]->sum_remset != NULL) {
          rs_enumerate( summ->remset_summaries[i]->sum_remset,
                        scan_refine_remset, 
                        context );
        }
      }
    }
  }
}

int sm_summarized_live( summ_matrix_t *summ, int rgn ) 
{
  bool rgn_summarized;
  int rgn_summarized_live;

  rgn_summarized = 
    summ->summarized_genset_valid && 
    gset_memberp( rgn, summ->summarized_genset );
  if (rgn_summarized) {
    if (summ->remset_summaries[ rgn ]->sum_remset == NULL ) {
      rgn_summarized_live = 0;
    } else {
      rgn_summarized_live = 
        summ->remset_summaries[ rgn ]->words;
    }
  } else {
    rgn_summarized_live = -summ->remset_summaries[ rgn ]->words - 1;
  }
  return rgn_summarized_live;
}

void sm_invalidate_summaries( summ_matrix_t *summ ) 
{
  int i;
  for (i=1; i<summ->remset_summaries_count; i++) {
    assert2( summ->remset_summaries[i]->sum_remset == NULL );
  }
  summ->summarized_genset_valid = FALSE;
}

void sm_clear_summary( summ_matrix_t *summ, int rgn_next )
{
  /* clear the summary that guided this collection. */
  {
    remset_t *rs = summ->remset_summaries[ rgn_next ]->sum_remset;
    if (rs != NULL) { 
      rs_clear( rs );
      return_to_remset_pool( rs );
      summ->remset_summaries[ rgn_next ]->sum_remset = NULL;
      summ->remset_summaries[ rgn_next ]->words = 0;
    }
    summ->summarized_genset = 
      gset_remove( rgn_next, summ->summarized_genset);

    { 
      gset_t genset = summ->summarized_genset;
      if (genset.tag == gs_singleton) {
        assert(genset.g1 <  summ->remset_summaries_count);
      } else if (genset.tag == gs_range) {
        assert(genset.g2 <= summ->remset_summaries_count);
      } else { 
        assert(0); 
      }
    }
  }
}

struct filter_objects_from_sum_remset_data {
  gc_t *gc;
  int gen; /* collected gen */
};
static bool filter_objects_from_sum_remset( word ptr, 
                                            void *the_data, 
                                            unsigned *count )
{
  struct filter_objects_from_sum_remset_data *data;
  data = (struct filter_objects_from_sum_remset_data *)the_data;
  assert(isptr(ptr));
  if (gen_of(ptr) == data->gen) {
    return FALSE;
  } else {
    return TRUE;
  }
}
void sm_clear_contribution_to_summaries( summ_matrix_t *summ, int rgn_next ) 
{
  /* clear contribution of rgn_next to all summaries */
  { 
    int i;
    remset_t *rs;
    struct filter_objects_from_sum_remset_data data;
    data.gen = rgn_next;
    for(i=0; i<summ->remset_summaries_count; i++ ) {
      if (i == rgn_next) 
        continue; /* the entire summary for i is cleared down below */
      /* (and shouldn't summary have nothing from region_i anyway?) */
      annoyingmsg( "clear summary [%d] of entries from %d", i, rgn_next );
      if (gset_memberp( i, summ->summarized_genset ) &&
          summ->remset_summaries[ i ]->sum_remset != NULL) {
        rs = summ->remset_summaries[ i ]->sum_remset;
        rs_enumerate( rs, 
                      filter_objects_from_sum_remset, 
                      &data );
      }
    }
  }
}

struct fold_from_nursery_data {
  int gen;
  remset_t *rs;
};
static bool fold_from_nursery( word ptr, void *my_data, unsigned *count ) {
  struct fold_from_nursery_data *data;
  data = (struct fold_from_nursery_data*)my_data;
  if (gen_of(ptr) != data->gen) {
    rs_add_elem( data->rs, ptr );
  }
  return TRUE;
}

void sm_fold_in_nursery_and_init_summary( summ_matrix_t *summ, 
                                          remset_t *nursery_rs, 
                                          int next_summ_idx, 
                                          summary_t *summary )
{
  /* XXX for now, fold the nursery remset into the remset
   * we're using for this major collection.  Better long term
   * approach may be to do two separate scans rather than a
   * fold-then-scan-combined XXX */
  {
    remset_t *rs = summ->remset_summaries[ next_summ_idx ]->sum_remset;
    if (rs != NULL) {
      struct fold_from_nursery_data data;
      data.gen = next_summ_idx;
      data.rs  = rs;
      rs_enumerate( nursery_rs, fold_from_nursery, &data );
    } else {
      rs = nursery_rs;
    }
    annoyingmsg( "construct rs (%d) summary", rs->live);
    rs_init_summary( rs, -1, summary );
  }
}

bool sm_majorgc_permitted( summ_matrix_t *summ, int rgn_next )
{
  return (summ->remset_summaries[ rgn_next ]->words <= summ->popularity_limit);
}

void sm_expand_summary_gnos( summ_matrix_t *summ, int fresh_gno ) 
{
  /* check that inserting fresh_gno does not upset
     the existing summarization data (that is, that there are *no*
     summaries pointing into regions >= fresh_gno
  */
  if (summ->summarized_genset_valid) {
    assert( ! gset_min_elem_greater_than( summ->summarized_genset, 
                                          fresh_gno-1 ));
  }

  /* even though inserting the fresh gno will not upset the 
     summarization data, we still need to expand the 
     gc fields related to summaries (because right now 
     I keep that structure proportional to the number of 
     regions
  */
  {
    int len = summ->remset_summaries_count+1;
    int i;
    remset_as_summary_t **remset_summaries;
    remset_summaries = 
      (remset_as_summary_t**)must_malloc(len*sizeof(remset_as_summary_t*));
    remset_summaries[0] = NULL;
    for( i = 1; i < fresh_gno; i++ )
      remset_summaries[i] = summ->remset_summaries[i];
    remset_summaries[ fresh_gno ] = 
      allocate_remset_as_summary( fresh_gno, summ->popularity_limit );
    for( i = fresh_gno+1; i < len; i++ )
      remset_summaries[i] = summ->remset_summaries[i-1];
    free(summ->remset_summaries);
    summ->remset_summaries = remset_summaries;
    summ->remset_summaries_count = len;
  }
}

void sm_points_across_callback( summ_matrix_t *summ, word lhs, int g_rhs )
{
  if ( summ->summarized_genset_valid &&
       gset_memberp( g_rhs, summ->summarized_genset )) {
    add_object_to_sum_rs( summ->remset_summaries[ g_rhs ], 
                          g_rhs, lhs );
  }
}

