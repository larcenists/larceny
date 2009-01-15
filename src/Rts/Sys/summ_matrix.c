/* Copyright 2008 Felix S Klock II              -*- indent-tabs-mode: nil -*-
 * 
 * $Id: $
 *
 * Summarization set matrix implementation.
 * 
 * The regions are partitioned into three parts: 
 * { uninteresting, summarized, summarizing },
 * aka "UNSZ", "SZED", and "SZING".
 * 
 * The matrix maps (SZED u SZING)  -> Col, 
 * where a Col (column) is a list of Cells and a Hashset.
 * 
 * interp: the union of a Col's Cells and its Hashset are the region's
 * points-into summary.
 * 
 * For any Col, a word may occur at most once in its Cells.  There
 * *can* be overlap between a Col's Cells and its Hashset.  If one
 * must ensure that an iteration over a Col does not repeat words, one
 * can filter the Cells iteration by the contents of the Hashset (at
 * the expense of paying for hash-lookups during the cell iteration).
 * 
 */ 

#define GC_INTERNAL

#define dbmsg( format, args... ) if (0) consolemsg( format, ## args )

#include <stdio.h>

#include "larceny.h"
#include "gclib.h"
#include "summ_matrix_t.h"
#include "remset_t.h"
#include "msgc-core.h"
#include "gc_t.h"
#include "seqbuf_t.h"
#include "smircy.h"

#define DEFAULT_OBJS_POOL_SIZE 256

#define EXPORT

/* As this gets larger, more check_rep calls are included.
 * Rough intution is that degree 1 maintains linear growth,
 * degree 2 introduces quadratic, etc, though this is not necessarily
 * adhered to. */
#define CHECK_REP_DEGREE 0

#define FREE_CELL_ASSERTS_UNREACHABLE 0

#define ADD_TO_SUMAR_ASSERTS_UNIQ_ENQ 0

#define SUMMARIZE_KILLS_RS_ENTRIES 0

#define MAINTAIN_REDUNDANT_RS_AS_SM_REP 0
#define USE_REDUNDANT_RS_AS_SM_REP 0

typedef struct objs_pool objs_pool_t;
typedef struct summ_cell summ_cell_t;
typedef struct summ_row summ_row_t;
typedef struct summ_col summ_col_t;
typedef struct summ_matrix_data summ_matrix_data_t;

struct objs_pool {
  word *bot;
  word *top;
  word *lim;
  objs_pool_t *next;
};
/* REP INV: bot < lim && bot <= top <= lim.
 * interpretation: each objs_pool is filled with objects from
 * [bot,top) if next is non-null, it holds prior insertions.
 * ABS INV: all inserted objects are distinct. */

struct summ_cell {
  objs_pool_t *objects;
  int source_gno;
  int target_gno;
  summ_cell_t *prev_row; /* left elem; sentinel cell at end */
  summ_cell_t *next_row; /* right elem; sentinel cell at end */
  summ_cell_t *prev_col; /* up   elem; sentinel cell at end */
  summ_cell_t *next_col; /* down elem; sentinel cell at end */
  int dbg_id;             /* uid (for debug output) */
};
static int summ_cell_next_dbg_id = 101;
static int next_dbg_id() {
  int ret = summ_cell_next_dbg_id;
  summ_cell_next_dbg_id += 1;
  return ret;
}
struct summ_row {
  summ_cell_t *cell_lft; /* sentinel node */
  summ_cell_t *cell_rgt; /* end of linked-list (sentinel if row empty) */
  int source_gno;
};

struct summ_col {
  summ_cell_t *cell_top; /* sentinel node */
  summ_cell_t *cell_bot; /* end of linked-list (sentinel if col empty) */
  int target_gno;
  int summarize_word_count; /* contribution from summarization */
  int writebarr_word_count; /* contribution from mutator (via write-barrier) */
  int collector_word_count; /* contribution from cheney (obj forwarding) */
  bool overly_popular;
  bool construction_inprogress;
  bool construction_complete;

  remset_t *sum_mutator; /* objects inserted via write-barrier */
  /* XXX [pnkfelix] Tue Jan 6 16:20:31 EST 2009
   * Why is sum_mutator maintained per-column rather than globally? 
   * 
   * Because:
   *  1. I don't want to spend time during a gc scanning irrelevant
   *     objects, and
   *  2. because I would not know when it was safe to remove the elements 
   *     that point into region R after collection of R based solely
   *     a single sum_mutator shared between all the remsets
   * 
   * XXX okay, well, how can I avoid spending O(#cols) time looking at
   * every sum_mutator during sm_clear_contribution_to_summaries ?
   * I'm punting on this question in the short term.
   * 
   * Note that such a scan is either: 
   * - looking at a dense array of mostly small sum_mutators, or
   * - looking at a spare array of dense sum_mutators
   * (we can't encounter a dense array of dense sum_mutators, because
   * we'll force gc's before that many mutations occur).
   * In the former case, a scan is unavoidable, but also acceptable (it seems to me).
   * Therefore the scan in the latter case is also acceptable.
   * 
   * If #cols can grow to the point that the scan is unacceptable,
   * that would mean that we can never have a dense array of non-empty
   * sum_mutators (otherwise by definition the scan must be
   * acceptable) -- in that situation, can work around problem by
   * cons'ing up a linked list of all non-empty sum_mutators.  (I may
   * consider putting that in anyway...)
   */
};

#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP 
struct remset_as_summary { 
  remset_t *sum_remset;
  int       gen;
  bool      valid;
  int       summarize_words; /* contribution from summarization */
  int       writebarr_words; /* contribution from mutator (via wb) */
  int       collector_words; /* contribution from cheney (obj forw'ing) */
  int       max_words;
};
typedef struct remset_as_summary remset_as_summary_t;
#endif 

struct summ_matrix_data {
  double coverage;
  double p;
  int entries_per_objs_pool_segment;

  summ_row_t **rows;
  summ_col_t **cols;
  int num_cols;
  int num_rows;

  int col_complete_first;   /* completed summmary, or -1 if none */
  int col_complete_lim;
  int col_inprogress_first; /* under-construction, or -1 if none */
  int col_inprogress_lim;

  /* TODO: use something like the below with concert with summ_cell to
   * cache summ_cell_t lookups. */
  struct {
    int cached_gno;
    summ_cell_t **table; /* maps src_gno -> cell or NULL */
    int table_len; /* may be redundant, given gensets below... */

    int last_src_gno;
    int last_tgt_gno;
    summ_cell_t* last_cell;
    bool last_cell_valid;
  } row_cache;

  /* refactoring */
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
  remset_as_summary_t **remset_summaries; /* points-into summaries */
#endif 
  int       remset_summaries_count;
  bool      summarized_genset_valid;
  gset_t    summarized_genset;
  int       popularity_limit;   /* Maximum summary size allowed (in words) */

  remset_t *nursery_remset;     /* Points-into remset for the nursery. */
};

#define DATA(sm)                ((summ_matrix_data_t*)(sm->data))

static bool rsenum_assert_memberp( word loc, void *data, unsigned *stats) 
{
  remset_t *other = (remset_t*) data;
  assert( other != NULL );
  assert( rs_isremembered(other, loc) );
  return TRUE; /* don't remove the object! */
}

static bool rsenum_add_elem( word loc, void *data, unsigned *stats )
{
  remset_t *other = (remset_t*) data;
  rs_add_elem( other, loc );
  return TRUE;
}

static void assert_subseteq( remset_t* a, remset_t *b )
{
  if (a != NULL) {
    rs_enumerate( a, rsenum_assert_memberp, b );
  }
}

static objs_pool_t *allocate_pool_segment( unsigned entries_per_pool_segment ) 
{
  objs_pool_t *p;
  word *heapptr;
  p = (objs_pool_t*) must_malloc( sizeof(objs_pool_t) );
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
  int dbg_id;

  dbg_id = next_dbg_id();
  dbmsg("   make_cell( src: %d, tgt: %d, entries: %d ) => id:%d",
        source_gno, target_gno, entries_per_pool_segment, dbg_id );

  e = (summ_cell_t*) must_malloc( sizeof(summ_cell_t) );
  e->objects = allocate_pool_segment( entries_per_pool_segment );
  e->source_gno = source_gno;
  e->target_gno = target_gno;
  e->dbg_id = dbg_id;
  return e;
}

static void free_cell( summ_cell_t *e, int entries_per_pool_segment ) 
{
  objs_pool_t *p; 
  objs_pool_t *n;

  dbmsg("   free_cell( id:%d src: %d tgt: %d, entries: %d )",
        e->dbg_id, e->source_gno, e->target_gno, 
        entries_per_pool_segment );

  p = e->objects;
  while (p != NULL) {
    gclib_free( p->bot, entries_per_pool_segment*sizeof(word) );
    n = p->next; /* save across free(p) call */
    free(p);
    p = n;
  }
  free(e);
}

static summ_cell_t* make_sentinel_cell( int src_gno, int tgt_gno ) 
{
  summ_cell_t *e;
  assert( src_gno == -1 || tgt_gno == -1 );
  e = (summ_cell_t*) must_malloc( sizeof(summ_cell_t) );
  e->objects = NULL;
  e->source_gno = src_gno;
  e->target_gno = tgt_gno;
  e->dbg_id = next_dbg_id();
  e->prev_row = e; /* complete       */
  e->next_row = e; /*  horizontally  */
  e->prev_col = e; /*   and          */
  e->next_col = e; /*    vertically! */
  return e;
}

static int pool_count_objects( objs_pool_t *objects ) 
{
  /* The pool for any one cell shouldn't get too deep. */
  if (objects == NULL) {
    return 0;
  } else {
    return (objects->top - objects->bot) + 
      pool_count_objects( objects->next );
  }
}

static char* row_templ = "%-5s";
static char* col_templ = "%-5s";
static char* cell_templ = "%-27s";

static void print_cell( summ_cell_t *cell ) 
{
  printf("[%3d(%2d,%2d)%-4d,l%3d,r%3d,u%3d,d%3d] ",
         cell->dbg_id, cell->source_gno, cell->target_gno, 
         pool_count_objects( cell->objects ),
         cell->prev_row->dbg_id, 
         cell->next_row->dbg_id, 
         cell->prev_col->dbg_id, 
         cell->next_col->dbg_id);
}
static void print_skip_cell() 
{
  printf(cell_templ, "");
}

static void print_row_header( summ_row_t *row ) 
{
  printf(row_templ, "ROW");
}
static void print_col_header( summ_col_t *col )
{
  printf(col_templ, "COL");
}

static void print_row( summ_row_t *row ) 
{
  summ_cell_t *sent = row->cell_lft;
  summ_cell_t *cell = sent;
  print_row_header( row );
  do {
    print_cell( cell );
    cell = cell->next_row;
  } while ( cell != sent );
  printf("\n");
}

static void print_matrix_via_rows( summ_matrix_t *sm ) 
{
  int i;
  for (i = 0; i < DATA(sm)->num_rows; i++) {
    print_row( DATA(sm)->rows[i] );
  }
  printf("\n");
}

static void print_col( summ_col_t *col ) 
{
  summ_cell_t *sent = col->cell_top;
  summ_cell_t *cell = sent;

  assert( col->cell_bot->next_col == col->cell_top );

  print_col_header( col );
  do {
    print_cell( cell );
    cell = cell->next_col;
  } while ( cell != sent );
  printf("\n");
}

static void print_matrix_via_cols( summ_matrix_t *sm ) 
{
  /* 
   * Note that this prints sideways; you can cock your head 90 degrees
   * to your left to read its output.
   *
   * So if the rows print as (roughly):
   *   r1    A   B       D
   *   r2        F   G   H
   *   r3    I   J
   *   r4        K   L   M
   * 
   * then the cols print as (roughly):
   *   c4    D   H       M
   *   c3        G       L
   *   c2    B   F   J   K
   *   c1    A       I
   */

  int i;
  int num_cols = DATA(sm)->num_cols;

  /* traverse columns from right to left (see above pic) */
  for (i = num_cols-1; i >= 0; i--) {
    print_col( DATA(sm)->cols[i] );
  }
  printf("\n");
}

static void print_matrix( char *ctxt, summ_matrix_t *sm ) 
{
  printf("%s\n", ctxt);
  print_matrix_via_rows( sm );
  print_matrix_via_cols( sm );
}

static summ_row_t* alloc_row(int source_gno)
{
  summ_row_t *row;
  summ_cell_t *sntl;

  sntl = make_sentinel_cell( source_gno, -1 );
  row = (summ_row_t*)must_malloc( sizeof( summ_row_t ));
  row->cell_lft = sntl;
  row->cell_rgt = sntl;
  row->source_gno = source_gno;
  return row;
}

static summ_row_t** alloc_rows(int num_rows) {
  summ_row_t** rows;
  int i;
  int gno = 0;
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
  summ_cell_t *sntl;
  sntl = make_sentinel_cell( -1, target_gno );
  col = (summ_col_t*)must_malloc( sizeof( summ_col_t ));
  col->cell_top = sntl;
  col->cell_bot = sntl;
  col->summarize_word_count = 0;
  col->writebarr_word_count = 0;
  col->collector_word_count = 0;
  col->target_gno = target_gno;
  col->overly_popular = FALSE;
  col->construction_inprogress = FALSE;
  col->construction_complete = FALSE;
  col->sum_mutator = NULL; /* Pooled remsets */
  assert( col->cell_bot->next_col == col->cell_top );
  return col;
}

static summ_col_t** alloc_cols(int num_cols) {
  summ_col_t** cols;
  int i;
  int gno = 0;
  cols = (summ_col_t**)must_malloc( num_cols*sizeof( summ_col_t* ));
  for(i = 0; i < num_cols; i++) {
    cols[i] = alloc_col(gno);
    assert( cols[i] != NULL );
    assert( cols[i]->cell_top != NULL );
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
    assert( DATA(summ)->cols[i]->summarize_word_count == 0 );
    assert( DATA(summ)->cols[i]->writebarr_word_count == 0 );
    assert( DATA(summ)->cols[i]->construction_inprogress == FALSE );
    assert( DATA(summ)->cols[i]->construction_complete == FALSE );
    assert( DATA(summ)->cols[i]->overly_popular == FALSE );

    /* We do *not* construct cells at this point; we do that on demand, 
     * as additions to the matrix are needed.
     */
  }
}

/* modifies: *west, *east
 * effects: 1. If row has cell w/ tgt_gno, returns cell; o/w returns
 * NULL, and, 2. *wst, *est get tgt_gno's neighbor(s) (where the 
 * sentinel is a neighbor candidate).
 */
static summ_cell_t *scan_row_for_cell( summ_row_t *row, int tgt_gno, 
                                summ_cell_t **wst, summ_cell_t **est )
{
  summ_cell_t *start = row->cell_lft; /* sentinel */
  summ_cell_t *end = row->cell_rgt;
  summ_cell_t *prev_start = NULL;

  assert( start->prev_row == end ); /* cyclic */
  assert( end->next_row == start );
  assert( tgt_gno > 0 );

  assert( start != NULL );
  assert(   end != NULL );

  assert( start->target_gno == -1 );

  /* Four disjoint cases: 
   * 1. row is completely empty
   * 2. cell with tgt_gno occurs in row
   * 3. all elements in row are before tgt_gno
   * 4. tgt_gno not in row, and some gno > tgt_gno in row.
   */

  if (start == end) {                     /* Case 1: sentinels */
    assert( end->target_gno == -1 );
    *wst = start;
    *est = end;
    return NULL;
  }

  end = start; /* real termination condition is when we've cycled back. */

  do {
    if (0) {
      printf("  scan_row( row:%d, tgt_gno:%d, &w, &e ): loop\n", 
             row->source_gno, tgt_gno );
      print_cell( start );
    }
    assert( start->target_gno <= tgt_gno );

    if ( start->target_gno == tgt_gno ) { /* Case 2: found it */
      *wst = prev_start;
      *est = start->next_row;
      return start;
    }

    /* continue search */
    prev_start = start;
    start = start->next_row;

    assert( prev_start->target_gno < tgt_gno );
  } while ( start != end && start->target_gno <= tgt_gno );

  /* left of || is case 3; right of || is case 4. */
  assert( start->target_gno == -1 || start->target_gno > tgt_gno );
  assert( prev_start->target_gno < tgt_gno );

  /* Both cases 3 and 4 can be handled in the same manner. */
  *wst = prev_start;
  *est = start;
  return NULL;
}


/* modifies: *nth, *sth
 * effects: 1. If col has cell w/ tgt_gno, returns cell; o/w returns
 * NULL, and, 2. *nth, *sth get tgt_gno's neighbor(s) (where the 
 * sentinel is a neighbor candidate).
 */
static summ_cell_t *scan_col_for_cell( summ_col_t *col, int src_gno,
                                summ_cell_t **nth, summ_cell_t **sth )
{
  summ_cell_t *start = col->cell_top; /* sentinel */
  summ_cell_t *end = col->cell_bot;
  summ_cell_t *prev_start = NULL;

  assert( end->next_col == start ); /* cyclic */
  assert( start->prev_col == end );
  assert( src_gno > 0 );

  assert( start != NULL );
  assert(   end != NULL );

  assert( start->source_gno == -1 );

  /* Four disjoint cases: 
   * 1. col is completely empty
   * 2. cell with src_gno occurs in col.
   * 3. all elements in col are before src_gno
   * 4. src_gno not in col, and some gno > src_gno in col.
   */

  if (start == end) {                     /* Case 1: sentinels */
    assert( end->source_gno == -1 );
    *nth = start;
    *sth = end;
    return NULL;
  }

  end = start; /* real termination condition is when we've cycled back. */

  do {
    assert( start->source_gno <= src_gno );

    if ( start->source_gno == src_gno ) { /* Case 2: found it */
      *nth = prev_start;
      *sth = start->next_col;
      return start;
    }

    /* continue search */
    prev_start = start;
    start = start->next_col;

    assert( prev_start->source_gno < src_gno );
  } while ( start != end && start->source_gno <= src_gno );

  /* left of || is case 3; right of || is case 4. */
  assert( start->source_gno == -1 || start->source_gno > src_gno );
  assert( prev_start->source_gno < src_gno );

  /* Both cases 3 and 4 can be handled in the same manner. */
  *nth = prev_start;
  *sth = start;
  return NULL;
}

#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP 
static remset_as_summary_t* allocate_remset_as_summary(int gen, int poplimit);
#endif

static void  create_refactored_from_memmgr( summ_matrix_t *sm,
                                            int popularity_limit )
{
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
  DATA(sm)->remset_summaries = 0;
#endif
  DATA(sm)->remset_summaries_count = 0;
  DATA(sm)->summarized_genset_valid = FALSE;

  /* data->summaries->region_count = data->region_count; */
  DATA(sm)->popularity_limit = popularity_limit;

  int len = sm->collector->remset_count+1;
  int i;
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
  DATA(sm)->remset_summaries = 
    (remset_as_summary_t**)must_malloc(len*sizeof(remset_as_summary_t*));
  DATA(sm)->remset_summaries[0] = NULL;
  for( i = 1; i < len; i++ ) {
    DATA(sm)->remset_summaries[i] = 
      allocate_remset_as_summary( i, popularity_limit );
  }
#endif
  DATA(sm)->remset_summaries_count = len;
  DATA(sm)->nursery_remset = create_remset( 0, 0 );
}

static void check_rep( summ_matrix_t *summ );
static void check_rep_1( summ_matrix_t *summ ) {
#if (CHECK_REP_DEGREE >= 1)
  check_rep( summ );
#endif
}
static void check_rep_2( summ_matrix_t *summ ) {
#if (CHECK_REP_DEGREE >= 2)
  check_rep( summ );
#endif
}
static void check_rep_3( summ_matrix_t *summ ) {
#if (CHECK_REP_DEGREE >= 3)
  check_rep( summ );
#endif
}


static void delayed_check_rep( summ_matrix_t *summ )
{
  static int call_count = 0;
  static const int start_on = 500000000;
  if (call_count < start_on) {
    call_count++;
  } else {
    check_rep( summ );
  }
}

EXPORT summ_matrix_t *
create_summ_matrix( gc_t *gc, int first_gno, int initial_num_rgns, 
                    double c, double p, int popularity_limit )
{
  summ_matrix_t *sm; 
  summ_matrix_data_t *data;
  /* int num_cols = first_gno + initial_num_rgns; */
  int num_cols = gc->remset_count;
  int num_under_construction = (int)(initial_num_rgns * c);
  int num_rows = gc->remset_count;

  dbmsg("  create_summ_matrix( gc, %d, initial_num_rgns: %d,"
        " %f, %f, pop_limit: %d, num_cols: %d )",
        first_gno, initial_num_rgns, c, p, popularity_limit, num_cols );

  assert( 0.0 < c && c <= 1.0 );
  assert( p >= 2.0 );

  sm = (summ_matrix_t*)must_malloc( sizeof( summ_matrix_t ));
  data = (summ_matrix_data_t*)must_malloc( sizeof( summ_matrix_data_t ));

  data->coverage = c;
  data->p = p;
  data->entries_per_objs_pool_segment = DEFAULT_OBJS_POOL_SIZE;
  data->num_cols = num_cols;
  data->num_rows = num_rows;

  data->rows = alloc_rows( first_gno + num_rows );
  data->cols = alloc_cols( first_gno + num_cols );

  data->col_complete_first = -1;
  data->col_complete_lim = -1;

  data->col_inprogress_first = -1;
  data->col_inprogress_lim = -1;

  data->row_cache.last_cell_valid = FALSE;

  sm->collector = gc;
  sm->data = data;

  my_prepare_cols( sm, first_gno, first_gno + num_under_construction );

  create_refactored_from_memmgr( sm, popularity_limit );

  check_rep_1(sm);

  return sm;
}

static void sm_expand_summary_gnos( summ_matrix_t *summ, int fresh_gno );
EXPORT void sm_expand_gnos( summ_matrix_t *summ, int fresh_gno )
{
  summ_row_t *fresh_row;
  summ_col_t *fresh_col;
  int new_num_rows, new_num_cols;
  summ_row_t** new_rows;
  summ_col_t** new_cols;
  int i;
  summ_cell_t *cell, *end;

  check_rep_3( summ );

  fresh_row = alloc_row( fresh_gno );
  fresh_col = alloc_col( fresh_gno );

  new_num_rows = DATA(summ)->num_rows + 1;

  assert( fresh_gno <= DATA(summ)->num_rows );

  {
    new_rows = (summ_row_t**)must_malloc( new_num_rows*sizeof( summ_row_t* ));

    for ( i=0; i<fresh_gno; i++ ) {
      new_rows[ i ] = DATA(summ)->rows[ i ];
    }
    new_rows[ fresh_gno ] = fresh_row;
    for ( i=fresh_gno+1; i<new_num_rows; i++ ) {
      DATA(summ)->rows[ i-1 ]->source_gno = i;
      cell = DATA(summ)->rows[ i-1 ]->cell_lft;
      end = cell;
      do {
        cell->source_gno = i;
        cell = cell->next_row;
      } while (cell != end);
      new_rows[ i ] = DATA(summ)->rows[ i-1 ];
    }
  }

  {
    assert( fresh_gno > 0 );
    if (fresh_gno <= DATA(summ)->num_cols ) {
      new_num_cols = DATA(summ)->num_cols + 1;

      new_cols = (summ_col_t**)must_malloc( new_num_cols*sizeof( summ_col_t* ));

      for ( i=0; i<fresh_gno; i++ ) {
        new_cols[ i ] = DATA(summ)->cols[ i ];
      }

      new_cols[ fresh_gno ] = fresh_col;
      for ( i=fresh_gno+1; i<new_num_cols; i++ ) {
        DATA(summ)->cols[ i-1 ]->target_gno = i;
        cell = DATA(summ)->cols[ i-1 ]->cell_top;
        end = cell;
        do {
          cell->target_gno = i;
          cell = cell->next_col;
        } while (cell != end);
        new_cols[ i ] = DATA(summ)->cols[ i-1 ];
      }
    } else {
      /* meet the new boss, same as the old boss */
      new_num_cols = DATA(summ)->num_cols;
      new_cols = DATA(summ)->cols;
    }

    for( i=0; i<new_num_cols; i++ ) {
      assert( new_cols[i] != NULL );
      assert( new_cols[i]->cell_top != NULL );
      assert( new_cols[i]->cell_bot->next_col == new_cols[i]->cell_top );
    }
  }

  sm_expand_summary_gnos( summ, fresh_gno );

  DATA(summ)->num_rows = new_num_rows;
  DATA(summ)->num_cols = new_num_cols;
  DATA(summ)->rows = new_rows;
  DATA(summ)->cols = new_cols;

  check_rep_3( summ );
}

EXPORT void sm_prepare_cols( summ_matrix_t *summ, int col_gno, int col_gno_lim )
{
  check_rep_1( summ );

  my_prepare_cols( summ, col_gno, col_gno_lim );

  check_rep_1( summ );
}

EXPORT void sm_dispose_cols( summ_matrix_t *summ, int col_gno, int col_gno_lim )
{
  check_rep_1( summ );
  assert(FALSE);
  check_rep_1( summ );
}

EXPORT void sm_construction_progress( summ_matrix_t *summ, 
                               int* word_countdown,
                               int* object_countdown )
{
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

  check_rep_2( summ );

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

  check_rep_2( summ );
}

EXPORT void sm_enumerate_row( summ_matrix_t *summ,
                       int row_gno, 
                       bool (*scanner)(word loc, void *data),
                       void *data )
{
  check_rep_1( summ );
  assert(FALSE);
  check_rep_1( summ );
}

EXPORT void sm_enumerate_col( summ_matrix_t *summ, 
                       int col_gno, 
                       bool (*scanner)(word loc, void *data),
                       void *data )
{
  check_rep_1( summ );
  assert(FALSE);
  check_rep_1( summ );
}


EXPORT void sm_add_entry( summ_matrix_t *summ, word source_obj, int target_gno )
{
  check_rep_3( summ );
  assert(FALSE);
  check_rep_3( summ );
}

EXPORT void sm_next_summary( summ_matrix_t *summ, summary_t *column ) 
{
  check_rep_1( summ );
  assert(FALSE);
  check_rep_1( summ );
}

/* Functions below are for use when this structure is being used in a
 * concurrent (ie multi-threaded) regional collector. */

EXPORT void sm_add_entry_concurrent( summ_matrix_t *summ, 
                              word source_obj, 
                              int target_gno )
{
  check_rep_3( summ );
  assert(FALSE);
  check_rep_3( summ );
}


EXPORT void sm_construction_concurrent( summ_matrix_t *summ,
                                 int grain_scan_words,
                                 int grain_scan_objects )
{
  check_rep_3( summ );
  assert(FALSE);
  check_rep_3( summ );
}


EXPORT void sm_interrupt_construction( summ_matrix_t *summ )
{
  check_rep_3( summ );
  assert(FALSE);
  check_rep_3( summ );
}

EXPORT bool sm_is_rgn_summarized( summ_matrix_t *summ, int gno ) 
{
  check_rep_1( summ );
  return DATA(summ)->summarized_genset_valid &&
    gset_memberp( gno, DATA(summ)->summarized_genset );
}
EXPORT bool sm_has_valid_summaries( summ_matrix_t *summ )
{
  check_rep_1( summ );
  return DATA(summ)->summarized_genset_valid;
}
EXPORT void sm_push_nursery_summary( summ_matrix_t *summ, smircy_context_t *smircy )
{
  check_rep_1( summ );
  smircy_push_remset( smircy, DATA(summ)->nursery_remset );
  check_rep_1( summ );
}
EXPORT void sm_clear_nursery_summary( summ_matrix_t *summ )
{
  check_rep_1( summ );
  rs_clear( DATA(summ)->nursery_remset );
  check_rep_1( summ );
}

/* below refactored from memmgr.c */

typedef struct remset_summary_data remset_summary_data_t;
struct remset_summary_data {
  /* _current_ representation: summaries[g] is non-null ==> (g in genset && summaries[g] non-empty). */
  gset_t genset;
  summ_matrix_t *summ;
  remset_t *skip_these; /* if non-null, don't traverse words in this remset */
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

#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
static remset_as_summary_t* allocate_remset_as_summary(int gen, int poplimit) 
{
  remset_as_summary_t *rast;
  rast = (remset_as_summary_t*)must_malloc(sizeof(remset_as_summary_t));
  rast->sum_remset = NULL;
  rast->gen        = gen;
  rast->valid      = TRUE;
  rast->summarize_words = 0;
  rast->writebarr_words = 0;
  rast->collector_words = 0;
  rast->max_words  = poplimit;
  return rast;
}

static int ras_words( remset_as_summary_t *r ) 
{
  return r->summarize_words + r->writebarr_words 
    + r->collector_words;
}
static void ras_reset_words( remset_as_summary_t *r ) 
{
  r->summarize_words = 0; 
  r->writebarr_words = 0;
  r->collector_words = 0;
}
static void ras_incr_words_sm( remset_as_summary_t *r, int dwords ) 
{
  r->summarize_words += dwords;
}
static void ras_incr_words_wb( remset_as_summary_t *r, int dwords )
{
  r->writebarr_words += dwords;
}
static void ras_incr_words_gc( remset_as_summary_t *r, int dwords )
{
  r->collector_words += dwords;
}
#endif

static int col_words( summ_col_t *c )
{
  return c->summarize_word_count + c->writebarr_word_count 
    + c->collector_word_count;
}
static void col_reset_words( summ_col_t *c ) 
{
  c->summarize_word_count = 0;
  c->writebarr_word_count = 0;
  c->collector_word_count = 0;
}
static void col_incr_words_sm( summ_col_t *c, int dwords ) 
{
  c->summarize_word_count += dwords;
}
static void col_incr_words_wb( summ_col_t *c, int dwords ) 
{
  c->writebarr_word_count += dwords;
}
static void col_incr_words_gc( summ_col_t *c, int dwords ) 
{
  c->collector_word_count += dwords;
}

#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
static void add_object_to_sum_rs( summ_matrix_t *summ, 
                                  int gen, 
                                  word ptr,
                                  void (*incr_words)( remset_as_summary_t *r,
                                                      int dw ))
{
  remset_as_summary_t *rs_sum = DATA(summ)->remset_summaries[ gen ];

  if (ras_words(rs_sum) <= rs_sum->max_words) {
    if (rs_sum->sum_remset == NULL) {
      rs_sum->sum_remset = grab_from_remset_pool();
    }
    if (!rs_isremembered( rs_sum->sum_remset, ptr )) {
      if (tagof(ptr) == PAIR_TAG) {
        incr_words( rs_sum, 2);
      } else {
        incr_words( rs_sum, sizefield( *ptrof(ptr) ) / 4);
      }

      if (ras_words(rs_sum) > rs_sum->max_words) {
        dbmsg( "remssumm for rgn %d overflowed on 0x%08x (%d): %d max %d",
               gen, ptr, gen_of(ptr), ras_words(rs_sum), rs_sum->max_words);
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
#endif

/* Returns cell of summ[tgt_gno] that holds objects from src_gno rgn,
 * or NULL if tgt_gno is not being summarized right now.
 * 
 * NOTE: each invocation of this fcn with a *different* src_gno from the
 * preceding invocation is expensive.  The common case is intended to be 
 * a series of calls with the same src_gno; switches to other 
 * src_gno's occur but should be rare events.
 */
static summ_cell_t* summ_cell( summ_matrix_t *summ, int src_gno, int tgt_gno )
{
  /* FIXME: simple unscalable implementation(s) first */
  /* [ going to have to define the allocation protocol much more 
   *   carefully to scale all this up.  :( ] */
  summ_cell_t *n, *s, *e, *w;
  summ_row_t *row;
  summ_col_t *col;
  summ_cell_t *row_cell, *col_cell;

  dbmsg( "  %s( summ, src: %d, tgt: %d ) num_rows: %d num_cols: %d", 
         "summ_cell", src_gno, tgt_gno, 
         DATA(summ)->num_rows, DATA(summ)->num_cols );

  assert( src_gno < DATA(summ)->num_rows );
  assert( tgt_gno < DATA(summ)->num_cols );

  if ( DATA(summ)->row_cache.last_src_gno == src_gno &&
       DATA(summ)->row_cache.last_tgt_gno == tgt_gno &&
       DATA(summ)->row_cache.last_cell_valid ) {
    return DATA(summ)->row_cache.last_cell;
  }

  row = DATA(summ)->rows[src_gno];
  col = DATA(summ)->cols[tgt_gno];
  assert( row != NULL );
  assert( col != NULL );
  row_cell = scan_row_for_cell( row, tgt_gno, &w, &e );
  col_cell = scan_col_for_cell( col, src_gno, &n, &s );

  if (row_cell != col_cell ) {
    consolemsg("  %s( summ, src: %d, tgt: %d ) num_rows: %d num_cols: %d"
               " => via row: %d, via col: %d", 
               "summ_cell", src_gno, tgt_gno, 
               DATA(summ)->num_rows, DATA(summ)->num_cols,
               (row_cell==NULL)?0:row_cell->dbg_id,
               (col_cell==NULL)?0:col_cell->dbg_id );
  }

  assert( row_cell == col_cell );

  if (row_cell == NULL) {
    dbmsg("row_cell null; w:%d e:%d n:%d s:%d", 
          w->dbg_id, e->dbg_id, n->dbg_id, s->dbg_id );
    assert( e->prev_row == w );
    assert( w->next_row == e );
    assert( s->prev_col == n );
    assert( n->next_col == s );
    assert( s != NULL );
    row_cell = make_cell( src_gno, 
                          tgt_gno, 
                          DATA(summ)->entries_per_objs_pool_segment );
    e->prev_row = row_cell;
    row_cell->next_row = e;
    w->next_row = row_cell;
    row_cell->prev_row = w;
    if ( row->cell_rgt == w ) {
      row->cell_rgt = row_cell;
    }
    s->prev_col = row_cell;
    row_cell->next_col = s;
    n->next_col = row_cell;
    row_cell->prev_col = n;
    if ( col->cell_bot == n ) {
      col->cell_bot = row_cell;
    }
    assert( col->cell_bot->next_col == col->cell_top );
  }

  DATA(summ)->row_cache.last_src_gno = src_gno;
  DATA(summ)->row_cache.last_tgt_gno = tgt_gno;
  DATA(summ)->row_cache.last_cell = row_cell;
  DATA(summ)->row_cache.last_cell_valid = TRUE;
  return row_cell;
}

static word pool_last_entry( objs_pool_t *objects ) 
{
  while (objects != NULL) {
    if ( objects->top > objects->bot ) {
      return objects->top[-1];
    }
    objects = objects->next;
  }
  return 0x0;
}

/* requires: ptr not in objects
 * modifies: objects
 * effects: returns a pool p = objects u { ptr }
 */
static objs_pool_t *pool_enqueue( summ_matrix_t *summ, objs_pool_t *objects, word ptr )
{
  int entries = DATA(summ)->entries_per_objs_pool_segment;
  if ( objects->top == objects->lim ) {
    objs_pool_t *p = allocate_pool_segment( entries );
    p->next = objects;
    objects = p;
  }
  *objects->top = ptr;
  objects->top += 1;
  return objects;
}

static word cell_last_entry( summ_matrix_t *summ, summ_cell_t *cell )
{
  return pool_last_entry( cell->objects );
}

static void cell_enqueue( summ_matrix_t *summ, summ_cell_t *cell, word ptr )
{
  /* assert2( ptr occurs nowhere in summ[cell->tgt].row[cell->src] */
  cell->objects = pool_enqueue( summ, cell->objects, ptr );
}

static void clear_col_cells( summ_matrix_t *summ, int col_idx );
static void clear_col_mutator_rs( summ_matrix_t *summ, int col_idx );

static bool col_contains_ptr( summ_matrix_t *summ, summ_col_t *col, word ptr )
{
  summ_cell_t *cell, *sent;
  cell = col->cell_top;
  sent = cell;
  do {
    objs_pool_t *objs;
    for( objs = cell->objects; objs != NULL; objs = objs->next ) {
      word *wptr;
      for( wptr = objs->bot; wptr < objs->top; wptr++ ) {
        if (*wptr == ptr)
          return TRUE;
      }
    }
    cell = cell->next_col;
  } while (cell != sent);

  return FALSE;
}

static void col_print_ptr( summ_matrix_t *summ, summ_col_t *col )
{
  summ_cell_t *cell, *sent;
  cell = col->cell_top;
  sent = cell;
  do {
    objs_pool_t *objs;
    for( objs = cell->objects; objs != NULL; objs = objs->next ) {
      word *wptr;
      for( wptr = objs->bot; wptr < objs->top; wptr++ ) {
        printf("0x%08x (%d) ", *wptr, gen_of(*wptr));
      }
    }
    cell = cell->next_col;
  } while (cell != sent);

  printf("\n");
  fflush(0);
}

static void incr_size_and_oflo_check( summ_matrix_t *summ, int tgno, word w,
                                      void (*incr_words)( summ_col_t *c, int dw ))
{
  int word_count;
  summ_col_t *col = DATA(summ)->cols[tgno];
  int pop_limit = DATA(summ)->popularity_limit;
  if (tagof(w) == PAIR_TAG) {
    word_count = 2;
  } else {
    word_count = sizefield( *ptrof(w) ) / sizeof(word);
  }

  incr_words( col, word_count );
  if (col_words(col) > pop_limit) {
    dbmsg( "cellsumm for rgn %d overflowed on 0x%08x (%d): %d max %d",
           tgno, w, gen_of(w), col_words(col), pop_limit );
    col->overly_popular = TRUE;
    clear_col_cells( summ, tgno );
    clear_col_mutator_rs( summ, tgno );
  }
}

#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
static bool word_counts_match( summ_col_t *col, remset_as_summary_t *ras )
{
  /* return (col_words(col) == ras_words(ras)); */
  return (col->summarize_word_count == ras->summarize_words)
    && (col->collector_word_count == ras->collector_words);
}
#endif

/* Let A be summ[tgt_gen].cell[src_gen].objects.
 * requires: (ptr in A) implies (ptr is A's latest entry)
 * modifies: A
 * effects: if ptr in A, do nothing.  Else enqueue ptr in A.
 */
static void add_object_to_sum_array( summ_matrix_t *summ, 
                                     int tgt_gen,
                                     word ptr, 
                                     int src_gen,
                                     void (*col_incr_w)(summ_col_t *c, 
                                                        int dw ) 
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
                                     , 
                                     void (*ras_incr_w)(remset_as_summary_t *r,
                                                        int dw )
#endif
)
{
  int pop_limit = DATA(summ)->popularity_limit;
  summ_col_t *col = DATA(summ)->cols[tgt_gen];

#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
  assert2( word_counts_match(col, DATA(summ)->remset_summaries[tgt_gen]) );
#endif

  if (col_words(col) <= pop_limit) {
    summ_cell_t *cell = summ_cell( summ, src_gen, tgt_gen );
    if (cell == NULL) {
      /* do nothing */
    } else if (cell_last_entry(summ, cell) == ptr) {
      /* do nothing */
    } else {
#if ADD_TO_SUMAR_ASSERTS_UNIQ_ENQ 
        assert( ! col_contains_ptr(summ, col, ptr) );
#endif

      cell_enqueue( summ, cell, ptr );
      incr_size_and_oflo_check( summ, tgt_gen, ptr, col_incr_w );
    }
  } else {
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
    assert( col->overly_popular 
            && ! DATA(summ)->remset_summaries[tgt_gen]->valid );
#endif
  }

  /* XXX */
  /* The interface for this function might not make sense, because it
   * is too expensive (ie >> constant time) to lookup the Cell into
   * which we are inserting ptr.
   *
   * Idea: Maintain the illusion of this interface by keeping a
   * transient table in the summ that maps SZING -> [Maybe Cell].
   * That would require that all invocations on gen_of(ptr) are
   * adjacent, but I actually think that holds for any particular
   * traversal.  (Note that currently there are two traversals that
   * use this callback; the per-remset scan in
   * sm_build_remset_summaries, and the per-tospace scan in
   * sm_points_across_callback.)
   *
   * Secondary Idea: why not make the illusion concrete by adding the
   * table as a parameter to this function?  (Doing so may require
   * generalizing some of the void* data arguments to callbacks to
   * types beyond summ_matrix_t*, but that's okay.)
   */

  /* Adding ptr to the Col's hashset *always* be a sound addition,
   * since there can be overlap between a Col's cells and its hashset.
   */
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
  add_object_to_sum_rs( summ, tgt_gen, ptr, ras_incr_w ); /* XXX remove when above "works" */

  assert2( word_counts_match(col, DATA(summ)->remset_summaries[tgt_gen]) );
#endif
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

static void add_object_to_mut_rs( summ_matrix_t *summ, int g_rhs, word w ) 
{
  remset_t *rs; 
  /* XXX TODO: filter addition to count words and wave-off when
   * summary has become over-populated. */ 
  rs = DATA(summ)->cols[ g_rhs ]->sum_mutator;
  if (rs == NULL) {
    rs = grab_from_remset_pool();
    DATA(summ)->cols[ g_rhs ]->sum_mutator = rs;
  }
  rs_add_elem( rs, w );
}

/* These come from the mutator, so there may be duplicates.
 * XXX
 * Therefore they go in columns' hashsets (represented by remset_t).
 */
EXPORT void sm_add_ssb_elems_to_summary( summ_matrix_t *summ, word *bot, word *top, int g_rhs )
{
  word *p, *q, w;
  int pop_limit;
  summ_col_t *col;

  check_rep_3( summ );

  pop_limit = DATA(summ)->popularity_limit;
  col = DATA(summ)->cols[g_rhs];

  rs_add_elems_funnel( DATA(summ)->nursery_remset, bot, top );

  if ( DATA(summ)->summarized_genset_valid
       && gset_memberp( g_rhs, DATA(summ)->summarized_genset )) {
    p = bot; 
    q = top; 
    while (q > p) {
      q--;
      w = *q;
      w = retagptr(w);
      if (!w) 
        continue; /* XXX put above retagptr invoc above? */

      if (col_words(col) <= pop_limit) {
        add_object_to_mut_rs( summ, g_rhs, w );
        incr_size_and_oflo_check( summ, g_rhs, w, col_incr_words_wb );
      } else {
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
        assert( col->overly_popular 
                && ! DATA(summ)->remset_summaries[g_rhs]->valid );
#endif
      }

#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
      add_object_to_sum_rs( summ, g_rhs, w, ras_incr_words_wb ); /* XXX remove when above "works" */
#endif
    }
  }

  check_rep_3( summ );
}

static bool scan_object_for_remset_summary( word ptr, void *data, unsigned *count )
{
  word *loc = ptrof(ptr);
  word scanned = 0;
  bool do_enqueue = FALSE;
  remset_summary_data_t *remsum = (remset_summary_data_t*)data;
  gset_t genset = remsum->genset;
  int mygen = gen_of(ptr); 
#if SUMMARIZE_KILLS_RS_ENTRIES
  bool keep_in_remembered_set = FALSE;
#endif
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

  if (remsum->skip_these != NULL 
      && rs_isremembered( remsum->skip_these, ptr )) {
#if SUMMARIZE_KILLS_RS_ENTRIES
    /* its in the other set, so we can remove it from this one. */
    keep_in_remembered_set = FALSE;
#endif
    goto end;
  }

  if (tagof( ptr ) == PAIR_TAG) {
    /* handle car */
    if (isptr(*loc)) {
      int gen = gen_of(*loc);
      if (instrumented) 
        annoyingmsg("scan_object_for_remset_summary "
                    "pair car: 0x%08d (%d)", *loc, gen);
      if (mygen != gen) {
#if SUMMARIZE_KILLS_RS_ENTRIES
        if (! gc_is_nonmoving( remsum->summ->collector, gen )) {
          keep_in_remembered_set = TRUE;
        }
#endif
        if (gset_memberp(gen,genset)) {
          do_enqueue = TRUE;
          add_object_to_sum_array( remsum->summ, gen, ptr, mygen,
                                   col_incr_words_sm
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
                                   , ras_incr_words_sm 
#endif
                                   );
        }
      }
    }
    ++loc;
    /* handle cdr */
    if (isptr(*loc)) {
      int gen = gen_of(*loc);
      if (instrumented) 
        annoyingmsg("scan_object_for_remset_summary "
                    "pair cdr: 0x%08d (%d)", *loc, gen);
      if (mygen != gen) {
#if SUMMARIZE_KILLS_RS_ENTRIES
        if (! gc_is_nonmoving( remsum->summ->collector, gen )) {
          keep_in_remembered_set = TRUE;
        }
#endif
        if (gset_memberp(gen,genset)) {
          do_enqueue = TRUE;
          add_object_to_sum_array( remsum->summ, gen, ptr, mygen,
                                   col_incr_words_sm
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
                                   , ras_incr_words_sm 
#endif
                                   );
        }
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
        if (mygen != gen) {
#if SUMMARIZE_KILLS_RS_ENTRIES
          if (! gc_is_nonmoving( remsum->summ->collector, gen )) {
            keep_in_remembered_set = TRUE;
          }
#endif
          if (gset_memberp(gen,genset)) {
            do_enqueue = TRUE;
            add_object_to_sum_array( remsum->summ, gen, ptr, mygen,
                                     col_incr_words_sm
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
                                     , ras_incr_words_sm 
#endif
                                     );
          }
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

 end:
#if SUMMARIZE_KILLS_RS_ENTRIES
  return keep_in_remembered_set;
#else
  return TRUE; /* don't remove entries from the remembered set we are summarizing! */  
#endif
}

static bool rsenum_assert_not_memberp( word loc, void *data, unsigned *stats )
{
  remset_t *other = (remset_t*) data;
  assert( ! rs_isremembered(other, loc) );
  return TRUE; /* don't remove the object! */
}
static void assert_disjoint( remset_t *ra, remset_t *rb ) {
  rs_enumerate( ra, rsenum_assert_not_memberp, rb );
  rs_enumerate( rb, rsenum_assert_not_memberp, ra );
}

/* These come from the collector's remsets; therefore no duplication.
 * XXX
 * Build a row at a time ("pROWducer").
 * Put entries into the columns' sequential arrays.
 *
 * FIXME: currently monolithic, iterating over all rows in a single
 * invocation.  This must be fixed; the pROWduction loop must be 
 * broken down so that it yields control in between rows, at least.
 */
EXPORT void sm_build_remset_summaries( summ_matrix_t *summ, gset_t genset )
{
  remset_summary_data_t remsum;
  int i;
  int remset_count = summ->collector->remset_count;
  int summ_len = gset_max_elem(genset); /* (some entries can be null) */

  check_rep_1( summ );

  /* XXX potentially assert summ->summarized_genset is nullset */

  remsum.genset = genset;
  remsum.summ = summ;
  remsum.skip_these = NULL;
  remsum.objects_visited = 0;
  remsum.objects_added = 0;
  remsum.words_added = 0;

  /* Optimistically assume that summarization will succeed for all
   * elems of genset; if one of them overflows, it will be
   * responsibility of scan_object_for_remset_summary to set valid
   * field to FALSE.
   */
  for( i=0 ; i < DATA(summ)->remset_summaries_count; i++ ) {
    if (gset_memberp( i, genset )) {
      DATA(summ)->cols[i]->overly_popular = FALSE;
      col_reset_words( DATA(summ)->cols[i] );

      /* XXX kill below after shifting to cells rep */
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
      DATA(summ)->remset_summaries[i]->valid = TRUE;
      ras_reset_words( DATA(summ)->remset_summaries[i] );
      /* Construction assumes that summaries start off empty. */
      assert2( DATA(remsum.summ)->remset_summaries[ i ]->sum_remset == NULL ||
               DATA(remsum.summ)->remset_summaries[ i ]->sum_remset->live == 0);
#endif
    }
  }

  for(i = 1; i < remset_count; i++) {
    /* enumerating all *rows*; thus this is the pROWduction loop. */

    /* TODO: use rs_enumerate_partial here? XXX */
    dbmsg("enum remsets of %d, live minor: %d major: %d", i, 
          summ->collector->remset[ i ]->live, 
          summ->collector->major_remset[ i ]->live );
    remsum.skip_these = NULL;
    rs_enumerate( summ->collector->remset[ i ], 
		  scan_object_for_remset_summary,
		  (void*) &remsum );
    /* don't allow duplicate scans */
    remsum.skip_these = summ->collector->remset[ i ];
    rs_enumerate( summ->collector->major_remset[ i ], 
		  scan_object_for_remset_summary,
		  (void*) &remsum );
  }
  if (genset.tag == gs_singleton) {
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
    remset_t *rs = DATA(remsum.summ)->remset_summaries[genset.g1]->sum_remset;
#endif
    assert( genset.g1 < DATA(summ)->remset_summaries_count );
  } else if (genset.tag == gs_range ) {
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
    remset_t *rs1, *rs2;
    assert( genset.g2 <= DATA(summ)->remset_summaries_count );
    rs1 = DATA(remsum.summ)->remset_summaries[genset.g1]->sum_remset;
    rs2 = DATA(remsum.summ)->remset_summaries[genset.g2-1]->sum_remset;
#endif
  } else { assert(0); }

  { /* XXX review me XXX */
    DATA(summ)->summarized_genset = genset;
    DATA(summ)->summarized_genset_valid = TRUE;
  }

  check_rep_1( summ );
}

static void* verify_summaries_msgc_fcn( word obj, word src, void *data )
{
  summ_matrix_t *summ = (summ_matrix_t*)data;
  int src_gen, tgt_gen;
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
  if (isptr(src) && isptr(obj) &&
      ((src_gen = gen_of(src)) != (tgt_gen = gen_of(obj))) &&
      ! gc_is_nonmoving( summ->collector, tgt_gen )) {
    assert( src_gen >= 0 );
    if (src_gen > 0) {
      assert( *summ->collector->ssb[src_gen]->bot == *summ->collector->ssb[src_gen]->top );
      assert( *summ->collector->ssb[tgt_gen]->bot == *summ->collector->ssb[tgt_gen]->top );
      if (DATA(summ)->summarized_genset_valid &&
          gset_memberp( tgt_gen, DATA(summ)->summarized_genset ) &&
          DATA(summ)->remset_summaries[ tgt_gen ]->valid ) {
        assert( (DATA(summ)->remset_summaries[ tgt_gen ]->sum_remset) != NULL );
        assert( rs_isremembered( DATA(summ)->remset_summaries[ tgt_gen ]->sum_remset, src ));
      }
    }
  }
#else
  consolemsg("verify_summaries_msgc_fcn req MAINTAIN_REDUNDANT_RS_AS_SM_REP");
  assert(0);
#endif
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

EXPORT void sm_verify_summaries_via_oracle( summ_matrix_t *summ )
{
  msgc_context_t *conserv_context;
  msgc_context_t *aggress_context;
  int marked, traced, words_marked;

  check_rep_1( summ );
  if (DATA(summ)->summarized_genset_valid) {
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

#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
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
	if (gset_memberp( i, DATA(summ)->summarized_genset )){
	  assert( DATA(summ)->remset_summaries[i] != NULL);
	  assert( i < DATA(summ)->remset_summaries_count );
	  /* we do not grab a remset_t if no entries are added, 
	     so this is a guard rather than an assertion. */
	  if ( DATA(summ)->remset_summaries[i]->sum_remset != NULL) {
	    data.summary_for_region = i;
	    rs_enumerate( DATA(summ)->remset_summaries[ i ]->sum_remset, 
	                  verify_summaries_remset_fcn, 
	                  &data );
	  }
	}
      }
      msgc_end( aggress_context );
    }
#else
    assert(0);
#endif
    msgc_end( conserv_context );
  }

  check_rep_1( summ );
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

/* This will need to traverse both the array and the hashset structures.
 * But it should be relatively straight-forward.
 * XXX 
 *
 * (I think the choice between row- or col- based iteration does not
 *  matter here; no matter what, we need to refine all cells and sets
 *  in the summary structure.)
 */
EXPORT void sm_refine_summaries_via_marksweep( summ_matrix_t *summ ) 
{
  smircy_context_t *context;

  check_rep_1( summ );

  context = summ->collector->smircy;

  /* XXX refining the summaries as well as the remsets based on the
     marksweep info.  This may or may not be necessary in an improved
     version of the refinement code.

     XXX its definitely going away as part of the shift from 
     remset-rep to cells-rep
  */
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
  if (DATA(summ)->summarized_genset_valid) {
    int i;
    for (i = 0; i < summ->collector->remset_count; i++) {
      if (gset_memberp( i, DATA(summ)->summarized_genset)) {
        assert( DATA(summ)->remset_summaries[i] != NULL);
        if (DATA(summ)->remset_summaries[i]->sum_remset != NULL) {
          rs_enumerate( DATA(summ)->remset_summaries[i]->sum_remset,
                        scan_refine_remset, 
                        context );
        }
      }
    }
  }
#endif

  /* refine cells-rep based on smircy state */
  {
    int i;
    for (i = 0; i < DATA(summ)->num_cols; i++) {
      summ_col_t *col = DATA(summ)->cols[i];

      summ_cell_t *sent = col->cell_top;
      summ_cell_t *cell = sent->next_col;
      while ( cell != sent ) {
        objs_pool_t *objects;
        for( objects = cell->objects; objects != NULL; objects = objects->next ) {
          word *wptr;
          for ( wptr = objects->bot; wptr < objects->top; wptr++ ) {
            if (*wptr == 0x0) {
              /* entry already clear */
            } else if (! smircy_object_marked_p( context, *wptr )) {
              *wptr = 0x0; /* clear entry */
            }
          }
        }
        cell = cell->next_col;
      }

      if (col->sum_mutator != NULL) {
        rs_enumerate( col->sum_mutator, scan_refine_remset, context );
      }
    }
  }

  check_rep_1( summ );
}

/* XXX
 * I will need to either traverse the array and remsets for this
 * (potentially with a cache), or maintain the info as summarization
 * and mutation progress.*/
EXPORT int sm_summarized_live( summ_matrix_t *summ, int rgn ) 
{
  bool rgn_summarized;
  int rgn_summarized_live;

  check_rep_3( summ );

  rgn_summarized = 
    DATA(summ)->summarized_genset_valid && 
    gset_memberp( rgn, DATA(summ)->summarized_genset );
#if USE_REDUNDANT_RS_AS_SM_REP
  if (rgn_summarized) {
    if (DATA(summ)->remset_summaries[ rgn ]->sum_remset == NULL ) {
      rgn_summarized_live = 0;
    } else {
      rgn_summarized_live = 
        ras_words( DATA(summ)->remset_summaries[ rgn ] );
    }
  } else {
    rgn_summarized_live = -ras_words(DATA(summ)->remset_summaries[ rgn ]) - 1;
  }
#else
  if (rgn_summarized) { /* XXX */
    rgn_summarized_live = col_words( DATA(summ)->cols[rgn] );
  } else {
    rgn_summarized_live = -col_words( DATA(summ)->cols[rgn] ) - 1;
  }
#endif

  check_rep_3( summ );
  return rgn_summarized_live;
}

/* XXX 
 * should be straight-forward to implement.
 * (Although maybe not; this method may be contrary to the overall 
 *  design, because the goal is to use one set of summaries while
 *  constructing the next set, so you never should invalidate
 *  /all/ of the summaries at once...) 
 * 
 * This function is only invoked by collect_rgnl_clear_summary; it
 * seems like it was a general "reset" operation, and so it will
 * probably go out the window when I shift to a two-stage pipelined
 * algorithm. */
EXPORT void sm_invalidate_summaries( summ_matrix_t *summ ) 
{
  int i;

  check_rep_1( summ );

#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
  for (i=1; i<DATA(summ)->remset_summaries_count; i++) {
    assert2( DATA(summ)->remset_summaries[i]->sum_remset == NULL );
  }
#endif
  DATA(summ)->summarized_genset_valid = FALSE;

  check_rep_1( summ );
}

static void assert_unreachable( summ_matrix_t *sm, summ_cell_t *ucell )
{
  /* Slow (traverses all of sm), so leave empty except when debugging. */
#if FREE_CELL_ASSERTS_UNREACHABLE
  int i;
  summ_row_t *row;
  summ_col_t *col;
  summ_cell_t *sent, *cell;
  for (i = 0; i < DATA(sm)->num_rows; i++) {
    row = DATA(sm)->rows[i];

    assert( row->cell_lft != ucell );
    assert( row->cell_rgt != ucell );

    sent = row->cell_lft;
    cell = sent;
    do {

      assert( cell != ucell );
      assert( cell->next_row != ucell );
      assert( cell->prev_row != ucell );
      assert( cell->next_col != ucell );
      assert( cell->prev_col != ucell );

      cell = cell->next_row;
    } while ( cell != sent );
  }

  for (i = 0; i < DATA(sm)->num_cols; i++) {
    col = DATA(sm)->cols[i];

    assert( col->cell_top != ucell );
    assert( col->cell_bot != ucell );

    sent = col->cell_top;
    cell = sent;
    do {

      assert( cell != ucell );
      assert( cell->next_row != ucell );
      assert( cell->prev_row != ucell );
      assert( cell->next_col != ucell );
      assert( cell->prev_col != ucell );

      cell = cell->next_col;
    } while ( cell != sent );
  }
#endif
}

static void clear_col_cells( summ_matrix_t *summ, int col_idx )
{
  int rgn_next = col_idx;
  summ_col_t *col = DATA(summ)->cols[ rgn_next ];

  DATA(summ)->row_cache.last_cell_valid = FALSE;

  /* clear contribution from summarization */
  {
    summ_cell_t *sent = col->cell_top;
    summ_cell_t *cell;
    summ_cell_t *next;
    cell = sent->next_col;
    sent->next_col = sent; /* (re-complete the */
    sent->prev_col = sent; /*   now trivial    */
    col->cell_bot = sent;  /*    cycle)        */
    while (cell != sent) {
      next = cell->next_col;
      cell->prev_row->next_row = cell->next_row;
      cell->next_row->prev_row = cell->prev_row;
      cell->prev_col->next_col = cell->next_col;
      cell->next_col->prev_col = cell->prev_col;
      if (DATA(summ)->rows[ cell->source_gno ]->cell_rgt == cell) {
        DATA(summ)->rows[ cell->source_gno ]->cell_rgt = cell->prev_row;
      }
      assert_unreachable( summ, cell ); /* XXX expensive */
      free_cell( cell, DATA(summ)->entries_per_objs_pool_segment );
      cell = next;
    }
    assert( (sent->next_col == sent) && (sent->prev_col == sent) );
  }
}

static void clear_col_mutator_rs( summ_matrix_t *summ, int col_idx ) 
{
  int rgn_next = col_idx;
  summ_col_t *col = DATA(summ)->cols[ rgn_next ];
  remset_t *rs = col->sum_mutator;
  if (rs != NULL) {
    rs_clear( rs );
    return_to_remset_pool( rs );
    col->sum_mutator = NULL;
  }
}

/* XXX
 * Should be straight-forward. 
 * 
 * This clears a column in the matrix; just return its cells (and
 * hashset) to pool.
 */
EXPORT void sm_clear_summary( summ_matrix_t *summ, int rgn_next )
{
  check_rep_1( summ );

  /* clear the summary that guided this collection. */
  {
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
    remset_t *rs = DATA(summ)->remset_summaries[ rgn_next ]->sum_remset;
    if (rs != NULL) { 
      rs_clear( rs );
      return_to_remset_pool( rs );
      DATA(summ)->remset_summaries[ rgn_next ]->sum_remset = NULL;
    }
    ras_reset_words( DATA(summ)->remset_summaries[ rgn_next ] );
#endif

    DATA(summ)->summarized_genset = 
      gset_remove( rgn_next, DATA(summ)->summarized_genset);

    { 
      gset_t genset = DATA(summ)->summarized_genset;
      if (genset.tag == gs_singleton) {
        assert(genset.g1 <  DATA(summ)->remset_summaries_count);
      } else if (genset.tag == gs_range) {
        assert(genset.g2 <= DATA(summ)->remset_summaries_count);
      } else { 
        assert(0); 
      }
    }
  }

  /* XXX above is cruft adapted from prior refactoring. */

  {
    /* clear contribution from summarization */
    clear_col_cells( summ, rgn_next );
    /* clear contribution from mutator */
    clear_col_mutator_rs( summ, rgn_next );

    col_reset_words( DATA(summ)->cols[rgn_next] );
  }

  check_rep_1( summ );
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
/* 
 * XXX
 * 
 * This is one where things get tricky; it needs to traverse a
 * single *row*.  (It is this computation that is motivating the
 * sparse matrix representation rather than a simple array and hashset
 * per region; see also sm_points_across_callback below.)
 */
EXPORT void sm_clear_contribution_to_summaries( summ_matrix_t *summ, int rgn_next ) 
{
  check_rep_1( summ );

#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
  /* clear contribution of rgn_next to all summaries */
  { 
    int i;
    remset_t *rs;
    struct filter_objects_from_sum_remset_data data;
    data.gc  = summ->collector;
    data.gen = rgn_next;
    for(i=0; i<DATA(summ)->remset_summaries_count; i++ ) {
      if (i == rgn_next) 
        continue; /* the entire summary for i is cleared down below */
      /* (and shouldn't summary have nothing from region_i anyway?) */
      annoyingmsg( "clear summary [%d] of entries from %d", i, rgn_next );
      if (gset_memberp( i, DATA(summ)->summarized_genset ) &&
          DATA(summ)->remset_summaries[ i ]->sum_remset != NULL) {
        rs = DATA(summ)->remset_summaries[ i ]->sum_remset;
        rs_enumerate( rs, 
                      filter_objects_from_sum_remset, 
                      &data );
      }
    }
  }
#endif

  /* XXX kill above when I switch from remset-rep to cells-rep */

  DATA(summ)->row_cache.last_cell_valid = FALSE;

  /* clear contribution of rgn_next to all columns. */
  {
    summ_row_t *row;
    summ_cell_t *sent;
    summ_cell_t *cell, *next;
    row = DATA(summ)->rows[ rgn_next ];
    sent = row->cell_lft;
    cell = sent->next_row;
    sent->next_row = sent; /* (re-complete the */
    sent->prev_row = sent; /*   now trivial    */
    row->cell_rgt = sent;  /*    cycle)        */
    while (cell != sent) {
      next = cell->next_row;
      cell->prev_col->next_col = cell->next_col;
      cell->next_col->prev_col = cell->prev_col;
      cell->prev_row->next_row = cell->next_row;
      cell->next_row->prev_row = cell->prev_row;
      if (DATA(summ)->cols[ cell->target_gno ]->cell_bot == cell) {
        DATA(summ)->cols[ cell->target_gno ]->cell_bot = cell->prev_col;
      }
      assert_unreachable( summ, cell ); /* XXX expensive */
      free_cell( cell, DATA(summ)->entries_per_objs_pool_segment );
      cell = next;
    }

    assert( (sent->next_row == sent) && (sent->prev_row == sent) );

    {
      struct filter_objects_from_sum_remset_data data;
      int i;
      data.gc  = summ->collector;
      data.gen = rgn_next;

      /* XXX see note from Tue Jan 6 16:20:31 EST 2009 
       * should traverse sum_mutator's via linked-list rather than
       * array here. 
       */
      for (i = 0; i < DATA(summ)->num_cols; i++) {
        summ_col_t *col;
        col = DATA(summ)->cols[i];
        if ( col->sum_mutator != NULL ) {
          rs_enumerate( col->sum_mutator,
                        filter_objects_from_sum_remset,
                        &data );
        }
      }
    }
  }

  check_rep_1( summ );
}

struct fold_from_nursery_data {
  int gen;
  remset_t *rs;
};
static bool rsenum_fold_from_nursery( word ptr, void *my_data, unsigned *count ) {
  struct fold_from_nursery_data *data;
  data = (struct fold_from_nursery_data*)my_data;
  if (gen_of(ptr) != data->gen) {
    rs_add_elem( data->rs, ptr );
  }

  /* maybe use FALSE, thus source rs would be cleared?  Would it matter? */
  return TRUE; 
}

static bool rsenum_filter_tgt_gen( word ptr, void *my_data, unsigned *count ) 
{
  int tgt_gen = *(int*)my_data;
  return (gen_of(ptr) != tgt_gen);
}

static bool rsenum_assert_not_forwarded( word object, void *data, unsigned *stats )
{
#define FORWARD_HDR 0xFFFFFFFE /* XXX eek!  Factor from cheney.h elsewhere! */
  if ( *ptrof(object) == FORWARD_HDR ) {
    consolemsg( " eek object 0x%08x (%d) has been forwarded but is in nurs rs.", 
                object, gen_of(object) );
  }
  assert( *ptrof(object) != FORWARD_HDR );
  return TRUE;
}

static void assert_no_forwarded_objects( remset_t *rs )
{
  rs_enumerate( rs, rsenum_assert_not_forwarded, 0x0 );
}

/* 
 * This should be fine as long as I continue using a remset_t to
 * represent the nursery summary.  (The only change I anticipate to
 * the nursery summary is perhaps choosing a different variant of
 * hashset, such as cuckoo hashing.)
 */
EXPORT void sm_init_summary_from_nursery_alone( summ_matrix_t *summ, 
                                         summary_t *summary ) 
{
  check_rep_1( summ );
  assert_no_forwarded_objects( DATA(summ)->nursery_remset );
  rs_init_summary( DATA(summ)->nursery_remset, -1, summary);
  check_rep_1( summ );
}

static bool filter_mutated_elems( summary_t *s, word w )
{
  summ_matrix_t *summ   = (summ_matrix_t*) s->cursor1;
  summ_col_t    *col    = (summ_col_t*) s->cursor2;
  remset_t      *nur_rs = DATA(summ)->nursery_remset;
  remset_t      *mut_rs = col->sum_mutator;
  return ! rs_isremembered( mut_rs, w );
}
static bool filter_gen( summary_t *s, word w )
{
  bool ret = (gen_of(w) != s->icursor2);
  if (0) consolemsg("filter_gen( s, w=0x%08x (%d) ) => %s", w, gen_of(w), ret?"T":"F");
  return ret;
}

static bool next_chunk( summary_t *s, word **start, word **lim, bool *all_unseen)
{
  summ_matrix_t *summ   = (summ_matrix_t*) s->cursor1;
  summ_col_t    *col    = (summ_col_t*)    s->cursor2;

  summ_cell_t *cell = (summ_cell_t*) s->cursor3;
  objs_pool_t *pool = (objs_pool_t*) s->cursor4;

  if (cell == col->cell_top) {
    /* XXX this *replaces* s->next_chunk/dispose/filter with the ones
     * for the mutator remset.  (Its an incredibly ugly hack that I
     * want to replace with something easier to comprehend when I have
     * time!) */
    rs_init_summary( col->sum_mutator, -1, s );
    return summary_next_chunk_with_flags( s, start, lim, all_unseen );
  } else {
    *all_unseen = FALSE;
    *start = pool->bot;
    *lim   = pool->top;
    if (pool->next != NULL) {
      s->cursor4 = pool->next;
      return TRUE;
    } else {
      s->cursor4 = cell->next_col->objects;
      s->cursor3 = cell->next_col;
      return TRUE;
    }
  }
}

/* 
 * XXX
 * 
 * This will traverse the column for next_summ_idx.
 * (The caller is our "COLsumer").
 */
EXPORT void sm_fold_in_nursery_and_init_summary( summ_matrix_t *summ, 
                                          int next_summ_idx, 
                                          summary_t *summary )
{
  check_rep_1( summ );
#if USE_REDUNDANT_RS_AS_SM_REP
  /* XXX for now, fold the nursery remset into the remset
   * we're using for this major collection.  Better long term
   * approach may be to do two separate scans rather than a
   * fold-then-scan-combined XXX */
  {
    remset_t *rs = DATA(summ)->remset_summaries[ next_summ_idx ]->sum_remset;
    if (rs != NULL) {
      struct fold_from_nursery_data data;
      data.gen = next_summ_idx;
      data.rs  = rs;
      rs_enumerate( DATA(summ)->nursery_remset, rsenum_fold_from_nursery, &data );

      /* XXX this is to maintain consistency between cells-rep and
       * remset-rep
       * Eventually all of this will initialize the summary so that it
       * traverses the column cells + sum_mutator + nursery_remset 
       */
      if (DATA(summ)->cols[ next_summ_idx ]->sum_mutator == NULL) {
        DATA(summ)->cols[ next_summ_idx ]->sum_mutator = grab_from_remset_pool();
      }
      data.rs = DATA(summ)->cols[ next_summ_idx ]->sum_mutator;
      rs_enumerate( DATA(summ)->nursery_remset, rsenum_fold_from_nursery, &data );
    } else {
      rs_enumerate( DATA(summ)->nursery_remset,
                    rsenum_filter_tgt_gen, 
                    &next_summ_idx );
      rs = DATA(summ)->nursery_remset;
    }
    annoyingmsg( "construct rs (%d) summary", rs->live);
    rs_init_summary( rs, -1, summary );
  }
#else
  {
    summ_col_t *col;
    int entries;
    col = DATA(summ)->cols[ next_summ_idx ];

    if (col->sum_mutator == NULL) {
      /* XXX consider delaying this until its actually 
       * needed within rsenum_fold_from_nursery */
      col->sum_mutator = grab_from_remset_pool();
    }
    {
      /* fold nursery into mutator rs if present */
      struct fold_from_nursery_data data;
      data.gen = next_summ_idx;
      data.rs  = col->sum_mutator;
      rs_enumerate( DATA(summ)->nursery_remset, 
                    rsenum_fold_from_nursery,
                    &data );
    }

    entries = col_words(col) + DATA(summ)->nursery_remset->live;
    if (col->cell_top->next_col != col->cell_top) {
      summary_init_dispose( summary, entries, next_chunk, 
                            NULL, filter_mutated_elems );
      /* 1: summ, 2: col, 3: cell, 4: objs_pool */
      summary->cursor1 = summ;
      summary->cursor2 = col;
      summary->cursor3 = col->cell_top->next_col;
      summary->cursor4 = col->cell_top->next_col->objects;
      assert( summary->cursor4 != NULL );
    } else {
      rs_init_summary( DATA(summ)->nursery_remset, -1, summary );
      summary->icursor2 = col->target_gno;
      summary->filter = filter_gen;
    }
  }
#endif
  check_rep_1( summ );
}

EXPORT bool sm_nursery_summary_contains( summ_matrix_t *summ, word obj ) 
{
  check_rep_3( summ );
  /* XXX the return statement below was missing for quite a while. 
   * Double-check calling-context; see if this invocation is really necessary.
   * (It probably was artificially doing the right thing; yay C...)
   */
  return rs_isremembered( DATA(summ)->nursery_remset, obj );
}

EXPORT void sm_nursery_summary_enumerate( summ_matrix_t *summ, 
                                   bool (*scanner)(word loc, void *data, unsigned *stats),
                                   void *data )
{
  check_rep_1( summ );
  rs_enumerate( DATA(summ)->nursery_remset, scanner, data );
  check_rep_1( summ );
}


EXPORT bool sm_majorgc_permitted( summ_matrix_t *summ, int rgn_next )
{
  check_rep_3( summ );
#if USE_REDUNDANT_RS_AS_SM_REP
  return (ras_words( DATA(summ)->remset_summaries[ rgn_next ])
          <= DATA(summ)->popularity_limit);
#else 
  return (col_words( DATA(summ)->cols[ rgn_next ])
          <= DATA(summ)->popularity_limit);
#endif
}

/* 
 * XXX
 * standard heap expansion operation.
 * Should just do the straight-forward ("dumb") expansion and
 * hope that's good enough.
 */
static void sm_expand_summary_gnos( summ_matrix_t *summ, int fresh_gno ) 
{
  /* check that inserting fresh_gno does not upset
     the existing summarization data (that is, that there are *no*
     summaries pointing into regions >= fresh_gno
  */
  if (DATA(summ)->summarized_genset_valid) {
    assert( ! gset_min_elem_greater_than( DATA(summ)->summarized_genset, 
                                          fresh_gno-1 ));
  }

  /* even though inserting the fresh gno will not upset the 
     summarization data, we still need to expand the 
     gc fields related to summaries (because right now 
     I keep that structure proportional to the number of 
     regions
  */
  {
    int len = DATA(summ)->remset_summaries_count+1;
    int i;
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
    remset_as_summary_t **remset_summaries;
    remset_summaries = 
      (remset_as_summary_t**)must_malloc(len*sizeof(remset_as_summary_t*));
    remset_summaries[0] = NULL;
    for( i = 1; i < fresh_gno; i++ )
      remset_summaries[i] = DATA(summ)->remset_summaries[i];
    remset_summaries[ fresh_gno ] = 
      allocate_remset_as_summary( fresh_gno, DATA(summ)->popularity_limit );
    for( i = fresh_gno+1; i < len; i++ )
      remset_summaries[i] = DATA(summ)->remset_summaries[i-1];
    free(DATA(summ)->remset_summaries);
    DATA(summ)->remset_summaries = remset_summaries;
#endif
    DATA(summ)->remset_summaries_count = len;
  }
}

/* 
 * XXX
 * This is the callback from cheney's scanning of a forwarded object.
 * It is perhaps the most subtle operation to implement.
 * 
 * It will probably be called many times, but I hypothesize:
 * (1.) all invocations for a given lhs are adjacent
 * (2.) any involved lhs is "fresh"
 * 
 * If the above two facts are true, then I should be able to enqueue
 * the lhs in the *array* for g_rhs, rather than the remset for g_rhs.
 * That's important.
 *
 * Note: a gno (for g_rhs) may occur in non-adjacent positions in the
 * invocations (e.g. when scanning an object X = { A1; B2; C1; } where
 * A1,C1 in RGN-1 and B2 in RGN-2).
 *
 * However, I hypothesize:
 * (3.) The set of distinct gen_of(lhs) will be small, and all will
 * occur adjacent to one another in the sequence.
 * 
 * That property is imporant for being able to enqueue in the array for
 * g_rhs *efficiently*; it should allow me to use a similar trick here
 * to the one I plan to use for the summarize pROWduction loop: 
 * keep transient table that maps (SZED u SZING) -> [Maybe Cell].
 */
EXPORT void sm_points_across_callback( summ_matrix_t *summ, word lhs, int g_rhs )
{

  if ( DATA(summ)->summarized_genset_valid &&
       gset_memberp( g_rhs, DATA(summ)->summarized_genset )) {

    /* This assertion is not long for this world; it will disappear
     * once I actually put in sumz arrays.  I have it here to lend
     * credence to the hypotheses above. :) */
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
    static word last_inserted = 0x0;
    assert( last_inserted == lhs ||
            DATA(summ)->remset_summaries[ g_rhs ]->sum_remset == NULL ||
            ! rs_isremembered( DATA(summ)->remset_summaries[ g_rhs ]->sum_remset, lhs ));
    last_inserted = lhs;
#endif

    /* XXX recomputing g_lhs wasteful here (plus it might not be valid
     * yet?  When does cheney change the gno for LOS, before or after
     * scan?); so perhaps change callback to pass g_lhs along too. */
    add_object_to_sum_array( summ, g_rhs, lhs, gen_of(lhs),
                             col_incr_words_gc
#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
                             , ras_incr_words_gc
#endif
                             );
  }
}

EXPORT void sm_before_collection( summ_matrix_t *summ )
{
  if (0) print_matrix( "sm_before_collection", summ );
  assert_no_forwarded_objects( DATA(summ)->nursery_remset );
  check_rep_2( summ );
}

EXPORT void sm_after_collection( summ_matrix_t *summ )
{
  if (0) print_matrix( "sm_after_collection", summ );
  assert_no_forwarded_objects( DATA(summ)->nursery_remset );
  check_rep_2( summ );
}

/* INV: forall i in [1..num_cols) :
 *       union( cols[i]->cell_top{->next_col}*->objects, cols[i]->sum_mutator )
 *      ==
 *       remset_summaries[i]->sum_remset
 */
static void check_cells_against_rs( summ_matrix_t *summ ) 
{
  int i;
  remset_t *cell_rs; /* remset_t-based copy of cells in column */

  cell_rs = grab_from_remset_pool();
  for( i = 1; i < DATA(summ)->num_cols; i++ ) {
    /* build up cell_rs for cols[i] */
    summ_col_t *col;
    summ_cell_t *cell, *sent;
    col = DATA(summ)->cols[i];
    cell = col->cell_top;
    sent = cell;
    do {
      objs_pool_t *objs;
      for( objs = cell->objects; objs != NULL; objs = objs->next ) {
        word *wptr;
        for( wptr = objs->bot; wptr < objs->top; wptr++ ) {
          rs_add_elem( cell_rs, *wptr );
        }
      }
      cell = cell->next_col;
    } while (cell != sent);
    /* need to add in mutator actions */
    if (col->sum_mutator != NULL)
      rs_enumerate( col->sum_mutator, rsenum_add_elem, cell_rs );

#if MAINTAIN_REDUNDANT_RS_AS_SM_REP || USE_REDUNDANT_RS_AS_SM_REP
    /* check that popularity measure matches */
    assert( col->overly_popular == (! DATA(summ)->remset_summaries[i]->valid) );

    /* check that word count matches where expected 
     */
    { 
      remset_as_summary_t *ras = DATA(summ)->remset_summaries[i];
      assert( word_counts_match(col, ras) );
    }

    /* check that copy of column cells matches remset_summary */
    assert_subseteq( cell_rs, DATA(summ)->remset_summaries[i]->sum_remset );
    assert_subseteq( DATA(summ)->remset_summaries[i]->sum_remset, cell_rs );
#endif

    /* clean up */
    rs_clear(cell_rs);
  }
  return_to_remset_pool( cell_rs );
}

static void check_rep( summ_matrix_t *summ ) {
  check_cells_against_rs( summ );
}

/* eof */
