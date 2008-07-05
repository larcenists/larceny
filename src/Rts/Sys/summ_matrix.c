/* Copyright 2008 Felix S Klock II
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

summ_matrix_t *
create_summ_matrix( gc_t *gc, int first_gno, int initial_num_rgns, 
                    double c, double p )
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

  sm->data = data;

  my_prepare_cols( sm, first_gno, first_gno + num_under_construction );

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

word* sm_construction_progress( summ_matrix_t *summ, 
                                word *bot, 
                                word *top,
                                int* word_countdown,
                                int* object_countdown )
{
  print_matrix( summ );
  assert(FALSE);
  print_matrix( summ );
  return bot; 
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


