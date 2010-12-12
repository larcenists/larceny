/* Copyright 2008 Felix S Klock II              -*- indent-tabs-mode: nil -*-
 */
#ifndef INCLUDED_SUMMARY_T_H
#define INCLUDED_SUMMARY_T_H

#define USE_OBJ_OFFS_REP_FOR_LOC_T 0

#if USE_OBJ_OFFS_REP_FOR_LOC_T
struct loc {
  word obj;
  int  offset; /* measured in bytes */
               /* (happens to match tagged fixnum word-count, for now) */
};

static bool loc_equal_p( loc_t loc1, loc_t loc2 )
{
  return (loc1.obj == loc2.obj) && (loc1.offset == loc2.offset);
}
static void assert_loc_ok( loc_t loc )
{
  /* offset is in bytes */
  assert( (loc.offset % sizeof(word)) == 0);
}

static loc_t make_loc( word obj, int offset ) 
{
  loc_t loc;

  loc.obj = obj;
  loc.offset = offset;

  assert_loc_ok( loc );
  return loc;
}

static void clear_loc( loc_t *loc )
{
  loc->obj = 0x0;
}

static bool loc_clear_p(loc_t loc ) 
{
  return (loc.obj == 0x0);
}

static word* loc_to_slot( loc_t l ) 
{
  return (word*)(((byte*)ptrof(l.obj))+l.offset);
}

static word loc_to_obj( loc_t loc )
{
  return loc.obj;
}
static int loc_to_offset( loc_t loc )
{
  return loc.offset;
}
#else
struct loc {
  word *slot;
};

static bool loc_equal_p( loc_t loc1, loc_t loc2 )
{
  return (loc1.slot == loc2.slot);
}

static void assert_loc_ok( loc_t loc )
{
}

static loc_t make_loc( word obj, int offset )
{
  loc_t l;
  l.slot = (word*)(((byte*)ptrof(obj))+offset);
  return l;
}

static void clear_loc( loc_t *loc )
{
  loc->slot = NULL;
}

static bool loc_clear_p( loc_t loc )
{
  return (loc.slot == NULL);
}

static word* loc_to_slot( loc_t l )
{
  return l.slot;
}

static word loc_to_obj( loc_t loc )
{
  assert(0);
}
static int loc_to_offset( loc_t loc )
{
  assert(0);
}
#endif

struct summary {
  int entries;
    /* Count of entries in summary.
       This number is at *least* the number of distinct words in the 
       summary, and at *most* an upper bound on the number of words 
       that a series of invocations of next_chunk would iterate over.

       (The process of iteration does not modify this value; it is 
       set at construction time and left alone thereafter.)
     */

  bool composed_summary;
    /* (set by summary_compose to establish that this is the
       composition of several sub-summaries.) */

  bool enumerate_locations_not_objects;

  bool (*next_chunk)( summary_t *this, /* remaining are "out" parameters */
                      word **start, word **lim );
    /* Simple wrapper around next_chunk_with_flags method; 
       client must assume most conservative results for all flags. 
       Valid only if ! enumerate_locations_not_objects.
    */ 

  bool (*next_chunk_with_flags)( summary_t *this, /* remaining are "out" parameters */
                                 word **start, word **lim, 
                                 bool *all_unseen_before );
    /* If returns false, then this is exhausted, and values of out 
       parameters are unspecified.  If returns true, then [*start,*lim) 
       are a range of words W held in this, and *all_unseen_before tells 
       whether the members of W may be duplicates of words that may have 
       already been seen in the course of the iteration.
       Valid only if ! enumerate_locations_not_objects.
     */

  bool (*next_chunk_enum_locs)( summary_t *this, /* "out" params remain */
                                loc_t **start, loc_t **lim,
                                bool *all_unseen_before );
    /* If returns false, then this is exhausted, and values of out 
       parameters are unspecified.  If returns true, then [*start,*lim) 
       are a range of locations L held in this, and *all_unseen_before tells 
       whether the members of L may be duplicates of locations that may have 
       already been seen in the course of the iteration.
       Valid only if enumerate_locations_not_objects.
     */

  void (*dispose)( summary_t *this );
    /* If non-null, client is responsible for invoking dispose after
       it has finished its iteration.

       (Note that summary_dispose convenience method below checks for
        NULL, so simplest protocol is to unconditionally invoke that
        macro once and only once after iteration is complete.)
    */

  bool (*filter)( summary_t *this, word w );
    /* If non-null, invoked on words during the enumeration; only
     * words for which this returns TRUE are scanned.
     * Valid only if ! enumerate_locations_not_objects
     */

  bool (*filter_loc)( summary_t *this, loc_t l );
    /* If non-null, invoked on words during the enumeration; only
     * words for which this returns TRUE are scanned.
     * Valid only if enumerate_locations_not_objects
     */

  /*** Implementation private state follows ***/

  /* These fields give implementors option of saving cursor state
   * without allocating (and disposing) privately held state. 
   * Note that the init functions below set all of these fields to
   * NULL or 0 as appropriate; if the implementor wants a different
   * initial state, it must set that itself *after* calling the init
   * function. 
   */
  void *cursor1;
  void *cursor2;
  void *cursor3;
  void *cursor4;
  int  icursor1;
  int  icursor2;
  int  icursor3;
  int  icursor4;
};

void summary_init( summary_t *summary, 
                   int entries, 
                   bool (*next_chunk)( summary_t *this, 
                                       word **start,
                                       word **lim,
                                       bool *all_unseen_before ) );
/*
 * Creates a summary, using remaining arguments to initialize its
 * fields.  For obligations of next_chunk fcn ptr, see description of
 * next_chunk_with_flags field above.
 */

void summary_init_dispose( summary_t *summary, 
                           int entries, 
                           bool (*next_chunk)( summary_t *this, 
                                               word **start,
                                               word **lim,
                                               bool *all_unseen_before ), 
                           void (*dispose)( summary_t *this ),
                           bool (*filter)( summary_t *this, word w ));
/*
 * Creates a summary, using remaining arguments to initialize its
 * fields.  For obligations of next_chunk fcn ptr, see description of
 * next_chunk_with_flags field above.
 * Any necessary cleanup can be encoded in the dispose fcn ptr.
 */

void summary_init_locs_dispose
                         ( summary_t *summary, 
                           int entries, 
                           bool (*next_chunk)( summary_t *this, 
                                               loc_t **start,
                                               loc_t **lim,
                                               bool *all_unseen_before ), 
                           void (*dispose)( summary_t *this ),
                           bool (*filter)( summary_t *this, loc_t l ));

void summary_compose( summary_t *fst, summary_t *snd, 
                      summary_t *thd, summary_t *fth, summary_t *recv );
  /* Initializes recv so that it iterates through fst, snd, and thd
     each in turn. */

#if 0
void summary_enumerate( summary_t *summary,
                        void (*scanner)(word loc, void *data, unsigned *stats),
                        void *data );
  /* Invokes scanner on each word produced by iterating through summary.
     Does *not* call summary_dispose when enumeration is complete.
   */

void summary_enumerate_locs( summary_t *summary,
                             void (*scanner)(word *loc, void *data),
                             void *data );
  /* Invokes scanner on each location produced by iterating through summary.
     Does *not* call summary_dispose when enumeration is complete.
   */
#endif

void summary_enumerate_locs2( summary_t *summary,
                              void (*scanner)(loc_t loc, 
                                              void *data),
                              void *data );
  /* Invokes scanner on each location produced by iterating through summary.
     Does *not* call summary_dispose when enumeration is complete.
   */

#define summary_next_chunk( summary, start_var, lim_var ) \
    ((summary)->next_chunk( summary, start_var, lim_var ))

#define summary_next_chunk_with_flags( summary, start_var, lim_var, u_var ) \
    ((summary)->next_chunk_with_flags( summary, start_var, lim_var, u_var ))

#define summary_next_chunk_enum_locs( summary, start_var, lim_var, u_var ) \
  ((summary)->next_chunk_enum_locs( summary, start_var, lim_var, u_var ))

#define summary_dispose( summary ) \
  do { if ((summary)->dispose) { (summary)->dispose( summary ); }} while (0)

#endif
