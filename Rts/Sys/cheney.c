/* Rts/Sys/cheney.c
 * Larceny -- copying garbage collector library
 *
 * $Id$
 *
 * Resist the temptation to shadow gclib_desc_g and gclib_pagebase
 * in locals in the scanning loops -- the low-level memory manager
 * may change them at any time!
 */

#define GC_INTERNAL

#include <memory.h>                /* For memcpy() */
#include "larceny.h"
#include "memmgr.h"
#include "gc_t.h"
#include "semispace_t.h"
#include "los_t.h"
#include "static_heap_t.h"
#include "gclib.h"
#include "barrier.h"


/* Forwarding header (should be defined elsewhere?).

   This bit pattern is an unused immediate and can be generated in a single
   cycle on most machines (it's -2).
   */
#define FORWARD_HDR      0xFFFFFFFE


/* Copy loop implementation.

   FORW_BY_LOOP is biased in favor of smaller structures, which is probably
   the right thing.  The other two have more startup overhead but will
   probably run faster for larger structures.
   */
#define FORW_BY_LOOP     1	/* Double-word copy loop + memcpy */
#define FORW_BY_DUFF     0	/* 1 iteration of Duff's device + memcpy */
#define FORW_BY_MEMCPY   0	/* Memcpy only */


/* Forwarding macros for normal copying collection and promotion.

   Forw_oflo() forwards the contents of a location after obtaining
   enough memory for the copy, if needed.

   Forw_oflo_record() is like forw_oflo() but is used during remembered-set
   scanning.  In addition to forwarding a location in a remembered object,
   it remembers whether any locations in the object were changed.
 
   Forw_core() implements the meat of the forwarding operation.

   Check_space() checks whether the semispace has room for 'wanted' bytes,
   and if not, it expands the semispace and updates the 'dest' and 'lim' 
   variables.

   Scan_core() implements the semispace scanning operation.  It is
   parameterized by an expression that calls the appropriate forwarding macro.

   Remset_scanner_core() scans a single remembered set entry.  It is
   parameterized by an expression that calls the appropriate forwarding macro.
   */

/* Assumes that all parameters except forw_limit_gen are lvalues whose
   evaluation has no side effect and whose value will not change in ways
   not controlled by the macro.  Forw_limit_gen may be any expression.
   */
#define forw_oflo( loc, forw_limit_gen, dest, lim, e ) \
  do { word T_obj = *loc; \
       if (isptr(T_obj) && gclib_desc_g[pageof(T_obj)] < (forw_limit_gen)){ \
          forw_core( T_obj, loc, dest, lim, e ); \
       } \
  } while( 0 )

/* Old_obj_gen is the generation of the object being scanned.
   Note that if the object being scanned is in the generation being collected,
   which happens during a promotion into that generation, then some objects
   will be believed to have intergenerational pointers even though they
   don't, because T_obj_gen is computed only once.  This is OK, because
   the collectors always clear the required remembered sets after a promotion.
   The alternative, to recompute T_obj_gen after forwarding, is more
   expensive.  Since the remembered set should be cleaned anyway to 
   reset pointers and so on, I forego the expense of fixing this macro.
   Same goes for forw_np_record.
   */
#define forw_oflo_record( loc, forw_limit_gen, dest, lim, has_intergen_ptr, \
			  old_obj_gen, e ) \
  do { word T_obj = *loc; \
       if (isptr( T_obj )) { \
          unsigned T_obj_gen = gclib_desc_g[pageof(T_obj)]; \
          if (T_obj_gen < (forw_limit_gen)) { \
            forw_core( T_obj, loc, dest, lim, e ); \
          } \
	  if (T_obj_gen < old_obj_gen) has_intergen_ptr=1; \
       } \
  } while( 0 )

#define forw_core( T_obj, loc, dest, lim, e ) \
  word *TMP_P = ptrof( T_obj ); \
  word TMP_W = *TMP_P; \
  if (TMP_W == FORWARD_HDR) \
    *loc = *(TMP_P+1); \
  else if (tagof( T_obj ) == PAIR_TAG) { \
    check_space(dest,lim,8,e); \
    *dest = TMP_W; \
    *(dest+1) = *(TMP_P+1); \
    *TMP_P = FORWARD_HDR; \
    *(TMP_P+1) = *loc = (word)tagptr(dest, PAIR_TAG); \
    dest += 2; \
  } \
  else { \
    word *TMPD; \
    check_space(dest,lim,sizefield(TMP_W)+4,e); \
    TMPD = dest; \
    *loc = forward( T_obj, &TMPD, e ); dest = TMPD; \
  }

/* Large objects must be handled here so we don't allocate space to
   handle them; by letting the check succeed for large objects, the
   subsequent call to forward() will handle the object properly.
   */
#define check_space( dest, lim, wanted, e ) \
  if ((char*)lim-(char*)dest < (wanted) && (wanted)<=GC_LARGE_OBJECT_LIMIT){ \
    word *CS_LIM=lim, *CS_DEST=dest; \
    expand_semispace( e->tospace, &CS_LIM, &CS_DEST, (wanted) ); \
    dest = CS_DEST; lim = CS_LIM; \
  }

#define scan_core( ptr, iflush, FORW ) \
  do { \
    word T_w = *ptr; \
    assert( T_w != FORWARD_HDR); \
    if (ishdr( T_w )) { \
      word T_h = header( T_w ); \
      if (T_h == BV_HDR) { \
	/* bytevector: skip it, and flush the icache if code */ \
	word *T_oldptr = ptr; \
	word T_bytes = roundup4( sizefield( T_w ) ); \
	ptr = (word *)((word)ptr + (T_bytes + 4)); /* doesn't skip padding */ \
	if (!(T_bytes & 4)) *ptr++ = 0;             /* pad. */ \
	/* Only code vectors typically use a plain bytevector typetag, \
	 * so almost any bytevector will be a code vector that must \
         * be flushed. \
	 */ \
	if (iflush && typetag( T_w ) == BVEC_SUBTAG) \
	  mem_icache_flush( T_oldptr, ptr ); \
      } \
      else { \
	/* vector or procedure: scan in a tight loop */ \
	word T_words = sizefield( T_w ) >> 2; \
	ptr++; \
	while (T_words--) { \
	  FORW; \
	  ptr++; \
	} \
	if (!(sizefield( T_w ) & 4)) *ptr++ = 0; /* pad. */ \
      } \
    } \
    else { \
      FORW; ptr++; FORW; ptr++; \
    } \
  } while (0)

/* 'p' is not local to the macro because it is also used by the expansion 
   of FORW.
   */
#define remset_scanner_core( ptr, p, FORW, count ) \
  p = ptrof( ptr ); \
  if (tagof( ptr ) == PAIR_TAG) { \
    FORW; \
    ++p; \
    FORW; \
    count += 2; \
  } \
  else { \
    word words = sizefield( *p ) / 4; \
    count += words; \
    while (words--) { \
      ++p; \
      FORW; \
    } \
  }


/* Additional forwarding macros for a splitting garbage collection.

   This is a hack and should go away, but it's still used to support
   the Boehm collector by allowing the creation of a clean split heap.

   A splitting gc uses two destination semispaces: one for pointer data
   (ss_data) and one for non-pointer data (ss_text).

   Forw_oflo2() is like forw_oflo(); forw_core2() is like forw_core().
   The same scanning macros are used for this type of collection as
   for a normal collection.

   Check_space2() differs from check_space() in that large objects are
   not handled specially -- during splitting gc, large objects are
   copied the same way as other objects.
   */

#define forw_oflo2( loc, forw_limit_gen, dest, dest2, lim, lim2, e ) \
  do { word T_obj = *loc; \
       if (isptr( T_obj ) && gclib_desc_g[pageof(T_obj)] < (forw_limit_gen)) { \
          forw_core2( T_obj, loc, dest, dest2, lim, lim2, e ); \
       } \
  } while( 0 )

#define forw_core2( T_obj, loc, dest, dest2, lim, lim2, e ) \
  word *TMP_P = ptrof( T_obj ); \
  if (*TMP_P == FORWARD_HDR) \
    *loc = *(TMP_P+1); \
  else if (tagof( T_obj ) == PAIR_TAG) { \
    check_space2(dest,lim,8,e->tospace); /*data*/ \
    *dest = *TMP_P; \
    *(dest+1) = *(TMP_P+1); \
    *TMP_P = FORWARD_HDR; \
    *(TMP_P+1) = *loc = (word)tagptr(dest, PAIR_TAG); \
    dest += 2; \
  } \
  else if (tagof( T_obj ) == BVEC_TAG) { \
    word *TMPD; \
    check_space2(dest2,lim2,sizefield(*TMP_P)+4,e->tospace2); /*text*/ \
    TMPD = dest2; \
    *loc = forward( T_obj, &TMPD, e ); dest2 = TMPD; \
  } \
  else { \
    word *TMPD; \
    check_space2(dest,lim,sizefield(*TMP_P)+4,e->tospace);/*data*/ \
    TMPD = dest; \
    *loc = forward( T_obj, &TMPD, e ); dest = TMPD; \
  }

#define check_space2( dest, lim, wanted, semispace ) \
  if ((char*)lim-(char*)dest < (wanted)) { \
    word *CS_LIM=lim, *CS_DEST=dest; \
    expand_semispace( semispace, &CS_LIM, &CS_DEST, (wanted) ); \
    dest = CS_DEST; lim = CS_LIM; \
  }


/* Additional forwarding macros for non-predictive promotion.

   During non-predictive promotion, objects are moved first into the old
   area until it fills up, then into the young area.  The counter
   e.np.old_steps_remaining keeps track of available space in the old
   area; when a chunk is full and this counter is found to be 0, then
   the old area is full.

   Forw_np() is like forw_oflo() but is only suitable for scanning the
   'old' area after promotion.

   Forw_np_record() is like forw_oflo_record().

   Forw_np_partial() is like forw_np(), but suitable for scanning the
   non-predictive 'young' area after promotion into it.  It records
   whether an object needs to be added to the non-predictive extra
   remembered set.

   Forw_core_np() is like forw_core().

   Check_space_np() is like check_space().

   Scan_core_partial() is like scan_core(), but is useful only when
   scanning the non-predictive 'young' area, and adds an object to the
   non-predictive extra remembered set if forw_np_partial() indicates
   that it must be done.
   */

#define forw_np( loc, forw_limit_gen, dest, lim, e ) \
  do { word T_obj = *loc; \
       if (isptr( T_obj ) && gclib_desc_g[pageof(T_obj)] < (forw_limit_gen)) { \
          forw_core_np( T_obj, loc, dest, lim, e ); \
       } \
  } while( 0 )

/* See comments for forw_oflo_record regarding this implementation. 
   */
#define forw_np_record( loc, forw_limit_gen, dest, lim, has_intergen_ptr, \
		        old_obj_gen, e ) \
  do { word T_obj = *loc; \
       if (isptr( T_obj )) { \
          unsigned T_obj_gen = gclib_desc_g[pageof(T_obj)]; \
          if (T_obj_gen < (forw_limit_gen)) { \
            forw_core_np( T_obj, loc, dest, lim, e ); \
          } \
	  if (T_obj_gen < (old_obj_gen)) has_intergen_ptr=1; \
       } \
  } while( 0 )

/* In general, the generation lookup must be done twice because
   it is possible for an object to be forwarded into the non-predictive
   'old' space during a promotion to both 'old' and 'young'.
   In particular, this happens if one object is forwarded into 'old',
   then a second object, pointing to the first, is forwarded into 'young',
   and then scanned.  The pointer in the second object will then
   change from a pointer into the ephemeral area to a pointer into
   'old'.  The second object must now be added to the non-predictive
   remembered set.

   In the old non-predictive collector, this did not happen, and the logic 
   was less expensive.
   */
#define forw_np_partial( loc, forw_limit_gen, dest, lim, np_young_gen, \
			 must_add_to_extra, e ) \
  do { word T_obj = *loc; \
       if ( isptr( T_obj ) ) { \
           if (gclib_desc_g[ pageof(T_obj) ] < (forw_limit_gen)) { \
             forw_core_np( T_obj, loc, dest, lim, e ); \
           } \
	   T_obj = *loc; \
           if (gclib_desc_g[pageof(T_obj)] < (np_young_gen)) \
             must_add_to_extra = 1; \
       } \
  } while( 0 )

#define forw_core_np( T_obj, loc, dest, lim, e ) \
  word *TMP_P = ptrof( T_obj ); \
  word TMP_W = *TMP_P; \
  if (TMP_W == FORWARD_HDR) \
    *loc = *(TMP_P+1); \
  else if (tagof( T_obj ) == PAIR_TAG) { \
    check_space_np(dest,lim,8,e); \
    *dest = TMP_W; \
    *(dest+1) = *(TMP_P+1); \
    *TMP_P = FORWARD_HDR; \
    *(TMP_P+1) = *loc = (word)tagptr(dest, PAIR_TAG); \
    dest += 2; \
  } \
  else { \
    word *TMPD; \
    check_space_np(dest,lim,sizefield(TMP_W)+4,e); \
    TMPD = dest; \
    *loc = forward( T_obj, &TMPD, e ); dest = TMPD; \
  }

#define check_space_np( dest, lim, wanted, e ) \
  if ((char*)lim-(char*)dest < (wanted) && (wanted)<=GC_LARGE_OBJECT_LIMIT){ \
    word *CS_LIM=lim, *CS_DEST=dest; \
    expand_semispace_np( &CS_LIM, &CS_DEST, (wanted), e ); \
    dest = CS_DEST; lim = CS_LIM; \
  }

#define scan_core_partial( ptr, iflush, FORW, must_add_to_extra, e ) \
  do { \
    word T_w = *ptr; \
    assert( T_w != FORWARD_HDR); \
    if (ishdr( T_w )) { \
      word T_h = header( T_w ); \
      if (T_h == BV_HDR) { \
	/* bytevector: skip it, and flush the icache if code */ \
	word *T_oldptr = ptr; \
	word T_bytes = roundup4( sizefield( T_w ) ); \
	ptr = (word *)((word)ptr + (T_bytes + 4)); /* doesn't skip padding */ \
	if (!(T_bytes & 4)) *ptr++ = 0;            /* pad. */ \
	/* Only code vectors typically use a plain bytevector typetag, \
	 * so almost any bytevector will be a code vector that must \
         * be flushed. \
	 */ \
	if (iflush && typetag( T_w ) == BVEC_SUBTAG) \
	  mem_icache_flush( T_oldptr, ptr ); \
      } \
      else { \
	/* vector or procedure: scan in a tight loop */ \
	word T_words = sizefield( T_w ) >> 2; \
        word* T_objp = ptr; \
        int must_add_to_extra = 0; \
	ptr++; \
	while (T_words--) { \
	  FORW; \
	  ptr++; \
	} \
        if (must_add_to_extra) remember_vec( tagptr( T_objp, VEC_TAG ), e ); \
	if (!(sizefield( T_w ) & 4)) *ptr++ = 0; /* pad. */ \
      } \
    } \
    else { \
      int must_add_to_extra = 0; \
      FORW; \
      ptr++; \
      FORW; \
      ptr++; \
      if (must_add_to_extra) remember_pair( tagptr( ptr-2, PAIR_TAG ), e ); \
    } \
  } while (0)

#define remember_vec( w, e ) \
 do {  **e->np.ssbtop = w; *e->np.ssbtop = *e->np.ssbtop+1; \
       if (*e->np.ssbtop == *e->np.ssblim) { \
         gc_compact_np_ssb( e->gc ); \
       } \
 } while(0)

#define remember_pair( w, e ) remember_vec( w, e )

#define FORW_NP_ENV_BEGIN( e_, dest_, lim_ ) \
  word *dest_ = (e_->np.has_switched ? e->dest2 : e->dest); \
  word *lim_ = (e_->np.has_switched ? e->lim2 : e->lim);

#define FORW_NP_ENV_END( e_, dest_, lim_ ) \
  if (e_->np.has_switched) { e_->dest2 = dest_; e_->lim2 = lim_; } \
  else { e_->dest = dest_; e->lim = lim_; }


/* Parameter container data structure */

typedef struct cheney_env cheney_env_t;
struct cheney_env {
  /* Controlling parameters */
  int  effective_generation;
    /* A generation number s.t. all (or some, see may_be_partial, below) 
       objects from younger generations are copied into tospace.
       */

  bool enumerate_np_remset;
    /* TRUE if the non-predictive 'extra' remembered set needs to be
       enumerated.
       */

  bool scan_static;
    /* TRUE if the static area needs to be scanned in its entirety.
       */

  bool np_promotion;
    /* TRUE if this is a promotion into both the old and young non-predictive
       areas.
       */

  bool splitting;
    /* TRUE if this is a limited 'splitting' garbage collection, used to
       reorganize the heap image into text and data areas.
       */

  bool iflush;
    /* TRUE if the instruction cache must be flushed for the destination
       address of codevectors.
       */

  void (*scanner)( cheney_env_t *e );
    /* Scanner function to be run after roots forwarding.
       */

  gc_t *gc;			/* The garbage collector. */
  los_t *los;			/* The collector's Large Object Space */

  semispace_t *tospace;		/* The first tospace */
  semispace_t *tospace2;	/* The second tospace, or 0 */
  word *dest;			/* Copy pointer of tospace */
  word *dest2;			/* Copy pointer of tospace2, or 0 */
  word *lim;			/* Copy limit of tospace */
  word *lim2;			/* Copy limit of tospace2, or 0 */
  word *scan_ptr;		/* Initial scan pointer in tospace */
  word *scan_ptr2;		/* Initial scan pointer in tospace2, or 0 */
  word *scan_lim;		/* Initial scan limit in tospace */
  word *scan_lim2;		/* Initial scan limit in tospace2, or 0 */
  int  scan_idx;		/* Initially the index of the chunk in 
				   tospace into which scan_ptr and scan_lim
				   point; later, garbage. */
  int  scan_idx2;		/* Ditto for tospace2, or 0 */

  /* Non-predictive promotion */
  struct {
    bool has_switched;		/* 1 if we're now promoting into young */
    int  old_steps_remaining;	/* Number of full steps remaining */
    int  young_steps_remaining;	/* Ditto */
    int  old_los_bytes;		/* Space used by newly promoted objects */
    int  young_los_bytes;	/* Ditto young */
    int  old_los_steps;		/* ceiling( old_los_bytes / stepsize ) */
    int  young_los_steps;	/* Ditto young */
    word **ssbtop;		/* For the NP 'extra' set */
    word **ssblim;		/* Ditto */
  } np;
};


/* External */

extern void mem_icache_flush( void *start, void *end );

/* Private procedures */

static void oldspace_copy( cheney_env_t *e );
static void sweep_large_objects( gc_t *gc, int sweep_oldest, int g1, int g2 );
static void init_env( cheney_env_t *e, gc_t *gc,
		      semispace_t *tospace, semispace_t *tospace2,
		      int  effective_generation,
		      bool np_promotion,
		      bool scan_static,
		      bool enumerate_np_remset,
		      bool splitting,
		      void (*scanner)( cheney_env_t * ) );
static void scan_roots( cheney_env_t *e );
static void scan_static_area( cheney_env_t *e );
static void root_scanner_oflo( word *addr, void *data );
static void root_scanner_np( word *ptr, void *data );
static bool remset_scanner_oflo( word obj, void *data, unsigned *count );
static bool remset_scanner_np( word ptr, void *data, unsigned *count );
static void scan_oflo_normal( cheney_env_t *e );
static void scan_oflo_splitting( cheney_env_t *e );
static void scan_oflo_np_promote( cheney_env_t *e );
static word forward( word, word **, cheney_env_t *e );
static void expand_semispace( semispace_t *, word **, word **, unsigned );
static void expand_semispace_np( word **, word **, unsigned, cheney_env_t* );
static word forward_large_object( cheney_env_t *e, word *ptr, int tag );

void gclib_stopcopy_promote_into( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;

  init_env( &e, gc, tospace, 0, tospace->gen_no, 0, 0, 0, 0,
	    scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no-1, tospace->gen_no, -1 );
}

void gclib_stopcopy_collect( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;

  init_env( &e, gc, tospace, 0, tospace->gen_no+1, 0, 0, 0, 0,
	    scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no, tospace->gen_no, -1 );
}

void gclib_stopcopy_collect_and_scan_static( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;

  init_env( &e, gc, tospace, 0, tospace->gen_no+1, 0, 1, 0, 0,
	    scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no, tospace->gen_no, -1 );
}

void gclib_stopcopy_promote_into_np( gc_t *gc,
				     semispace_t *old, semispace_t *young,
				     int old_remaining, int young_remaining )
{
  cheney_env_t e;

  init_env( &e, gc, old, young, old->gen_no, 1, 0, 0, 0, scan_oflo_np_promote );
  e.np.old_steps_remaining = old_remaining / GC_CHUNK_SIZE;
  e.np.young_steps_remaining = young_remaining / GC_CHUNK_SIZE;
  gc_np_remset_ptrs( gc, &e.np.ssbtop, &e.np.ssblim );

  oldspace_copy( &e );
  gc_compact_np_ssb( gc );
  sweep_large_objects( gc, old->gen_no-1, old->gen_no, young->gen_no );
}

void gclib_stopcopy_collect_np( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;

  init_env( &e, gc, tospace, 0, tospace->gen_no, 0, 0, 1, 0, scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no-1, tospace->gen_no, -1 );
}

void gclib_stopcopy_split_heap( gc_t *gc, semispace_t *data, semispace_t *text )
{
  cheney_env_t e;

  init_env( &e, gc, data, text, data->gen_no+1, 0, 0, 0, 1,
	    scan_oflo_splitting );
  oldspace_copy( &e );
  /* Note: No LOS sweeping */
}

static void
sweep_large_objects( gc_t *gc, int sweep_oldest, int dest, int dest2 )
{
  int i;

  for ( i=0 ; i <= sweep_oldest ; i++ )
    los_sweep( gc->los, i );
  los_append_and_clear_list( gc->los, gc->los->mark1, dest );
  if (dest2 >= 0) los_append_and_clear_list( gc->los, gc->los->mark2, dest2 );
}

static void init_env( cheney_env_t *e, gc_t *gc,
		      semispace_t *tospace, semispace_t *tospace2,
		      int effective_generation,
		      bool np_promotion,
		      bool scan_static,
		      bool enumerate_np_remset,
		      bool splitting,
		      void (*scanner)( cheney_env_t * ) )
{
  assert( !(scan_static && (np_promotion || splitting)) );

  memset( e, 0, sizeof( cheney_env_t ) );
  e->gc = gc;
  e->effective_generation = effective_generation;
  e->scan_static = scan_static;
  e->np_promotion = np_promotion;
  e->enumerate_np_remset = enumerate_np_remset;
  e->splitting = splitting;
  e->iflush = gc_iflush( gc );
  e->tospace = tospace;
  e->tospace2 = tospace2;
  e->dest = tospace->chunks[tospace->current].top;
  e->dest2 = (tospace2 ? tospace2->chunks[tospace2->current].top : 0);
  e->lim = tospace->chunks[tospace->current].lim;
  e->lim2 = (tospace2 ? tospace2->chunks[tospace2->current].lim : 0);
  e->los = (splitting ? 0 : gc->los);
  e->scanner = scanner;
}

static void oldspace_copy( cheney_env_t *e )
{
  e->scan_idx = e->tospace->current;
  e->scan_idx2 = (e->tospace2 ? e->tospace2->current : 0);
  e->scan_ptr = e->tospace->chunks[e->scan_idx].top;
  e->scan_ptr2 = (e->tospace2 ? e->tospace2->chunks[e->scan_idx2].top : 0);
  e->scan_lim = e->tospace->chunks[e->scan_idx].lim;
  e->scan_lim2 = (e->tospace2 ? e->tospace2->chunks[e->scan_idx2].lim : 0);

  scan_roots( e );
  e->scanner( e );

  e->tospace->chunks[e->tospace->current].top = e->dest;
  if (e->tospace2)
    e->tospace2->chunks[e->tospace2->current].top = e->dest2;
}

static void scan_roots( cheney_env_t *e )
{
  void (*roots)( word *, void * );
  bool (*remsets)( word, void *, unsigned * );

  roots = (e->np_promotion ? root_scanner_np : root_scanner_oflo);
  remsets = (e->np_promotion ? remset_scanner_np : remset_scanner_oflo);

  gc_enumerate_roots( e->gc, roots, (void*)e );
  gc_enumerate_remsets_older_than( e->gc, 
				   e->effective_generation-1,
				   remsets,
				   (void*)e,
				   e->enumerate_np_remset );

  if (e->scan_static && e->gc->static_area) {
    scan_static_area( e );
  }
}

static void scan_static_area( cheney_env_t *e )
{
  int         forw_limit_gen = e->effective_generation;
  semispace_t *s_data = e->gc->static_area->data_area;
  word        *dest = e->dest;
  word        *lim = e->lim;
  word        *loc, *limit;
  int         i;

  for ( i=0 ; i <= s_data->current ; i++ ) {
    loc = s_data->chunks[i].bot;
    limit = s_data->chunks[i].top;
    while ( loc < limit )
      scan_core( loc, e->iflush, forw_oflo( loc, forw_limit_gen, dest, lim, e));
  }

  e->dest = dest;
  e->lim = lim;
}

static void root_scanner_oflo( word *ptr, void *data )
{
  cheney_env_t *e = (cheney_env_t*)data;
  forw_oflo( ptr, e->effective_generation, e->dest, e->lim, e );
}

static void root_scanner_np( word *ptr, void *data )
{
  cheney_env_t *e = (cheney_env_t*)data;
  FORW_NP_ENV_BEGIN( e, dest, lim )

  forw_np( ptr, e->effective_generation, dest, lim, e );

  FORW_NP_ENV_END( e, dest, lim )
}

static bool remset_scanner_oflo( word object, void *data, unsigned *count )
{
  cheney_env_t *e = (cheney_env_t*)data;
  unsigned     forw_limit_gen = e->effective_generation;
  unsigned     old_obj_gen = gclib_desc_g[pageof(object)];
  bool         has_intergen_ptr = 0;
  word         *dest = e->dest;
  word         *lim = e->lim;
  word         *loc;		/* Used as a temp by scanner and fwd macros */

  remset_scanner_core( object, loc, 
		       forw_oflo_record( loc, forw_limit_gen, dest, lim,
					 has_intergen_ptr, old_obj_gen, e ),
		       *count );

  e->dest = dest;
  e->lim = lim;
  return has_intergen_ptr;
}

static bool remset_scanner_np( word object, void *data, unsigned *count )
{
  cheney_env_t *e = (cheney_env_t*)data;
  unsigned     forw_limit_gen = e->effective_generation;
  unsigned     old_obj_gen = gclib_desc_g[pageof(object)];
  bool         has_intergen_ptr = 0;
  word         *loc;		/* Used as a temp by scanner and fwd macros */
  FORW_NP_ENV_BEGIN( e, dest, lim )

  remset_scanner_core( object, loc, 
		       forw_np_record( loc, forw_limit_gen, dest, lim,
				       has_intergen_ptr, old_obj_gen, e ),
		       *count );

  FORW_NP_ENV_END( e, dest, lim )
  return has_intergen_ptr;
}

static void scan_oflo_normal( cheney_env_t *e )
{
  unsigned gno = e->effective_generation;
  word     *scanptr = e->scan_ptr;
  word     *scanlim = e->scan_lim;
  word     *dest = e->dest;
  word     *copylim = e->lim;
  word     *los_p = 0, *p;
  int      morework;

  do {
    morework = 0;

    while (scanptr != dest) {
      while (scanptr != dest && scanptr < scanlim) {
	scan_core( scanptr, e->iflush,
		   forw_oflo( scanptr, gno, dest, copylim, e ));
      }

      if (scanptr != dest) {
	e->scan_idx++;
	scanptr = e->tospace->chunks[e->scan_idx].bot;
	scanlim = e->tospace->chunks[e->scan_idx].lim;
      }
    }

    while ((p = los_walk_list( e->los->mark1, los_p )) != 0) {
      los_p = p;
      morework = 1;
      assert( ishdr( *p ) );
      scan_core( p, e->iflush, forw_oflo( p, gno, dest, copylim, e ));
    }
  } while (morework);

  e->dest = dest;
  e->lim = copylim;
}

static void scan_np_old( cheney_env_t *e );
static void scan_np_young( cheney_env_t *e );
static void scan_np_los_old( cheney_env_t *e, word **los_p_arg );
static void scan_np_los_young( cheney_env_t *e, word **los_p_arg );

static void scan_oflo_np_promote( cheney_env_t *e )
{
  word *los_p_old = 0, *los_p_young = 0;
  bool work;

  do {
    if (e->scan_ptr == e->scan_lim && e->scan_idx < e->tospace->current) {
      e->scan_idx++;
      e->scan_ptr = e->tospace->chunks[ e->scan_idx ].bot;
      e->scan_lim = e->tospace->chunks[ e->scan_idx ].lim;
    }
    if (e->scan_ptr2 == e->scan_lim2 && e->scan_idx2 < e->tospace2->current) {
      e->scan_idx2++;
      e->scan_ptr2 = e->tospace2->chunks[ e->scan_idx2 ].bot;
      e->scan_lim2 = e->tospace2->chunks[ e->scan_idx2 ].lim;
    }

    work = 0;
    if (e->scan_ptr != e->scan_lim && e->scan_ptr != e->dest) {
      scan_np_old( e );
      work=1;
    }
    if (los_walk_list( e->los->mark1, los_p_old ) != 0) {
      scan_np_los_old( e, &los_p_old );
      work=1;
    }
    if (e->scan_ptr2 != e->scan_lim2 && e->scan_ptr2 != e->dest2) {
      scan_np_young( e );
      work=1;
    }
    if (los_walk_list( e->los->mark2, los_p_young ) != 0) {
      scan_np_los_young( e, &los_p_young );
      work=1;
    }
  } while( work );

  assert( e->scan_idx == e->tospace->current );
  assert( e->scan_idx2 == e->tospace2->current );
}

static void scan_np_old( cheney_env_t *e )
{
  unsigned forw_limit_gen = e->tospace->gen_no;
  word     *scanptr = e->scan_ptr;
  word     *scanlim = e->scan_lim;
  FORW_NP_ENV_BEGIN( e, dest, copylim )

  while (scanptr != dest && scanptr < scanlim) {
    scan_core( scanptr, e->iflush,
	       forw_np( scanptr, forw_limit_gen, dest, copylim, e ) );
  }

  e->scan_ptr = scanptr;
  e->scan_lim = scanlim;
  FORW_NP_ENV_END( e, dest, copylim )
}

static void scan_np_young( cheney_env_t *e )
{
  unsigned forw_limit_gen = e->tospace->gen_no;	/* [sic] */
  unsigned np_young_gen = e->tospace2->gen_no;
  word     *scanptr = e->scan_ptr2;
  word     *scanlim = e->scan_lim2;
  FORW_NP_ENV_BEGIN( e, dest, copylim )

  /* must_add_to_extra is a name used by the scanning and fwd macros as a temp*/
  while (scanptr != dest && scanptr < scanlim) {
    scan_core_partial( scanptr, e->iflush,
		       forw_np_partial( scanptr, forw_limit_gen, dest, copylim, 
					np_young_gen, must_add_to_extra, e ),
		       must_add_to_extra, e );
  }

  e->scan_ptr2 = scanptr;
  e->scan_lim2 = scanlim;
  FORW_NP_ENV_END( e, dest, copylim )
}

static void scan_np_los_old( cheney_env_t *e, word **los_p )
{
  unsigned forw_limit_gen = e->tospace->gen_no;
  FORW_NP_ENV_BEGIN( e, dest, copylim )
  word     *p;

  while ((p = los_walk_list( e->los->mark1, *los_p )) != 0) {
    *los_p = p;
    assert( ishdr( *p ) );
    scan_core( p, e->iflush,
	       forw_np( p, forw_limit_gen, dest, copylim, e ));
  }

  FORW_NP_ENV_END( e, dest, copylim )
}

static void scan_np_los_young( cheney_env_t *e, word **los_p )
{
  unsigned forw_limit_gen = e->tospace->gen_no;      /* [sic] */
  unsigned np_young_gen = e->tospace2->gen_no;
  FORW_NP_ENV_BEGIN( e, dest, copylim )
  word     *p;

  /* must_add_to_extra is a name used by the scanning and fwd macros as a temp*/
  while ((p = los_walk_list( e->los->mark2, *los_p )) != 0) {
    *los_p = p;
    assert( ishdr( *p ) );
    scan_core_partial( p, e->iflush,
		       forw_np_partial( p, forw_limit_gen, dest, copylim, \
				        np_young_gen, must_add_extra, e ),
		       must_add_extra, e );
  }

  FORW_NP_ENV_END( e, dest, copylim )
}

static void scan_oflo_splitting( cheney_env_t *e )
{
  unsigned forw_limit_gen = e->effective_generation;
  word     *scanptr = e->scan_ptr;
  word     *scanlim = e->scan_lim;
  word     *dest = e->dest;
  word     *dest2 = e->dest2;
  word     *copylim = e->lim;
  word     *copylim2 = e->lim2;

  while (scanptr != dest) {
    while (scanptr != dest && scanptr < scanlim) {
      scan_core( scanptr, e->iflush,
		 forw_oflo2( scanptr, forw_limit_gen, dest, dest2,
			     copylim, copylim2, e ));
    }

    if (scanptr != dest) {
      e->scan_idx++;
      scanptr = e->tospace->chunks[e->scan_idx].bot;
      scanlim = e->tospace->chunks[e->scan_idx].lim;
    }
  }

  e->dest = dest;
  e->dest2 = dest2;
  e->lim = copylim;
  e->lim2 = copylim2;
}


/* "p" is a tagged pointer into oldspace;
 * "*dest" is a pointer into newspace, the destination of the next object.
 *
 * Forward() returns the forwarding value of "ptr"; it does this by
 * copying the object and returning the new address.
 *
 * Most objects are smallish, so this code should be biased in favor
 * of small objects.
 */
static word forward( word p, word **dest, cheney_env_t *e )
{
  word hdr, newptr, *p1, *p2, tag, *ptr;

  tag = tagof( p ); 
  ptr = ptrof( p );

  /* Copy the structure into newspace and pad if necessary. */
  p1 = *dest;
  newptr = (word)p1;    /* really *dest, but the compiler is dumb. */
  p2 = ptr;

  hdr = *ptr;
  assert( ishdr( hdr ) );

#if FORW_BY_LOOP
  { unsigned words;

    /* gcc gets this right, so no sense in being obscure.
       words = (((hdr >> 8) + 11) >> 3) << 1; */
    words = roundup8( sizefield( hdr ) + 4 ) / 4;

    /* 32 is pretty arbitrary; chosen to match overhead of memcpy(). */
    /* gcc doesn't schedule real well. */
    if (words < 32) {
      while (words > 0) {
        p1[0] = p2[0];
	p1 += 2;
	p1[-1] = p2[1];
	p2 += 2;
	words -= 2;
      }
    }
    else if (words > GC_LARGE_OBJECT_LIMIT/4 && e->los) 
      return forward_large_object( e, ptr, tag );
    else {
      memcpy( p1, p2, words*4 );
      p1 += words;
    }
    *dest = p1;
  }
#endif

#if FORW_BY_DUFF
  { unsigned bytes;

  bytes = roundup8( sizefield( hdr ) + 4 );
  if (bytes > GC_LARGE_OBJECT_LIMIT && los) 
    return forward_large_object( e, ptr, tag );

  switch (bytes >> 3) {
    case 8  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 7  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 6  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 5  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 4  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 3  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 2  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 1  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 0  : break;
    default : memcpy( p1, p2, bytes );  p1 += (bytes >> 2);
  }
  *dest = p1;
  }
#endif

#if FORW_BY_MEMCPY
  { unsigned bytes;
  /* This appears to be slowest */
  bytes = roundup8( sizefield( hdr ) + 4 );
  if (bytes > GC_LARGE_OBJECT_LIMIT && los)
    return forward_large_object( e, ptr, tag );

  memcpy( p1, p2, bytes );
  *dest = p1 + (bytes >> 2);
  }
#endif

  newptr = (word) tagptr( newptr, tag );

  /* leave forwarding pointer */
  *ptr = FORWARD_HDR;
  *(ptr+1) = newptr;

  return newptr;
}


/* Expand the semispace by switching to a new chunk.  To make things
   simple for the scanning code, a string header is put into the
   heap at the current location (unless we're at the end of the chunk)
   with a length field that covers the rest of the chunk.  The scanning
   code will simply skip the data.  The string's first four bytes
   are initialized with a recognizable but unlikely pattern, in the event
   any code needs to grovel over the heap and count padding.

   A string header is used rather than a generic bytevector header since
   the latter value would cause the scanner to flush the icache for the
   garbage area.
   */
static void seal_chunk( semispace_t *ss, word *lim, word *dest )
{
  if (dest < lim) {
    word len = (lim - dest)*sizeof(word);
    *dest = mkheader(len-sizeof(word),STR_HDR);
    *(dest+1) = 0xABCDABCD;
  }
  ss->chunks[ ss->current ].top = dest;
}

static void
expand_semispace( semispace_t *ss, word **lim, word **dest, unsigned bytes )
{
  seal_chunk( ss, *lim, *dest );
  ss_expand( ss, max( bytes, GC_CHUNK_SIZE ) );
  *lim = ss->chunks[ss->current].lim;
  *dest = ss->chunks[ss->current].top;
}

static void
expand_semispace_np( word **lim, word **dest, unsigned bytes, cheney_env_t *e )
{
  semispace_t *ss;

 again:
  if (e->np.has_switched) {
    ss = e->tospace2;
    e->np.young_steps_remaining--;
  }
  else if (e->np.old_steps_remaining - e->np.old_los_steps > 0) {
    ss = e->tospace;
    e->np.old_steps_remaining--;
  }
  else {
    e->np.has_switched = 1;
    e->dest = *dest;
    e->lim = *lim;
    seal_chunk( e->tospace, *lim, *dest );
    ss = e->tospace2;
    *dest = ss->chunks[ ss->current ].top; /* [sic] */
    *lim = ss->chunks[ ss->current ].lim;
    if ((char*)*lim - (char*)*dest < bytes) goto again; else return;
  }
  seal_chunk( ss, *lim, *dest );
  ss_expand( ss, max( bytes, GC_CHUNK_SIZE ) );
  *dest = ss->chunks[ ss->current ].bot;
  *lim = ss->chunks[ ss->current ].lim;
}

static word forward_large_object( cheney_env_t *e, word *ptr, int tag )
{
  los_t *los = e->los;
  los_list_t *mark_list;
  word hdr, ret;
  int bytes, was_marked, sub1 = 0, sub2 = 0;

  hdr = *ptr;
  bytes = roundup8( sizefield( hdr ) + 4 );

  mark_list = los->mark1;
  sub1 = bytes;
  if (e->np_promotion) {
    int free1, free2;

    free1 = e->np.old_steps_remaining*GC_CHUNK_SIZE - los_bytes_used( los, -1 );
    free2 = e->np.young_steps_remaining*GC_CHUNK_SIZE - los_bytes_used(los, -2);

    if (bytes > free1 && bytes <= free2) {
      mark_list = los->mark2;
      sub2 = sub1; sub1 = 0;
    }
  }

  if (gclib_desc_b[pageof(ptr)] & MB_LARGE_OBJECT) {
    was_marked = los_mark( los, mark_list, ptr, gclib_desc_g[ pageof(ptr) ] );
    ret = tagptr( ptr, tag );
  }
  else {
    /* The large object was not allocated specially, so we must move it. */
    word *new;

    hdr = *ptr;
    bytes = roundup8( sizefield( hdr ) + 4 );
    new = los_allocate( los, bytes, gclib_desc_g[ pageof( ptr ) ] );
    memcpy( new, ptr, bytes );
    
    /* Must mark it also! */
    was_marked = los_mark( los, mark_list, new, gclib_desc_g[ pageof( ptr ) ] );
    
    /* Leave a forwarding pointer */
    *ptr = FORWARD_HDR;
    *(ptr+1) = tagptr( new, tag );
    ret = *(ptr+1);
  }

  if (e->np_promotion && !was_marked) {
    e->np.old_los_bytes += sub1;
    e->np.young_los_bytes += sub2;
    e->np.old_los_steps = ceildiv( e->np.old_los_bytes, GC_CHUNK_SIZE );
    e->np.young_los_steps = ceildiv( e->np.young_los_bytes, GC_CHUNK_SIZE );
  }
  return ret;
}

/* eof */
