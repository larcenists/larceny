/* Rts/Sys/cheney.c
 * Larceny run-time system -- copying garbage collector library.
 *
 * $Id: cheney.c,v 1.13 1997/09/17 15:17:26 lth Exp $

Look, let's just face it: this module needs to be cleaned up eventually.
Some procedures are no longer used; some functionality (like the split
scanning) was never really used at all.  The static-heap reorg should be
moved somewhere else and just use a dumb mark+copy routine; it's not
performance critical and it clutters the code.  The abstraction level of
the collector/heaps/remsets should be _lowered_ so that we don't have to
simulate higher-order functions all over the place -- for example,
remembered-set scanning could be done in a simple loop rather than with
the callbacks and macros and the whole bit.  With the experience we
currently have and the Hicks/Moore/Nettles paper in mind, we should
decide on low-level mechanisms once and for all, and leave it at that.
Massive improvements should come from higher-level algorithms.

For the sake of expediency in reimplementing the promotion/expansion
policies and the Large Object Space, however, I have not yet cleaned up
this module, as it would take a lot of effort to do so properly.

 *
 *
 * Description.
 *
 * The code in this file is reentrant.
 *
 * Bugs/Discussion/FIXMEs.
 *
 * - finesse the use of the write barrier's SSBs -- is it OK the way it
 *   is now, or should we clean it up properly, and if so, how?
 *   (I don't understand this comment any more. --lars)
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

/* This bit pattern is an unused immediate and can be generated in a single
   cycle on most machines (it's -2).
   */
#define FORWARD_PTR 0xFFFFFFFE     /* Should be defined somewhere else */


/* The forw_oflo() macro forwards a pointer after checking for overflow
 * and obtaining enough memory in the semispace to perform the fowarding.
 * A generation ownership check on the page is used to check that the
 * pointer points into a heap area with generation number lower than that
 * of the parameter 'gno'.
 *
 * forw_oflo() assumes that all parameters except gno are variable names
 * (lvalues whose evaluation has no side effect); gno may be any expression.
 *
 * static void forw_oflo( word *p, unsigned gno, word *dest, word *lim,
 *                        semispace_t *semispace, los_t *los )
 */

#define forw_oflo( p, gno, dest, lim, semispace, los ) \
  do { word TMP2 = *p; \
       if (isptr( TMP2 ) && gclib_desc_g[pageof(TMP2)] < gno) { \
          forw_core( TMP2, p, dest, check_space, lim, semispace, los ); \
       } \
  } while( 0 )


/* forw_oflo2() works like forw_oflo() except that data is copied into
 * two semispaces: one for pointer-containing data (ss_data) and one
 * for non-pointer-containing data (ss_text).  This is way useful for
 * creating static areas, but it could perhaps be used to maintain
 * a BiBoP-ish heap too?
 */

#define forw_oflo2( p, gno, dest, dest2, lim, lim2, ss_data, ss_text, los ) \
  do { word TMP2 = *p; \
       if (isptr( TMP2 ) && gclib_desc_g[pageof(TMP2)] < gno) { \
          forw_core2( TMP2, p, dest, dest2, check_space, lim, lim2, ss_data, ss_text, los ); \
       } \
  } while( 0 )


/* forw_oflo_record() is like forw_oflo() except that 'bit' is set to 1 if
 * the word being forwarded is a pointer into a younger generation than
 * the generation of the object it resides in; the latter generation is
 * passed in in the variable obj_gen.
 *
 * Used for remset scanning _only_.
 */
#define forw_oflo_record( p, gno, dest, lim, semispace, bit, obj_gen, los ) \
  do { word TMP2 = *p; \
       if (isptr( TMP2 )) { \
          unsigned T_genw = gclib_desc_g[pageof(TMP2)]; \
          if (T_genw < gno) { \
            forw_core( TMP2, p, dest, check_space, lim, semispace, los ); \
          } \
	  if (T_genw < obj_gen) bit=1; \
       } \
  } while( 0 )


/* The forw_oflo_partial() macro is used only by scan_core_partial,
 * when promoting from the ephemeral area into steps 1 through j of
 * a non-predictive heap.
 * The forw_oflo_partial() macro does everything that forw_oflo() does,
 * and also sets T_bit if the object being traced needs to be added to
 * the remembered set.
 *
 * forw_oflo_partial() assumes that all parameters except gno are
 * variable names (lvalues whose evaluation has no side effect);
 * gno may be any expression.
 *
 * static void forw_oflo( word *p, unsigned gno, word *dest, word *lim,
 *                        semispace_t *semispace, int T_obj_gen )
 */

#define forw_oflo_partial( p, gno, dest, lim, semispace, T_obj_gen, T_bit, los ) \
  do {  word TMP2 = *p; \
        if ( isptr( TMP2 ) ) { \
            int TMP3 = gclib_desc_g[pageof(TMP2)]; \
            if ( TMP3 < gno ) { \
                forw_core( TMP2, p, dest, check_space, lim, semispace, los ); \
            } \
            else if ( TMP3 < T_obj_gen ) \
                T_bit = 1; \
        } \
  } while( 0 )


/* The forw_core() macro implements most of the forwarding operation.
 * In the case of forw(), the checking code expands to a no-op; in the
 * case of forw_oflo, a check will take place.
 */

#define forw_core( TMP2, p, dest, check, lim, semispace, los ) \
  word *TMP_P = ptrof( TMP2 ); \
  word TMP_W = *TMP_P; \
  if (TMP_W == FORWARD_PTR) \
    *p = *(TMP_P+1); \
  else if (tagof( TMP2 ) == PAIR_TAG) { \
    check(dest,lim,8,semispace); \
    *dest = TMP_W; \
    *(dest+1) = *(TMP_P+1); \
    *TMP_P = FORWARD_PTR; \
    *(TMP_P+1) = *p = (word)tagptr(dest, PAIR_TAG); \
    dest += 2; \
  } \
  else { \
    word *TMPD; \
    check(dest,lim,sizefield(TMP_W)+4,semispace); \
    TMPD = dest; \
    *p = forward( TMP2, &TMPD, los ); dest = TMPD; \
  }


/* forw_core2() is just like forw_core() except that it copies objects
 * into two semispaces depending on type; see comments for forw_oflo2().
 */

#define forw_core2( TMP2, p, dest, dest2, check, lim, lim2, ss_data, ss_text, los ) \
  word *TMP_P = ptrof( TMP2 ); \
  if (*TMP_P == FORWARD_PTR) \
    *p = *(TMP_P+1); \
  else if (tagof( TMP2 ) == PAIR_TAG) { \
    check(dest,lim,8,ss_data); \
    *dest = *TMP_P; \
    *(dest+1) = *(TMP_P+1); \
    *TMP_P = FORWARD_PTR; \
    *(TMP_P+1) = *p = (word)tagptr(dest, PAIR_TAG); \
    dest += 2; \
  } \
  else if (tagof( TMP2 ) == BVEC_TAG) { \
    word *TMPD; \
    check(dest2,lim2,sizefield(*TMP_P)+4,ss_text); \
    TMPD = dest2; \
    *p = forward( TMP2, &TMPD, los ); dest2 = TMPD; \
  } \
  else { \
    word *TMPD; \
    check(dest,lim,sizefield(*TMP_P)+4,ss_data); \
    TMPD = dest; \
    *p = forward( TMP2, &TMPD, los ); dest = TMPD; \
  }


/* The check_space() macro expands into code that checks whether the
 * semispace has room for 'wanted' bytes, and if not, it expands the
 * semispace and updates the 'dest' and 'lim' variables.  Local temporaries
 * are passed by reference to expand_semispace rather than the addresses
 * of 'dest' and 'lim' to prevent the compiler from getting confused about
 * possible aliasing of the latter variables.
 *
 * Large objects must be handled here so we don't allocate space to
 * handle them; by letting the check succeed for large objects, the
 * subsequent call to forward() will handle the object properly.
 */

#define check_space( dest, lim, wanted, semispace ) \
  if ((char*)lim-(char*)dest < (wanted) && (wanted)<=GC_LARGE_OBJECT_LIMIT){ \
    word *CS_LIM=lim, *CS_DEST=dest; \
    expand_semispace( semispace, &CS_LIM, &CS_DEST, (wanted) ); \
    dest = CS_DEST; lim = CS_LIM; \
  }


/* The remset_scanner_core() macro implements most of the logic of scanning
 * a single remembered set entry.  It is parameterized by the forwarding
 * logic; the user of the macro must pass an expression that expands into
 * the correct forwarding call.
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

/*  The extra checking bit:

  if *ptr is a pointer into an intermediate generation between
    the current target generation and the oldest generation being
    considered for forwarding (indeed, if *ptr still points to
    any younger generation!) then we must remember the present
    object.  We need remember an object only once, so set a bit
    and check the bit afterwards.

   NOTE!!! a tag VEC_TAG is used even when remembering procedures.

*/

#define scan_core_partial( ptr, dest, iflush, FORW, T_bit ) \
  do { \
    word T_w = *ptr; \
    assert( T_w != FORWARD_PTR); \
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
        word* T_obj = ptr; \
        int T_bit = 0; \
	ptr++; \
	while (T_words--) { \
	  FORW; \
	  ptr++; \
	} \
        if (T_bit) remember_vec( tagptr( T_obj, VEC_TAG ) ); \
	if (!(sizefield( T_w ) & 4)) *ptr++ = 0; /* pad. */ \
      } \
    } \
    else { \
      int T_bit = 0; \
      FORW; \
      ptr++; \
      FORW; \
      ptr++; \
      if (T_bit) remember_pair( tagptr( ptr-2, PAIR_TAG ) ); \
    } \
  } while (0)


#define remember_vec( w ) \
 do {  **ssbtop = w; *ssbtop = *ssbtop+1; \
       if (*ssbtop == *ssblim) { \
         gc_compact_np_ssb( gc ); \
       } \
 } while(0)

#define remember_pair( w ) remember_vec( w )



/* The scan_core() macro implements the semispace scanning operation.
 * It is parameterized by the forwarding macro, which should be either
 * a call to forw() or a call to forw_oflo(), with the right parameters.
 */
#define scan_core( ptr, dest, iflush, FORW ) \
  do { \
    word T_w = *ptr; \
    assert( T_w != FORWARD_PTR); \
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



/* Types used to emulate closures during scanning. */

typedef struct fast_env fast_env_t;
struct fast_env {
  word *oldlo;
  word *oldhi;
  word *dest;
  los_t *los;
};

typedef struct oflo_env oflo_env_t;
struct oflo_env {
  unsigned gno;
  word *dest;
  word *dest2;
  word *lim;
  word *lim2;
  semispace_t *tospace;
  semispace_t *tospace2;
  gc_t *gc;
  word **ssbtop;
  word **ssblim;
  los_t *los;
  caddr_t pagebase;
  unsigned *desc_g;
};


/* External */

extern void mem_icache_flush( void *start, void *end );

/* Private procedures */

static void oldspace_copy( gc_t *gc, 
			   semispace_t *tospace, semispace_t *tospace2, 
			   int effective_generation,
			   bool may_be_partial,
			   bool scan_static,
			   bool enumerate_np_remset 
			  );
static void scan_static_area( gc_t *gc, oflo_env_t *e );
static void root_scanner_oflo( word *addr, void *data );
static int  remset_scanner_oflo( word obj, void *data, unsigned *count );
static void scan_oflo(word *scanptr, word *scanlim,
		      unsigned starti, oflo_env_t *e, int iflush,
		      int may_be_partial );
static word forward( word, word **, los_t * );
static void expand_semispace( semispace_t *, word **, word **, unsigned );
static word forward_large_object( los_t *los, word *ptr, int tag );
static void sweep_large_objects( gc_t *gc, int sweep_oldest, int dest );

void gclib_stopcopy_promote_into( gc_t *gc, semispace_t *tospace )
{
  oldspace_copy( gc, tospace, 0, tospace->gen_no, 0, 0, 0 );
  sweep_large_objects( gc, tospace->gen_no-1, tospace->gen_no );
}

void gclib_stopcopy_collect( gc_t *gc, semispace_t *tospace )
{
  oldspace_copy( gc, tospace, 0, tospace->gen_no+1, 0, 0, 0 );
  sweep_large_objects( gc, tospace->gen_no, tospace->gen_no );
}

void gclib_stopcopy_collect_and_scan_static( gc_t *gc, semispace_t *tospace )
{
  oldspace_copy( gc, tospace, 0, tospace->gen_no+1, 0, 1, 0 );
  sweep_large_objects( gc, tospace->gen_no, tospace->gen_no );
}

void gclib_stopcopy_promote_into_np_young( gc_t *gc, semispace_t *tospace )
{
  oldspace_copy( gc, tospace, 0, tospace->gen_no-1, 1, 0, 0 );
  sweep_large_objects( gc, tospace->gen_no-2, tospace->gen_no );
}

void gclib_stopcopy_collect_np( gc_t *gc, semispace_t *tospace )
{
  oldspace_copy( gc, tospace, 0, tospace->gen_no, 0, 0, 1 );
  sweep_large_objects( gc, tospace->gen_no-1, tospace->gen_no );
}


void
gclib_stopcopy_split_heap( gc_t *gc, semispace_t *data, semispace_t *text )
{
  oldspace_copy( gc, data, text, data->gen_no+1, 0, 0, 0 );
}

/* 'effective_generation' is a generation number s.t. all objects from
 * younger generations are copied into tospace.  When promoting, the
 * effective generation is the generation number of tospace, and when 
 * copying in an older generation, it is that generation's generation number,
 * plus 1.
 *
 * 'may_be_partial' is 1 if there are generations younger than tospace
 * from which objects may not be copied, i.e., if
 *     effective_generation < gen(tospace)
 */

static void 
oldspace_copy( gc_t *gc,
	       semispace_t *tospace,             /* data or both */ 
 	       semispace_t *tospace2,            /* text or 0 */
	       int effective_generation,
	       bool may_be_partial,
	       bool scan_static,
	       bool enumerate_np_remset 
	      )
{
  word *scanptr;
  unsigned scan_chunk_idx;
  oflo_env_t e;
  word *scanlim;

  scan_chunk_idx = tospace->current;
  scanptr = tospace->chunks[scan_chunk_idx].top;
  e.dest = tospace->chunks[scan_chunk_idx].top;
  e.dest2 = (tospace2 ? tospace2->chunks[tospace2->current].top : 0);
  e.lim = tospace->chunks[scan_chunk_idx].lim;
  e.lim2 = (tospace2 ? tospace2->chunks[tospace2->current].lim : 0);
  e.tospace = tospace;
  e.tospace2 = tospace2;
  e.gc = gc;
  e.los = gc->los;
  e.pagebase = gclib_pagebase;
  e.desc_g = gclib_desc_g;

  /* assert( !scan_static || scan_static && !may_be_partial ); */ /* FIXME */
  e.gno = effective_generation;
  gc->np_remset_ptrs( gc, &e.ssbtop, &e.ssblim );

  /* Forward the roots */
  gc_enumerate_roots( gc, root_scanner_oflo, (void*)&e );
  gc_enumerate_remsets_older_than( gc, 
				   effective_generation-1,
				   remset_scanner_oflo, 
				   (void*)&e,
				   enumerate_np_remset );
  if (scan_static && gc->static_area) 
    scan_static_area( gc, &e );

  /* Scan */
  scanlim = tospace->chunks[scan_chunk_idx].lim;
  scan_oflo( scanptr, scanlim, scan_chunk_idx, &e, gc->iflush(gc,e.gno),
	    may_be_partial
	    );

  tospace->chunks[tospace->current].top = e.dest;
  if (tospace2)
    tospace2->chunks[tospace2->current].top = e.dest2;
}

static void scan_static_area( gc_t *gc, oflo_env_t *e )
{
  /* gclib_pagebase and gclib_desc_g shadow globals of same name */
  caddr_t gclib_pagebase = e->pagebase;
  unsigned *gclib_desc_g = e->desc_g;
  word *dest = e->dest;
  word *lim = e->lim;
  int gno = e->gno;
  semispace_t *tospace = e->tospace;
  los_t *los = e->los;
  semispace_t *s_data;
  int i, iflush;
  word *p, *limit;

  iflush = gc_iflush( gc, gno );
  s_data = gc->static_area->data_area;

  for ( i=0 ; i <= s_data->current ; i++ ) {
    p = s_data->chunks[i].bot;
    limit = s_data->chunks[i].top;
    while ( p < limit )
      scan_core( p, dest, iflush, forw_oflo( p, gno, dest, lim, tospace, los));
  }

  e->dest = dest;
  e->lim = lim;
}

static void root_scanner_oflo( word *ptr, void *data )
{
  oflo_env_t *e = (oflo_env_t*)data;
  forw_oflo( ptr, e->gno, e->dest, e->lim, e->tospace, e->los );
}

static int remset_scanner_oflo( word ptr, void *data, unsigned *count )
{
  oflo_env_t *e = (oflo_env_t*)data;
  word *dest = e->dest, *lim = e->lim;
  unsigned gno = e->gno;
  semispace_t *s = e->tospace;
  word *p;
  int bit=0;
  unsigned obj_gen = gclib_desc_g[pageof(ptr)];

  remset_scanner_core( ptr, p, 
		forw_oflo_record( p, gno, dest, lim, s, bit, obj_gen, e->los ),
		*count );

  e->dest = dest;
  e->lim = lim;
  return bit;
}

/* The scan_oflo procedure has been split up into three procedures to get
   better precision when doing profiling.
   */

static void 
scan_oflo_normal( word *scanptr, word *scanlim, unsigned scan_chunk_idx,
		  oflo_env_t *e, int iflush );
static void 
scan_oflo_splitting( word *scanptr, word *scanlim, unsigned scan_chunk_idx,
		    oflo_env_t *e, int iflush );
static void 
scan_oflo_np_promote( word *scanptr, word *scanlim, unsigned scan_chunk_idx,
		      oflo_env_t *e, int iflush );

static void 
scan_oflo( word *scanptr, word *scanlim, unsigned scan_chunk_idx,
	   oflo_env_t *e, int iflush, int may_be_partial )
{
  if (may_be_partial)
    /* NP promotion into 'young' */
    scan_oflo_np_promote( scanptr, scanlim, scan_chunk_idx, e, iflush );
  else if (e->dest2 != 0)
    /* Splitting the heap */
    scan_oflo_splitting( scanptr, scanlim, scan_chunk_idx, e, iflush );
  else
    /* No magic */
    scan_oflo_normal( scanptr, scanlim, scan_chunk_idx, e, iflush );
}

static void 
scan_oflo_normal( word *scanptr, word *scanlim, unsigned scan_chunk_idx,
		  oflo_env_t *e, int iflush )
{
  /* gclib_pagebase and gclib_desc_g shadow globals of same name */
  caddr_t gclib_pagebase = e->pagebase;
  unsigned *gclib_desc_g = e->desc_g;
  unsigned gno = e->gno;
  semispace_t *tospace = e->tospace;
  word *dest = e->dest;
  word *copylim = e->lim;
  gc_t *gc = e->gc;
  los_t *los = e->los;
  word *los_p = 0, *los_next;
  int morework;

  do {
    morework = 0;

    while (scanptr != dest) {
      while (scanptr != dest && scanptr < scanlim) {
	scan_core( scanptr, dest, iflush,
		   forw_oflo( scanptr, gno, dest, copylim, tospace, los ));
      }

      if (scanptr != dest) {
	scan_chunk_idx++;
	scanptr = tospace->chunks[scan_chunk_idx].bot;
	scanlim = tospace->chunks[scan_chunk_idx].lim;
      }
    }

    los_next = los_walk_list( los->marked, los_p );
    if (los_next != 0) {
      morework = 1;
      do {
	word *p = los_next;
	assert( ishdr( *p ) );
	scan_core( p, dest, iflush,
		   forw_oflo( p, gno, dest, copylim, tospace, los ));
	los_p = los_next;
	los_next = los_walk_list( los->marked, los_p );
      } while (los_next != 0);
    }
  } while (morework);

  e->dest = dest;
  e->lim = copylim;
}

static void 
scan_oflo_np_promote( word *scanptr, word *scanlim, unsigned scan_chunk_idx,
		      oflo_env_t *e, int iflush )
{
  unsigned gno = e->gno;
  semispace_t *tospace = e->tospace;
  word *dest = e->dest;
  word *copylim = e->lim;
  word **ssbtop = e->ssbtop;
  word **ssblim = e->ssblim;
  gc_t *gc = e->gc;
  word *los_p = 0, *los_next;
  unsigned T_obj_gen = gclib_desc_g[pageof(scanptr)];
  int morework;

  do {
    morework = 0;

    while (scanptr != dest) {
      while (scanptr != dest && scanptr < scanlim) {
	/* T_bit is just a name that is common to scan_core_partial() and
	 * forw_oflo_partial(); scan_core_partial() uses it to declare some
	 * variable.
	 */
	scan_core_partial( scanptr, dest, iflush,
			  forw_oflo_partial( scanptr, gno, dest, copylim, 
					    tospace, T_obj_gen, T_bit, e->los),
			  T_bit );
      }

      if (scanptr != dest) {
	scan_chunk_idx++;
	scanptr = tospace->chunks[scan_chunk_idx].bot;
	scanlim = tospace->chunks[scan_chunk_idx].lim;
      }
    }

    los_next = los_walk_list( e->los->marked, los_p );
    if (los_next != 0) {
      morework = 1;
      do {
	word *p = los_next;
	assert( ishdr( *p ) );
	scan_core_partial( p, dest, iflush,
			  forw_oflo_partial( p, gno, dest, copylim, tospace, 
					     T_obj_gen, T_bit, e->los ),
			  T_bit );
	los_p = los_next;
	los_next = los_walk_list( e->los->marked, los_p );
      } while (los_next != 0);
    }
  } while (morework);

  e->dest = dest;
  e->lim = copylim;
}

static void 
scan_oflo_splitting( word *scanptr, word *scanlim, unsigned scan_chunk_idx,
		     oflo_env_t *e, int iflush )
{
  unsigned gno = e->gno;
  semispace_t *tospace = e->tospace;
  semispace_t *tospace2 = e->tospace2;
  word *dest = e->dest;
  word *dest2 = e->dest2;
  word *copylim = e->lim;
  word *copylim2 = e->lim2;
  gc_t *gc = e->gc;

  while (scanptr != dest) {
    while (scanptr != dest && scanptr < scanlim) {
      scan_core( scanptr, dest, iflush,
		 forw_oflo2( scanptr, gno,
			     dest, dest2,
			     copylim, copylim2,
			     tospace, tospace2, 0 ));
    }

    if (scanptr != dest) {
      scan_chunk_idx++;
      scanptr = tospace->chunks[scan_chunk_idx].bot;
      scanlim = tospace->chunks[scan_chunk_idx].lim;
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
static word forward( word p, word **dest, los_t *los )
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

#define FORW_BY_LOOP

#ifdef FORW_BY_LOOP
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
    else if (words > GC_LARGE_OBJECT_LIMIT/4 && los) 
      return forward_large_object( los, ptr, tag );
    else {
      memcpy( p1, p2, words*4 );
      p1 += words;
    }
    *dest = p1;
  }
#endif

#ifdef FORW_BY_DUFF
  { unsigned bytes;

  bytes = roundup8( sizefield( hdr ) + 4 );
  if (bytes > GC_LARGE_OBJECT_LIMIT && los) 
    return forward_large_object( los, ptr, tag );

  /* One might be tempted to think that the following can be speeded up by
   * either using explicit indices (e.g. *(p1+5) = *(p2+5)), introducing
   * temporaries to allow the compiler more room to schedule, by prefetching
   * the destination to prevent cache stalls, and so on. On the Sparc, 
   * however, this code is still faster.
   */
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

#ifdef FORW_BY_MEMCPY
  { unsigned bytes;
  /* This appears to be slowest */
  bytes = roundup8( sizefield( hdr ) + 4 );
  if (bytes > GC_LARGE_OBJECT_LIMIT && los)
    return forward_large_object( los, ptr, tag );

  memcpy( p1, p2, bytes );
  *dest = p1 + (bytes >> 2);
  }
#endif

  newptr = (word) tagptr( newptr, tag );

  /* leave forwarding pointer */
  *ptr = FORWARD_PTR;
  *(ptr+1) = newptr;

  return newptr;
}


/* Expand the semispace by switching to a new chunk.  To make things
 * simple for the scanning code, a string header is put into the
 * heap at the current location (unless we're at the end of the chunk)
 * with a length field that covers the rest of the chunk.  The scanning
 * code will simply skip the data.
 *
 * A string header is used rather than a generic bytevector header since
 * the latter value would cause the scanner to flush the icache for the
 * garbage area.
 *
 * It would be nice to supply a better estimate to the expansion function,
 * especially to ask for a larger block if much data remains to be copied.
 */
static void 
expand_semispace( semispace_t *ss, word **lim, word **dest, unsigned bytes )
{
  if (*dest < *lim) {
    word len = (*lim - *dest)*sizeof(word);
    **dest = mkheader(len-sizeof(word),STR_HDR);
  }

  ss->chunks[ss->current].top = *dest;
  ss_expand( ss, max( bytes, OLDSPACE_EXPAND_BYTES ) );
  *lim = ss->chunks[ss->current].lim;
  *dest = ss->chunks[ss->current].top;
}

static word forward_large_object( los_t *los, word *ptr, int tag )
{
  if (gclib_desc_b[pageof(ptr)] & MB_LARGE_OBJECT) {
    los_mark( los, ptr, gclib_desc_g[pageof(ptr)] );
    return tagptr( ptr, tag );
  }
  else {
    /* The large object was not allocated specially, so we must move it. */
    word *new;
    word hdr;
    int bytes;

    hdr = *ptr;
    bytes = roundup8( sizefield( hdr ) + 4 );
    new = los_allocate( los, bytes, gclib_desc_g[pageof(ptr)] );
    memcpy( new, ptr, bytes );
    
    /* Must mark it also! */
    los_mark( los, new, gclib_desc_g[pageof(ptr)] );
    
    /* Leave a forwarding pointer */
    *ptr = FORWARD_PTR;
    *(ptr+1) = tagptr( new, tag );
    return *(ptr+1);
  }
}

static void sweep_large_objects( gc_t *gc, int sweep_oldest, int dest )
{
  int i;

  for ( i=0 ; i <= sweep_oldest ; i++ )
    los_sweep( gc->los, i );
  los_append_and_clear_list( gc->los, gc->los->marked, dest );
}

/* eof */
