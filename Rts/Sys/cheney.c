/* Rts/Sys/cheney.c
 * Larceny run-time system -- copying garbage collector library.
 *
 * $Id: cheney.c,v 1.2 1997/02/03 18:11:14 lth Exp $
 *
 * This file contains generic low-level procedures for standard copying
 * garbage collection.  There are two public procedures:
 *
 *  gclib_stopcopy_fast() performs a simple trace and copy from one
 *  heap area to the other and is suitable for stop-and-copy collection
 *  in the youngest generation.  It is written to be fast rather than
 *  general; in particular, the tospace must be large enough to 
 *  hold all the objects.
 *
 *  gclib_copy_younger_into() copies all reachable objects from generations
 *  younger than the destination into the destination generation's tospace.
 *  The tospace is extended as necessary to accomodate the objects.  (By
 *  manipulating the generation bits in the memory descriptor tables this
 *  procedure can be used to perform copying collection within a generation.)
 *
 * These procedures are mostly isolated from the rest of the run-time
 * system.
 *
 * The code in this file is reentrant.
 */

#define GC_INTERNAL

#include "larceny.h"
#include "macros.h"
#include "cdefs.h"
#include "gclib.h"
#include "assert.h"

#include <memory.h>                /* For memcpy() */


#define FORWARD_PTR 0xFFFEFFFE     /* Should be defined somewhere else */


/* The forw() macro forwards a pointer without checking for overflow,
 * and using a range test to check that the pointer points into a
 * particular heap area.  It is supposed to be fast.
 *
 * forw() assumes that all parameters are variable names (lvalues whose
 * evaluation has no side effect).
 *
 * static void forw( word *p, word *oldlo, word *oldhi, word *dest ) 
 */

#define forw( p, oldlo, oldhi, dest ) \
  do { word TMP2 = *p; \
       if (isptr( TMP2 ) && (word*)TMP2 >= oldlo && (word*)TMP2 < oldhi) { \
          forw_core( TMP2, p, dest, nocheck_space, VOID, VOID ); \
       } \
  } while( 0 )

/* forw_record() is like forw() except that 'bit' is set to 1 if
 * the word being forwarded is a pointer into a younger generation than
 * the generation of the object it resides in; the latter generation is
 * passed in in the variable obj_gen.
 */
#define forw_record( p, oldlo, oldhi, dest, bit, obj_gen ) \
  do { word TMP2 = *p; \
       if (isptr( TMP2 )) { \
         unsigned T_genw = gclib_desc_g[pageof(TMP2)]; \
         if ((word*)TMP2 >= oldlo && (word*)TMP2 < oldhi) { \
           forw_core( TMP2, p, dest, nocheck_space, VOID, VOID ); \
         } \
         if (T_genw < obj_gen) bit = 1; \
       } \
  } while( 0 )


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
 *                        semispace_t *semispace )
 */

#define forw_oflo( p, gno, dest, lim, semispace ) \
  do { word TMP2 = *p; \
       if (isptr( TMP2 ) && gclib_desc_g[pageof(TMP2)] < gno) { \
          forw_core( TMP2, p, dest, check_space, lim, semispace ); \
       } \
  } while( 0 )

/* forw_oflo_record() is like forw_oflo() except that 'bit' is set to 1 if
 * the word being forwarded is a pointer into a younger generation than
 * the generation of the object it resides in; the latter generation is
 * passed in in the variable obj_gen.
 */
#define forw_oflo_record( p, gno, dest, lim, semispace, bit, obj_gen ) \
  do { word TMP2 = *p; \
       if (isptr( TMP2 )) { \
          unsigned T_genw = gclib_desc_g[pageof(TMP2)]; \
          if (T_genw < gno) { \
            forw_core( TMP2, p, dest, check_space, lim, semispace ); \
          } \
	  if (T_genw < obj_gen) bit=1; \
       } \
  } while( 0 )


/* The forw_core() macro implements most of the forwarding operation.
 * In the case of forw(), the checking code expands to a no-op; in the
 * case of forw_oflo, a check will take place.
 */

#define forw_core( TMP2, p, dest, check, lim, semispace ) \
  word *TMP_P = ptrof( TMP2 ); \
  if (*TMP_P == FORWARD_PTR) \
    *p = *(TMP_P+1); \
  else if (tagof( TMP2 ) == PAIR_TAG) { \
    check(dest,lim,8,semispace); \
    *dest = *TMP_P; \
    *(dest+1) = *(TMP_P+1); \
    *TMP_P = FORWARD_PTR; \
    *(TMP_P+1) = *p = (word)tagptr(dest, PAIR_TAG); \
    dest += 2; \
  } \
  else { \
    word *TMPD; \
    check(dest,lim,sizefield(*TMP_P),semispace); \
    TMPD = dest; \
    *p = forward( TMP2, &TMPD ); dest = TMPD; \
  }


/* The check_space() macro expands into code that checks whether the
 * semispace has room for 'wanted' bytes, and if not, it expands the
 * semispace and updates the 'dest' and 'lim' variables.  Local temporaries
 * are passed by reference to expand_semispace rather than the addresses
 * of 'dest' and 'lim' to prevent the compiler from getting confused about
 * possible aliasing of the latter variables.
 */

#define check_space( dest, lim, wanted, semispace ) \
  if ((char*)lim-(char*)dest < (wanted)) { \
    word *CS_LIM=lim, *CS_DEST=dest; \
    expand_semispace( semispace, &CS_LIM, &CS_DEST, (wanted) ); \
    dest = CS_DEST; lim = CS_LIM; \
  }

/* The nocheck_space() macro expands into a no-op. */

#define nocheck_space( dest, lim, wanted, semispace ) \
  ((void)0)


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
	if (iflush && typetag( T_h ) == BVEC_SUBTAG) \
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
};

typedef struct oflo_env oflo_env_t;
struct oflo_env {
  unsigned gno;
  word *dest;
  word *lim;
  semispace_t *tospace;
};


/* Private procedures */

static void oldspace_copy( gc_t *gc, semispace_t *to, int youngest_remset );
static void root_scanner( word *addr, void *data );
static void root_scanner_oflo( word *addr, void *data );
static int  remset_scanner( word obj, void *data, unsigned *count );
static int  remset_scanner_oflo( word obj, void *data, unsigned *count );
static void scan( word *start, fast_env_t *e, int iflush );
static void scan_oflo(word *start, unsigned starti, oflo_env_t *e, int iflush);
static word forward( word, word ** );
static void expand_semispace( semispace_t *, word **, word **, unsigned );


/* 
 * Perform a stop-and-copy collection of generation-0 objects from the
 * given fromspace to a contiguous area starting at newlo; the tospace is
 * known to be large enough.
 */
word
*gclib_stopcopy_fast( gc_t *gc, word *oldlo, word *oldhi, word *newlo )
{
  fast_env_t e;

  e.oldlo = oldlo;
  e.oldhi = oldhi;
  e.dest = newlo;

  gc->enumerate_roots( gc, root_scanner, (void*)&e );
  gc->enumerate_remsets_older_than( gc, 0, remset_scanner, (void*)&e );
  scan( newlo, &e, gc->iflush(gc,0) );

  return e.dest;
}


/*
 * Copy objects younger than the given semispace into the semispace,
 * growing it if necessary
 */
void
gclib_copy_younger_into( gc_t *gc, semispace_t *tospace )
{
  oldspace_copy( gc, tospace, tospace->gen_no );
}


/*
 * Copy all objects from fromspace and from any generation younger than
 * fromspace into tospace (growing twospace if necessary).
 *
 * Fromspace is not currently used; it is an artifact of an earlier
 * implementation.  FIXME.
 */
void
gclib_stopcopy_slow( gc_t *gc, semispace_t *fromspace, semispace_t *tospace )
{
  assert( fromspace->gen_no == tospace->gen_no );

  oldspace_copy( gc, tospace, tospace->gen_no+1 );
}


/* Here, effective_generation is a generation number s.t. all objects from
 * younger generations are copied into tospace.  When promoting, the
 * effective generation is the generation number of tospace, and when 
 * copying in an older generation, it is that generation's generation number,
 * plus 1.
 */
static void 
oldspace_copy( gc_t *gc, semispace_t *tospace, int effective_generation )
{
  word *scanptr;
  unsigned scan_chunk_idx;
  oflo_env_t e;

  scan_chunk_idx = tospace->current;
  scanptr = tospace->chunks[scan_chunk_idx].top;
  e.gno = effective_generation;
  e.dest = scanptr;
  e.lim = tospace->chunks[scan_chunk_idx].lim;
  e.tospace = tospace;

  gc->enumerate_roots( gc, root_scanner_oflo, (void*)&e );
  gc->enumerate_remsets_older_than( gc, 
				    effective_generation-1,
				    remset_scanner_oflo, 
				    (void*)&e );
  scan_oflo( scanptr, scan_chunk_idx, &e, gc->iflush(gc,e.gno) );

  tospace->chunks[tospace->current].top = e.dest;
}


static void
root_scanner( word *ptr, void *data )
{
  fast_env_t *e = (fast_env_t*)data;
  forw( ptr, e->oldlo, e->oldhi, e->dest );
}


static void
root_scanner_oflo( word *ptr, void *data )
{
  oflo_env_t *e = (oflo_env_t*)data;
  forw_oflo( ptr, e->gno, e->dest, e->lim, e->tospace );
}


static int
remset_scanner( word ptr, void *data, unsigned *count )
{
  fast_env_t *e = (fast_env_t*)data;
  word *oldlo = e->oldlo, *oldhi = e->oldhi, *dest = e->dest;
  word *p;
  int bit=0;
  unsigned obj_gen = gclib_desc_g[pageof(ptr)];

  remset_scanner_core( ptr, p,
		       forw_record( p, oldlo, oldhi, dest, bit, obj_gen ), 
		       *count );

  e->dest = dest;
  return bit;
}


static int
remset_scanner_oflo( word ptr, void *data, unsigned *count )
{
  oflo_env_t *e = (oflo_env_t*)data;
  word *dest = e->dest, *lim = e->lim;
  unsigned gno = e->gno;
  semispace_t *s = e->tospace;
  word *p;
  int bit=0;
  unsigned obj_gen = gclib_desc_g[pageof(ptr)];

  remset_scanner_core( ptr, p, 
		       forw_oflo_record( p, gno, dest, lim, s, bit, obj_gen ),
		       *count );

  e->dest = dest;
  e->lim = lim;
  return bit;
}


static void 
scan( word *scanptr, fast_env_t *e, int iflush )
{
  word *oldlo = e->oldlo;
  word *oldhi = e->oldhi;
  word *dest = e->dest;

  while (scanptr < dest) {
    scan_core( scanptr, dest, iflush, forw( scanptr, oldlo, oldhi, dest ) );
  }

  e->dest = dest;
}


static void 
scan_oflo( word *scanptr, unsigned scan_chunk_idx, oflo_env_t *e, int iflush )
{
  word *dest = e->dest;
  word *copylim = e->lim;
  word *scanlim = e->lim;
  unsigned gno = e->gno;
  semispace_t *tospace = e->tospace;

  while (scanptr != dest) {
    /* FIXME: It would be nice to have only one condition in this loop */
    while (scanptr != dest && scanptr < scanlim) {
      scan_core( scanptr, dest, iflush,
		 forw_oflo( scanptr, gno, dest, copylim, tospace ));
    }
    if (scanptr != dest) {
      scan_chunk_idx++;
      scanptr = tospace->chunks[scan_chunk_idx].bot;
      scanlim = tospace->chunks[scan_chunk_idx].lim;
    }
  }

  e->dest = dest;
  e->lim = copylim;
}


/*
 * "p" is a tagged pointer into oldspace;
 * "*dest" is a pointer into newspace, the destination of the next object.
 *
 * Forward() returns the forwarding value of "ptr"; it does this by
 * copying the object and returning the new address.
 */
static word forward( word p, word **dest )
{
  word hdr, newptr, *p1, *p2, tag, *ptr;
  unsigned bytes;

  tag = tagof( p ); 
  ptr = ptrof( p );

  /* Copy the structure into newspace and pad if necessary. */
  p1 = *dest;
  newptr = (word)p1;    /* really *dest, but the compiler is dumb. */
  p2 = ptr;

  hdr = *ptr;
  bytes = roundup8( sizefield( hdr ) + 4 );

  assert( ishdr( hdr ) );

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


/* eof */
