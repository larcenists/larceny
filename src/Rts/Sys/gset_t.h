/* Copyright 2007 Felix S Klock II
 *
 * $Id: gc_t.h 5226 2007-12-17 14:51:13Z pnkfelix $
 *
 * Larceny run-time system -- sets of generations 
 * (an inadequate representation, in the technical sense of the term)
 */

#ifndef INCLUDED_GSET_T_H
#define INCLUDED_GSET_T_H

typedef enum { gs_nil, gs_singleton, gs_range, gs_twrng } gset_tag_t;
typedef struct { gset_tag_t tag; int g1; int g2; int g3; int g4; } gset_t;
/* interpretation of gset_t:
 * [[ <gs_nil, g1, g2, g3, g4> ]]       = [    ]
 * [[ <gs_singleton, g1, g2, g3, g4> ]] = [ g1 ]
 * [[ <gs_range, g1, g2, g3, g4> ]]     = [ i | g1 <= i < g2 ]
 * [[ <gs_twrng, g1, g2, g3, g4> ]]     = 
 *                [ i | g1 <= i < g2 ] ++ [ j | g3 <= j < g4 ]
 *
 * (the two ranges of gs_twrng must not overlap; should be assert2'ing this)
 */
static gset_t gset_null() { 
  gset_t g; g.tag = gs_nil; return g; 
}
static gset_t gset_singleton( int g1 ) { 
  gset_t g; g.tag = gs_singleton; g.g1 = g1; return g; 
}
static gset_t gset_range( int g1, int g2 ) {
  gset_t g; g.tag = gs_range; g.g1 = g1; g.g2 = g2; return g; 
}
static gset_t gset_younger_than( int g2 ) { return gset_range( 0, g2 ); }
static bool gset_singleton_memberp( int gno, gset_t gs ) {
  assert2( gs.tag == gs_singleton ); return ( gs.g1 == gno ); 
}
static bool gset_range_memberp( int gno, gset_t gs ) {
  assert2( gs.tag == gs_range ); return ( gs.g1 <= gno && gno < gs.g2 ); 
}
static bool gset_twrng_memberp( int gno, gset_t gs ) {
  assert2( gs.tag == gs_twrng ); 
  return (( gs.g1 <= gno && gno < gs.g2 )
          || ( gs.g3 <= gno && gno < gs.g4 ));
}
static bool gset_memberp( int gno, gset_t gs ) {
  switch (gs.tag) {
  case gs_nil: return FALSE;
  case gs_singleton: return gset_singleton_memberp( gno, gs );
  case gs_range:     return gset_range_memberp( gno, gs );
  case gs_twrng:     return gset_twrng_memberp( gno, gs );
  }
  assert2(0);
}
static bool gset_emptyp( gset_t gs ) {
  switch (gs.tag) {
  case gs_nil: return TRUE;
  case gs_singleton: return FALSE;
  case gs_range: return (gs.g1 >= gs.g2);
  case gs_twrng: return ((gs.g1 > gs.g2) && (gs.g3 > gs.g4));
  }
}
static int gset_count( gset_t gs ) {
  switch (gs.tag) {
  case gs_nil: return 0;
  case gs_singleton: return 1;
  case gs_range: return max(0,(gs.g2 - gs.g1));
  case gs_twrng: return (max(0,(gs.g2 - gs.g1)) + max(0,(gs.g4 - gs.g3)));
  }
}
static bool gset_max_elem( gset_t gs ) {
  switch (gs.tag) {
  case gs_singleton: return gs.g1;
  case gs_range:     return gs.g2-1;
  case gs_twrng:     return max( gs.g2-1, gs.g4-1 );
  }
  assert2(0);
}
static bool gset_last_elem( gset_t gs ) {
  switch (gs.tag) {
  case gs_singleton: return gs.g1;
  case gs_range:     return gs.g2-1;
  case gs_twrng:     return gs.g4-1;
  }
  assert2(0);
}
static bool gset_first_elem( gset_t gs ) {
  switch (gs.tag) {
  case gs_singleton: return gs.g1;
  case gs_range:     return gs.g1;
  case gs_twrng:     return gs.g1;
  }
  assert2(0);
}
static int gset_singleton_min_elem_greater_than( gset_t gs, int gno ) {
  if (gs.g1 > gno) return gs.g1; else return 0;
}
static int gset_range_min_elem_greater_than( gset_t gs, int gno ) {
  if (gno+1 <= gs.g1) return gs.g1;
  else if (gno+1 < gs.g2) return gno+1;
  else return 0;
}
static int gset_twrng_min_elem_greater_than( gset_t gs, int gno ) {
  if (gno+1 <= min(gs.g1, gs.g3)) return min(gs.g1, gs.g3);
  else if (gs.g1 <= gno+1 && gno+1 < gs.g2) return gno+1;
  else if (gs.g3 <= gno+1 && gno+1 < gs.g4) return gno+1;
  else return 0;
}
static int gset_min_elem_greater_than( gset_t gs, int gno ) {
  /* returns 0 if no elem of gs is larger than gno */
  switch (gs.tag) {
  case gs_nil: return 0;
  case gs_singleton: return (gs.g1 > gno) ? gs.g1 : 0;
  case gs_range: return gset_range_min_elem_greater_than( gs, gno );
  case gs_twrng: return gset_twrng_min_elem_greater_than( gs, gno );
  }
}
static gset_t gset_remove( int gno, gset_t gs ) {
  gset_t rtn;
  switch (gs.tag) {
  case gs_nil: assert(0);
  case gs_singleton: rtn.tag = gs_nil; break;
  case gs_range: assert(gno == gs.g1); 
    if (gs.g1+1 == gs.g2) {
      rtn.tag = gs_nil;
    } else { assert2( gs.g1+1 < gs.g2 );
      rtn.tag = gs_range; rtn.g1 = gs.g1+1; rtn.g2 = gs.g2;
    }
    break;
  case gs_twrng: assert( gno == gs.g1 || gno == gs.g3 );
    if (gno == gs.g1) {
      if (gs.g1+1 == gs.g2) {
        return gset_range( gs.g3, gs.g4 );
      } else { assert2( gs.g1+1 < gs.g2 );
        rtn.tag = gs_twrng;
        rtn.g1 = gs.g1+1;
        rtn.g2 = gs.g2;
        rtn.g3 = gs.g3;
        rtn.g4 = gs.g4;
      }
    } else { assert2( gs.g1 < gs.g2 );
      if (gs.g3+1 == gs.g4) {
        return gset_range( gs.g1, gs.g2 );
      } else { assert2( gs.g3+1 < gs.g4 );
        rtn.tag = gs_twrng;
        rtn.g1 = gs.g1;
        rtn.g2 = gs.g2;
        rtn.g3 = gs.g3+1;
        rtn.g4 = gs.g4;
      }
    }
    break;
  }
  return rtn;
}

static gset_t gset_union( gset_t gs1, gset_t gs2 ) {
  gset_t rtn;
  if (gset_emptyp( gs1 )) return gs2;
  if (gset_emptyp( gs2 )) return gs1;
  assert2( gs1.tag == gs_range );
  assert2( gs2.tag == gs_range );
  rtn.tag = gs_twrng;
  rtn.g1 = gs1.g1;
  rtn.g2 = gs1.g2;
  rtn.g3 = gs2.g1;
  rtn.g4 = gs2.g2;
  return rtn;
}
static bool gset_disjointp( gset_t gs1, gset_t gs2 ) {
  switch (gs1.tag) {
  case gs_nil: return TRUE;
  case gs_singleton: return ! gset_memberp( gs1.g1, gs2 );
  case gs_range: 
    return ! gset_memberp( gset_min_elem_greater_than( gs2, gs1.g1-1 ), gs1 );
  case gs_twrng: /* fall through */
    break;
  }
  switch (gs2.tag) {
  case gs_nil: return TRUE;
  case gs_singleton: return ! gset_memberp( gs2.g1, gs1 );
  case gs_range: 
    return ! gset_memberp( gset_min_elem_greater_than( gs1, gs2.g1-1 ), gs2 );
  case gs_twrng: /* fall through */
    break;
  }
  /* if here, both are gs_twrng, for which union is unsupported */
  assert( FALSE ); /* figure this out later. */
  return FALSE;
}

static char* gset_tagname( gset_t gs ) {
  switch (gs.tag) {
  case gs_nil:       return "gs_empty";
  case gs_singleton: return "gs_singl";
  case gs_range:     return "gs_range";
  case gs_twrng:     return "gs_twrng";
  }
  assert2(0);
}

#endif    /* INCLUDED_GSET_T_H */

/* eof */
