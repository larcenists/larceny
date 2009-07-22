/* Copyright 2009 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Representation for bitmap of untagged heap addresses.
 * 
 * This module represents a set of heap addresses via a collection of
 * bitmap fragments; the collection is maintain in a tree structure so
 * that the representation can be sparse (large empty areas of the
 * heap do not have bitmap fragments assigned to them) while still
 * allowing membership to be relatively quick.
 *
 * Each leaf at the deepest level of the tree is a bitmap covering a
 * contiguous range of address R; a node above the deepest level
 * represent the union of the addresses represented by its children.
 * (If no descendant bitmap covers an address, then the address is not
 *  in the set; this is what makes the representation sparse.)
 * 
 */

#include "larceny.h"
#include "gclib.h"
#include "extbmp_t.h"

#if defined(BITS_32)
# define BIT_IDX_SHIFT       3  /* shift to get doubleword bit address */
# define BIT_IDX_TO_WORDADDR 5  /* shift to get word addr from bit addr */
# define BIT_IN_WORD_MASK   31  /* mask to get bit shift */
# define BITS_IN_WORD       32
/* This upper bounds distinct entries in bitmap. */
# define SHIFTED_ADDRESS_SPACE 536870912 /* 2^32 >> 3 */
#else
# error "Must define EXTBMP macros for non-32 bit systems."
#endif

/* All objects are double word aligned */
#define MIN_BYTES_PER_OBJECT (2*sizeof(word))
#define ADDRS_PER_WORD (16*sizeof(word))
#define BITS_PER_WORD (8*sizeof(word))

/* Calculates ceil(x/y); (unlike quotient, which does floor). */
#define CEILDIV(x,y) (roundup((x),(y))/(y))

/*
 * The GC_CHUNK_SIZE is the minimum number of bytes in a fragment of a
 * semispace; a typical value is 256K bytes.  For now it seems that
 * each leaf bitmap is probably best off covering one chunk; thus a
 * leaf should be 
 *   (GC_CHUNK_SIZE / MIN_BYTES_PER_OBJECT) 
 *    = 32 K bits
 *    =  4 K bytes = 1 K words.
 *
 * (Hmm.  Maybe a smaller bitmap size is warranted, since it seems 
 *  like it would be expensive to inspect 1 K words if the leaf
 *  has a few entries.  Revisit later.)
 * ((And of course there's always the hybrid approach where I might
 *   stash a fixed size list of say 64 or 128 words and wait for that
 *   to fill up; but then I have to deal with membership testing...))
 */

/* 
 * Covering a 32-bit address space requires ( 2^32 / LEAF_BYTES )
 * (Assuming leaf covers exactly 1 chunk as described above, this is
 *  16K leaves.)
 * 
 * Let NL be the number of leaves.
 * 
 *  If we assume a uniform branching factor in the tree, we have:
 * 
 *                         NL <= branching ^ depth
 *                     ln(NL) <= ln( branching ^ depth )
 *                     ln(NL) <= depth * ln( branching )
 *  ln(NL) / ln( branching )  <=  depth
 * 
 * Thus:
 *   ceil((ln(NL) / ln(branching-factor))) = max-depth
 * iff
 *   (max-depth - 1) < ln(NL) / ln(branching-factor) <= max-depth 
 * iff
 *   (ln(NL) / max-depth) <= ln(branching-factor) < (ln(NL) / (max-depth - 1))
 * iff 
 *   e^(ln(NL) / max-depth) <= branching-factor < e^(ln(NL) / (max-depth - 1))
 *
 * Any branching-factor value in the half-open range above will yield
 * the desired max depth; then use its minimal integer value:
 *
 *   branching-factor = ceil( e^(ln(NL) / max-depth) )
 * 
 */
/* 
 * Assuming NL = 16 K, this means:
 * 
 *         max-depth = 2  ==>  branching-factor = 128
 *         max-depth = 3  ==>  branching-factor =  26
 *         max-depth = 4  ==>  branching-factor =  12
 *         max-depth = 5  ==>  branching-factor =   7
 *         max-depth = 6  ==>  branching-factor =   6
 *         max-depth = 7  ==>  branching-factor =   4
 *         max-depth = 9  ==>  branching-factor =   3
 *        max-depth = 14  ==>  branching-factor =   2
 *
 * Having fairly wide nodes does not seem like a problem to me at the
 * moment (at least not compared to the cost of having to traverse
 * many levels of the tree before reaching the desired leaf), so I'm
 * going to try the branching factor of 128.  I mostly did the
 * calculation above so that I know what steps to go through if I
 * discover that a leaf-size that covers GC_CHUNK_SIZE is a bad idea.
 *
 */

static const int DEFAULT_LEAF_BYTES = 4096;
static const int DEFAULT_ENTRIES_PER_INODE = 8; /* branching factor */

/* default leaves large; allocate individually */
static const int DEFAULT_LEAF_POOL_SIZE  = 1;  /*  1 element  = 4KB */
static const int DEFAULT_INODE_POOL_SIZE = 64; /* 64 elements = 8KB */

typedef struct freed freed_t;
typedef struct inode inode_t;
typedef struct leaf leaf_t;
typedef union tnode tnode_t;

/* Special mode that adds debugging fields to structs that carry
 * redundant data about the nodes of the tree so that code can
 * double-check its state as it traverses and manipulates the tree.
 * 
 * Note that this changes their sizes and thus the runtime allocation
 * behavior, and thus this switch cannot be toggled at will without
 * risking changes to runtime behavior.
 */
#define INCLUDE_REDUNDANT_FIELDS 1

#define dbmsg( format, args... ) if (0) consolemsg( format, ## args )

#if INCLUDE_REDUNDANT_FIELDS
enum tnode_tag { tag_ignoreme, tag_freed, tag_inode, tag_leaf };
struct metadata_always_first { enum tnode_tag tag; int length_in_bytes; };
#endif

#define DYNAMIC_SIZE 1 /* (the "1" is lying; size selected dynamically) */

struct freed {
#if INCLUDE_REDUNDANT_FIELDS
  struct metadata_always_first metadata;
#endif
  freed_t *next;
};
struct inode { 
#if INCLUDE_REDUNDANT_FIELDS
  struct metadata_always_first metadata;
  word start;
  word limit;
#endif
  int address_range;
  long long addresses_per_child;
  /* XXX FIXME move above into INCLUDE_REDUNDANT_FIELDS */

  tnode_t *nodes[DYNAMIC_SIZE];
};
struct leaf  {
#if INCLUDE_REDUNDANT_FIELDS
  struct metadata_always_first metadata;
  word start;
  word limit;
#endif
  word   bitmap[DYNAMIC_SIZE];
};

union tnode {
#if INCLUDE_REDUNDANT_FIELDS
  struct metadata_always_first metadata;
#endif
  freed_t freed; 
  inode_t inode; 
  leaf_t leaf; 
};

struct extbmp {
  gc_t *gc;
  int leaf_words;        /* number of words allocated in leaf bitmap */
  int entries_per_inode; /* branching factor */
  int depth;             /* depth=1 implies immediate children have bitmaps */
  tnode_t *tree;
};

static tnode_t *alloc_tnode( extbmp_t *ebmp, 
                             enum tnode_tag tag, 
                             int length_in_bytes )
{
  tnode_t *retval;

  retval = (tnode_t*)
    /* XXX FIXME use pooled allocation */
    gclib_alloc_rts( length_in_bytes, MB_REMSET );

#if INCLUDE_REDUNDANT_FIELDS
  retval->metadata.tag             = tag;
  retval->metadata.length_in_bytes = length_in_bytes;
#endif

  return retval;
}
static void free_tnode( extbmp_t *ebmp, tnode_t *t, 
                        enum tnode_tag tag, int length_in_bytes ) {
  gclib_free( t, length_in_bytes );
}

static void init_leaf_fields_cleared( extbmp_t *ebmp, leaf_t *l ) {
  int i;
  int bmp_words = ebmp->leaf_words;
  word *bmp = l->bitmap;
  for (i = 0; i < bmp_words; i++) {
    bmp[i] = 0;
  }
}
static tnode_t *alloc_leaf( extbmp_t *ebmp, word start, word limit ) 
{
  tnode_t *retval;
  retval = alloc_tnode( ebmp, tag_leaf, 
                        (sizeof( leaf_t )
                         + ebmp->leaf_words*sizeof(word)));
#if INCLUDE_REDUNDANT_FIELDS
  retval->leaf.start = start;
  retval->leaf.limit = limit;
#endif

  init_leaf_fields_cleared( ebmp, &retval->leaf );

  dbmsg(     "alloc_leaf(ebmp,start=0x%08x,limit=0x%08x)"
             "           ==> 0x%08x",
             start, limit, retval);
  return retval;
}
static void free_leaf( extbmp_t *ebmp, tnode_t *leaf )
{
  dbmsg("free_leaf( ebmp, leaf=0x%08x", leaf);
  free_tnode( ebmp, leaf, 
              tag_leaf, 
              (sizeof( leaf_t )
               + ebmp->leaf_words*sizeof(word)));
}

static void init_inode_fields_cleared( extbmp_t *ebmp, inode_t *n )
{
  int i;
  int entries = ebmp->entries_per_inode;
  tnode_t **nodes = n->nodes;
  for (i = 0; i < entries; i++) {
    nodes[i] = NULL;
  }
}

static tnode_t *alloc_inode( extbmp_t *ebmp, 
                             long long address_range, 
                             word start, 
                             long long limit )
{
  tnode_t *retval;

  assert( address_range > 0 );

  retval = alloc_tnode( ebmp, tag_inode, 
                        (sizeof( inode_t ) 
                         + ebmp->entries_per_inode*sizeof( tnode_t* )));
  retval->inode.address_range = address_range;
  retval->inode.addresses_per_child = 
    CEILDIV( ((long long)address_range), ebmp->entries_per_inode );

  assert( retval->inode.addresses_per_child > 0 );

#if INCLUDE_REDUNDANT_FIELDS
  retval->inode.start = start;
  retval->inode.limit = limit;
#endif

  init_inode_fields_cleared( ebmp, &retval->inode );

  dbmsg(     "alloc_inode(ebmp,%8d,start=0x%08x,limit=0x%08x)"
             " ==> 0x%08x", 
             address_range, start, limit, retval);
  return retval;
}
static void free_inode( extbmp_t *ebmp, tnode_t *inode )
{
  dbmsg("free_inode( ebmp, inode=0x%08x", inode);
  free_tnode( ebmp, inode, 
              tag_inode, 
              (sizeof( inode_t ) 
               + ebmp->entries_per_inode*sizeof( tnode_t* )));
}

extbmp_t *create_extensible_bitmap_params( gc_t *gc, gc_param_t *info,
                                           int leaf_bytes, 
                                           int entries_per_node )
{
  /* Creates bitmap representing empty set of addresses */
  extbmp_t *ebmp;
  int depth;
  int max_leaves;
  int leaf_words = CEILDIV(leaf_bytes, sizeof(word));

  assert( (leaf_bytes % MIN_BYTES_PER_OBJECT) == 0 );
  max_leaves = SHIFTED_ADDRESS_SPACE / (BITS_PER_WORD * leaf_words);
  assert( max_leaves > 0 );
  { /* calculate max depth of tree */
    int i = 1;
    long long tot = entries_per_node;
    while (tot < max_leaves) {
      i   += 1;
      tot *= entries_per_node;
    }
    depth = i;
  }

  ebmp = (extbmp_t*)must_malloc( sizeof( extbmp_t ));
  ebmp->gc                = gc;
  ebmp->leaf_words        = CEILDIV(leaf_bytes,sizeof(word));
  ebmp->entries_per_inode = entries_per_node;
  ebmp->depth             = depth;
  ebmp->tree              = 
    alloc_inode( ebmp, 
                 (((long long)SHIFTED_ADDRESS_SPACE) << BIT_IDX_SHIFT),
                 0, 
                 (((long long)SHIFTED_ADDRESS_SPACE) << BIT_IDX_SHIFT));

  consolemsg( "ebmp{gc,leaf_words=%d,entries_per_inode=%d,depth=%d,tree} max_leaves:%d",
              ebmp->leaf_words, ebmp->entries_per_inode, ebmp->depth, max_leaves );
  return ebmp;
}

extbmp_t *create_extensible_bitmap( gc_t *gc, gc_param_t *info )
{
  return create_extensible_bitmap_params( gc, info,
                                          DEFAULT_LEAF_BYTES, 
                                          DEFAULT_ENTRIES_PER_INODE );
}

static bool find_leaf_calc_offset_recur( extbmp_t *ebmp, word untagged_w,
                                         tnode_t *tree, int depth, 
                                         word start_addr_of_node_coverage, 
                                         leaf_t **leaf_recv, 
                                         word *first_addr_for_leaf,
                                         bool alloc_if_unfound )
{
  bool retval;

  dbmsg(     "find_leaf_calc_offset_recur"
             "( ebmp, 0x%08x, "
             "tree, depth=%d, "
             "start_addr=0x%08x, "
             "leaf_recv, first_addr_for_leaf, "
             "alloc_if_unfound=%s )", 
             untagged_w, depth, 
             start_addr_of_node_coverage, 
             (alloc_if_unfound?"TRUE":"FALSE") );

  assert2( tree != NULL );

  if (depth == 0) {
    assert2( tree->metadata.tag == tag_leaf );

    *leaf_recv           = &tree->leaf;
    *first_addr_for_leaf = start_addr_of_node_coverage;

    return TRUE;
  } else {
    inode_t *inode;
    int idx;
    long long new_start_addr;
    long long new_addr_limit;

    assert2( tree->metadata.tag == tag_inode );

    inode = &tree->inode;
    idx   = 
      ( (untagged_w - start_addr_of_node_coverage)
        / inode->addresses_per_child );

    assert2( idx >= 0 );
    assert2( idx < ebmp->entries_per_inode );

    new_start_addr = 
      (start_addr_of_node_coverage 
       + idx * inode->addresses_per_child );
    new_addr_limit = 
      (new_start_addr 
       + inode->addresses_per_child );
    assert2( new_start_addr <= untagged_w );
    assert2( untagged_w  < new_addr_limit );

    if (inode->nodes[ idx ] == NULL) {
      if (alloc_if_unfound) {
        if (depth == 1) {
          inode->nodes[ idx ] =  alloc_leaf( ebmp, new_start_addr, new_addr_limit );
          assert2( inode->nodes[idx]->metadata.tag == tag_leaf );
        } else {
          inode->nodes[ idx ] = alloc_inode( ebmp, 
                                             inode->addresses_per_child,
                                             new_start_addr, new_addr_limit );
          assert2( inode->nodes[idx]->metadata.tag == tag_inode );
        }
      } else {
        return FALSE;
      }
    }
    assert2( inode->nodes[ idx ] != NULL );
    assert2( ((depth > 1) && (inode->nodes[ idx ]->metadata.tag == tag_inode)) ||
             ((depth == 1) && (inode->nodes[ idx ]->metadata.tag == tag_leaf)) );
    retval = 
      find_leaf_calc_offset_recur( ebmp, 
                                   untagged_w,
                                   inode->nodes[ idx ],
                                   depth-1,
                                   new_start_addr, 
                                   leaf_recv,
                                   first_addr_for_leaf,
                                   alloc_if_unfound );

    assert2( tree->metadata.tag == tag_inode );
    return retval;
  }
}

/* If returns TRUE, then 
 *   The tree has a leaf L that covers untagged_w
 *   *leaf_recv           := L
 *   *first_addr_for_leaf := start of L's bitmap
 * Otherwise the tree has no such leaf 
 *   (untagged_w is not a member of the set).
 * 
 * (Parameter alloc_if_unfound implies that this must return TRUE.)
 */
static bool find_leaf_calc_offset( extbmp_t *ebmp, word untagged_w,
                                   leaf_t **leaf_recv, 
                                   word *first_addr_for_leaf_recv,
                                   bool alloc_if_unfound )
{
  tnode_t *tree;
  int depth;
  word start_addr_of_node_coverage;

  dbmsg(     "find_leaf_calc_offset"
             "( ebmp, 0x%08x, "
             "leaf_recv, first_addr_for_leaf, "
             "alloc_if_unfound=%s )", 
             untagged_w, (alloc_if_unfound?"TRUE":"FALSE") );

  tree = ebmp->tree;
  depth = ebmp->depth;
  start_addr_of_node_coverage = 0;

  return 
    find_leaf_calc_offset_recur( ebmp, untagged_w, 
                                 tree, depth, start_addr_of_node_coverage,
                                 leaf_recv, first_addr_for_leaf_recv, 
                                 alloc_if_unfound );
}

static void find_or_alloc_leaf( extbmp_t *ebmp, word untagged_w,
                                leaf_t **leaf_recv, 
                                word *first_addr_for_leaf )
{
  bool retval_ignored = 
    find_leaf_calc_offset( ebmp, untagged_w, leaf_recv, first_addr_for_leaf, 
                           TRUE );
  assert2(retval_ignored);
}

static bool find_leaf_or_fail(  extbmp_t *ebmp, word untagged_w,
                                leaf_t **leaf_recv, 
                                word *first_addr_for_leaf )
{
  bool retval = 
    find_leaf_calc_offset( ebmp, untagged_w, leaf_recv, first_addr_for_leaf, 
                           FALSE );
  return retval;
}

static word leaf_wordaddr_lim( extbmp_t *ebmp, word first )
{
  return (word)((word*)first + ADDRS_PER_WORD*ebmp->leaf_words);
}

void extbmp_add_elem( extbmp_t *ebmp, word untagged_w )
{ /* ebmp := ebmp U { untagged_w } */
  leaf_t *leaf;
  word first, lim;
  unsigned int bit_idx, word_idx, bit_in_word;

  assert( tagof(untagged_w) == 0 );

  first = 0xFFFFFFF;
  lim   = 0x0;

  find_or_alloc_leaf( ebmp, untagged_w, &leaf, &first );
  lim = leaf_wordaddr_lim( ebmp, first );

  assert( first <= untagged_w  );
  assert( untagged_w < lim );

  bit_idx     = (untagged_w - first) >> BIT_IDX_SHIFT;
  word_idx    = bit_idx >> BIT_IDX_TO_WORDADDR;
  bit_in_word = 1 << (bit_idx & BIT_IN_WORD_MASK);

#if 0
  if ( ! (bit_in_word & leaf->bitmap[ word_idx ] ))
    consolemsg(" extbmp_add_elem new elem: 0x%08x (%d) mhdr:0x%08x", 
               untagged_w, gen_of(untagged_w), *(ptrof(untagged_w)) );
#endif

  leaf->bitmap[ word_idx ] |= bit_in_word;

  assert2( extbmp_is_member(ebmp, untagged_w ));
}

bool extbmp_is_member( extbmp_t *ebmp, word untagged_w )
{ /* untagged_w in ebmp ? */
  leaf_t *leaf;
  word first;
  bool found;
  unsigned int bit_idx, word_idx, bit_in_word;
  found = find_leaf_or_fail( ebmp, untagged_w, &leaf, &first );

#if 0
  if (found) {
    word lim = leaf_wordaddr_lim( ebmp, first );
    assert2( first <= untagged_w  );
    assert2( untagged_w < lim );
    consolemsg( "extbmp_is_member( ebmp, w=0x%08x )"
                " found, leaf=0x%08x first=0x%08x lim=0x%08x", untagged_w, leaf, first, lim);
  } else {
    consolemsg( "extbmp_is_member( ebmp, w=0x%08x ) unfound", untagged_w );
  }
#endif 

  if (! found)
    return FALSE;

  assert2( first <= untagged_w  );
  assert2( untagged_w < leaf_wordaddr_lim( ebmp, first ));

  bit_idx     = (untagged_w - first) >> BIT_IDX_SHIFT;
  word_idx    = bit_idx >> BIT_IDX_TO_WORDADDR;
  bit_in_word = 1 << (bit_idx & BIT_IN_WORD_MASK);

  return (leaf->bitmap[ word_idx ] & bit_in_word);
}

static void kill_member( extbmp_t *ebmp, word untagged_w ) 
{ /* untagged_w in ebmp ? */
  leaf_t *leaf;
  word first;
  bool found;
  unsigned int bit_idx, word_idx, bit_in_word;
  found = find_leaf_or_fail( ebmp, untagged_w, &leaf, &first );

  if (! found)
    return;

  bit_idx     = (untagged_w - first) >> BIT_IDX_SHIFT;
  word_idx    = bit_idx >> BIT_IDX_TO_WORDADDR;
  bit_in_word = 1 << (bit_idx & BIT_IN_WORD_MASK);

  leaf->bitmap[ word_idx ] &= ~bit_in_word;
}

static bool scan_clear_always( word loc, void *data )
{
  return FALSE;
}

void extbmp_clear_members_in( extbmp_t *ebmp, int gno )
{ /* ebmp := ebmp \ addresses(gno) */
  /* stupid simple but slow impl */
  dbmsg("extbmp_clear_members_in( ebmp, gno=%d )", gno );
  extbmp_enumerate_in( ebmp, gno, scan_clear_always, NULL );
}

static bool scan_accum_count( word loc, void *data )
{
  int *accum = (int*)data;
  *accum += 1;
  return TRUE;
}

int  extbmp_count_members_in( extbmp_t *ebmp, int gno )
{ /* cardinality(ebmp & addresses(gno)) */
  int accum;
  /* stupid simple but slow impl */
  accum = 0;
  extbmp_enumerate_in( ebmp, gno, scan_accum_count, &accum );
  return accum;
}

/* Returns TRUE implies leaf post-enumeration is clear[ed] (ie all zero bits). */
static bool tnode_enum_leaf( extbmp_t *ebmp,
                             int gno,
                             bool ignore_gno, 
                             bool need_tagged_ptr, 
                             leaf_t *leaf, 
                             word first_addr_for_leaf,
                             bool (*scanner)(word loc, void *data), 
                             void *data )
{
  word *bitmap;
  int leaf_words;
  int word_idx, j, bit_in_word;
  word curr_bmp_word, obj;
  bool scan_retval;
  bool found_nonzero_word;

  leaf_words = ebmp->leaf_words;
  bitmap = leaf->bitmap;

  found_nonzero_word = FALSE;

  for (word_idx = 0; word_idx < leaf_words; word_idx += 1) {
    curr_bmp_word = bitmap[ word_idx ];
    if (curr_bmp_word == 0) {
      continue;
    } else {
      word curr_bmp_word_orig = curr_bmp_word;

      if (ignore_gno) {
        dbmsg(     "tnode_enum_leaf"
                   " first_addr_for_leaf:0x%08x word_idx:%4d word:%8x", 
                   first_addr_for_leaf, word_idx, curr_bmp_word );
      } else {
        dbmsg(     "tnode_enum_leaf"
                   " first_addr_for_leaf:0x%08x word_idx:%4d word:%8x gno:%d", 
                   first_addr_for_leaf, word_idx, curr_bmp_word, gno );
      }

      for (j = 0; j < BITS_IN_WORD; j += 1) {
        bit_in_word = (1 << j);
        if (curr_bmp_word & bit_in_word) {
          obj = (first_addr_for_leaf 
                 + ((word_idx*BITS_IN_WORD + j) << BIT_IDX_SHIFT));
          assert( tagof(obj) == 0 );
          if (! ignore_gno) {
            word obj2 = tagptr( obj, PAIR_TAG );

            if (gen_of(obj2) != gno) {
              dbmsg(     "tnode_enum_leaf"
                         " first_addr_for_leaf:0x%08x word_idx:%d j:%2d"
                         " SKIP 0x%08x (%d) looking for gno=%d", 
                         first_addr_for_leaf, word_idx, j, 
                         obj, gen_of(obj2), gno );
              /* can probably do a bit better even without changing to
               * iteration-over-region-address-ranges -- the whole
               * page of an address belongs to the same gno */
              /* If I do that then I will have to revisit how I am
               * handling leaf clearing (see found_nonzero_word); but
               * that will probably be necessary anyway. */
              continue;
            }
          }

          if (need_tagged_ptr) {
            word w = *ptrof(obj);

#if 0
          consolemsg("tnode_enum_leaf"
                     " first_addr_for_leaf:0x%08x word_idx:%4d j:%d"
                     " obj:0x%08x (%d) mhdr:0x%08x", 
                     first_addr_for_leaf, word_idx, j, 
                     obj, gen_of(obj), w );
#endif

            if ( ! ishdr(w) ) {
              obj = tagptr( obj, PAIR_TAG );
            } else if ( header(w) == BV_HDR ) {
              obj = tagptr( obj, BVEC_TAG );
            } else if ( header(w) == VEC_HDR ) {
              obj = tagptr( obj, VEC_TAG );
            } else {
              /* Felix cannot assert this and thinks he knew why once
               * but no longer remembers.
               * assert( header(w) == PROC_HDR ); */
              obj = tagptr( obj, PROC_TAG );
            }
          }

#if 0
          consolemsg("tnode_enum_leaf"
                     " first_addr_for_leaf:0x%08x word_idx:%4d j:%d"
                     " obj:0x%08x (%d)", 
                     first_addr_for_leaf, word_idx, j, 
                     obj, gen_of(obj) );
#endif

          scan_retval = scanner( obj, data );
          if (! scan_retval)
            curr_bmp_word = curr_bmp_word & ~(bit_in_word);
        }
      }
      if (curr_bmp_word != curr_bmp_word_orig) {
        bitmap[ word_idx ] = curr_bmp_word;
      }
      if (curr_bmp_word != 0) {
        found_nonzero_word = TRUE;
      }
    }
  }

  if (! found_nonzero_word) {
    dbmsg("can free this leaf: 0x%08x", leaf );
    return TRUE;
  } else {
    return FALSE;
  }
}

static void tnode_enumerate_slow_but_certain
                           ( extbmp_t *ebmp,
                             int gno, 
                             bool ignore_gno, 
                             tnode_t *tree, 
                             int depth, 
                             word first_addr_for_node, 
                             bool (*scanner)(word loc, void *data), 
                             void *data )
{
  int i;
  word w;
  bool scan_ret;
  for ( i = 0; i < SHIFTED_ADDRESS_SPACE; i += 1 ) {
    assert( i >= 0 );
    w = ((unsigned)i) << BIT_IDX_SHIFT;
    if (extbmp_is_member( ebmp, w )) {
      scan_ret = scanner( w, data );
      if (! scan_ret) {
        kill_member( ebmp, w );
      }
    }
  }
}

/* Returns TRUE implies node post-enumeration is clear[ed] (ie all zero bits). */
static bool tnode_enumerate( extbmp_t *ebmp,
                             int gno, 
                             bool ignore_gno, 
                             tnode_t *tree, 
                             int depth, 
                             word first_addr_for_node, 
                             bool (*scanner)(word loc, void *data), 
                             void *data )
{
  if (depth == 0) {
    return
      tnode_enum_leaf( ebmp, gno, ignore_gno, TRUE, 
                       &tree->leaf, 
                       first_addr_for_node, 
                       scanner, data );
  } else {
    int i;
    int entries;
    inode_t *inode;
    bool found_nonempty_tree;
    bool subtree_is_empty;

    inode = &tree->inode;
    entries = ebmp->entries_per_inode;
    found_nonempty_tree = FALSE;

    for (i=0; i<entries; i++) {
      if (inode->nodes[i] != NULL) {
        if (ignore_gno) {
          dbmsg(      "tnode_enum i:%d depth:%d first_addr:0x%08x addrs_per_child:%lld", 
                      i, depth, first_addr_for_node,
                      inode->addresses_per_child );
        } else {
          dbmsg(      "tnode_enum i:%d depth:%d first_addr:0x%08x addrs_per_child:%lld gno:%d", 
                      i, depth, first_addr_for_node,
                      inode->addresses_per_child, gno );
        }
        subtree_is_empty =
          tnode_enumerate( ebmp, gno, ignore_gno, 
                           inode->nodes[i], depth-1, 
                           (first_addr_for_node + 
                            i * inode->addresses_per_child),
                           scanner, data );
        if (! subtree_is_empty) {
          found_nonempty_tree = TRUE; 
        } else {
          if (depth == 1) {
            free_leaf( ebmp, inode->nodes[i] );
          } else {
            free_inode( ebmp, inode->nodes[i] );
          }
          inode->nodes[i] = NULL;
        }
      }
    }
    if (! found_nonempty_tree) {
      dbmsg("can free this inode: 0x%08x", inode );
    }
    return ! found_nonempty_tree;
  }
}


static void tnode_enumerate_in( extbmp_t *ebmp,
                                int gno, 
                                tnode_t *tree, 
                                int depth, 
                                word first_addr_for_node, 
                                bool (*scanner)(word loc, void *data), 
                                void *data )
{
  tnode_enumerate( ebmp, gno, FALSE, 
                   tree, depth, first_addr_for_node, 
                   scanner, data );
}

static void tnode_enumerate_all( extbmp_t *ebmp,
                                 tnode_t *tree, 
                                 int depth, 
                                 word first_addr_for_node, 
                                 bool (*scanner)(word loc, void *data), 
                                 void *data )
{
  tnode_enumerate( ebmp, -66, TRUE, 
                   tree, depth, first_addr_for_node, 
                   scanner, data );
}

void extbmp_enumerate( extbmp_t *ebmp, 
                       bool (*scanner)(word loc, void *data), 
                       void *data )
{
  /* for each w in ebmp 
   *   invoke scanner( w, data )
   *   if scanner returns FALSE, ebmp := ebmp \ { w }
   */
  /* stupid and slow impl */
  tnode_enumerate_all( ebmp,
                       ebmp->tree, ebmp->depth, 0, 
                       scanner, data );
}

void extbmp_enumerate_in( extbmp_t *ebmp, 
                          int gno, 
                          bool (*scanner)(word loc, void *data), 
                          void *data )
{
  /* for each w in (ebmp & addresses(gno))
   *   invoke scanner( w, data )
   *   if scanner returns FALSE, ebmp := ebmp \ { w }
   */
  /* stupid and slow impl */
  tnode_enumerate_in( ebmp, gno, 
                      ebmp->tree, ebmp->depth, 0, 
                      scanner, data );
}
