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
#include "gc_t.h"
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
 * Covering a 32-bit address space requires ( 2^32 / bytes_covered_per_leaf )
 * leaves.  Assuming a leaf covers exactly 1 chunk as described above, this is
 *     2^32 / 256K == 16K leaves.
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

static const int DEFAULT_LEAF_BYTES = (8*4096);
static const int DEFAULT_ENTRIES_PER_INODE = 46; /* branching factor */

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

/* Microsoft's C compiler doesn't like this:
 * #define dbmsg( format, args... ) if (0) consolemsg( format, ## args )
 */
#define dbmsg( format, ... ) if (0) consolemsg( format, ## __VA_ARGS__ )

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
  long long address_range_in_words;
  long long address_words_per_child;
  /* XXX FIXME move above into INCLUDE_REDUNDANT_FIELDS */

  tnode_t *nodes[DYNAMIC_SIZE];
};
struct leaf  {
#if INCLUDE_REDUNDANT_FIELDS
  struct metadata_always_first metadata;
  word start;
  word limit;
#endif
  int    gno; /* If gno > 0 then leaf's values are in gno; if 0 then mixed */
  leaf_t *prev_for_gno; /* doubly-linked list of elems w/ same gno field */
  leaf_t *next_for_gno;
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
  struct {
    leaf_t *leaf;        /* NULL iff cache invalid */
    word   first_addr_for_leaf;
  } mru_cache;           /* Most recently access via find_xxx */
  leaf_t **gno_to_leaf;  /* Maps gno g to leaves l with l->gno == g */
  int gno_count;
  int leaf_count;
};

static void insert_leaf_in_list_core( extbmp_t *ebmp, leaf_t *leaf, int gno ) 
{
  leaf_t *old_first;

  old_first = ebmp->gno_to_leaf[gno];

  assert( leaf->prev_for_gno == NULL );
  assert( leaf->next_for_gno == NULL );
  assert( (old_first == NULL) || (old_first->prev_for_gno == NULL) );

  leaf->gno = gno;

  if (old_first != NULL) {
    old_first->prev_for_gno = leaf;
    leaf->next_for_gno = old_first;
  }

  ebmp->gno_to_leaf[gno] = leaf;
}

static void insert_leaf_in_list( extbmp_t *ebmp, leaf_t *leaf, int gno ) 
{
  dbmsg("new leaf of %d", gno );
  insert_leaf_in_list_core( ebmp, leaf, gno );
}

static void move_leaf_to_mixed_list( extbmp_t *ebmp, leaf_t *leaf, int gno )
{
  int old_gno;
  leaf_t *prev, *next;

  assert( leaf->gno != 0 );

#if 0
  consolemsg("leaf [0x%08x,0x%08x] of %d became mixed with %d", leaf->start, leaf->limit, leaf->gno, gno );
#endif

  old_gno = leaf->gno;
  prev = leaf->prev_for_gno;
  next = leaf->next_for_gno;

  if (prev != NULL) 
    prev->next_for_gno = next;
  if (next != NULL)
    next->prev_for_gno = prev;

  leaf->prev_for_gno = NULL;
  leaf->next_for_gno = NULL;

  if (ebmp->gno_to_leaf[old_gno] == leaf) {
    assert( prev == NULL );
    ebmp->gno_to_leaf[old_gno] = next;
  }

  insert_leaf_in_list_core( ebmp, leaf, 0 );
}

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
  l->gno = -2;
  l->prev_for_gno = NULL;
  l->next_for_gno = NULL;
}
static tnode_t *alloc_leaf( extbmp_t *ebmp, word start, word limit ) 
{
  tnode_t *retval;
  ebmp->leaf_count += 1;
  retval = alloc_tnode( ebmp, tag_leaf, 
                        (sizeof( leaf_t )
                         + ebmp->leaf_words*sizeof(word)));
  assert( ((word*)start + (ADDRS_PER_WORD*ebmp->leaf_words)) == (word*)limit );
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
  ebmp->leaf_count -= 1;
  if (leaf == (tnode_t*)ebmp->mru_cache.leaf) {
    ebmp->mru_cache.leaf                = NULL;
    ebmp->mru_cache.first_addr_for_leaf = 0;
  }

  { 
    leaf_t *l;
    l = &leaf->leaf;
    if (l->next_for_gno != NULL)
      l->next_for_gno->prev_for_gno = l->prev_for_gno;
    if (l->prev_for_gno != NULL)
      l->prev_for_gno->next_for_gno = l->next_for_gno;
    if (ebmp->gno_to_leaf[ l->gno ] == l) {
      ebmp->gno_to_leaf[ l->gno ] = l->next_for_gno;
    }
  }

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
                             long long address_range_in_words, 
                             word start, 
                             long long limit )
{
  tnode_t *retval;

  assert( address_range_in_words > 0 );

  retval = alloc_tnode( ebmp, tag_inode, 
                        (sizeof( inode_t ) 
                         + ebmp->entries_per_inode*sizeof( tnode_t* )));
  retval->inode.address_range_in_words = address_range_in_words;
  retval->inode.address_words_per_child = 
    CEILDIV( ((long long)address_range_in_words), ebmp->entries_per_inode );

  assert( retval->inode.address_words_per_child > 0 );
  assert((retval->inode.address_words_per_child 
          * ebmp->entries_per_inode) 
         >= retval->inode.address_range_in_words);

#if INCLUDE_REDUNDANT_FIELDS
  retval->inode.start = start;
  retval->inode.limit = limit;
#endif

  init_inode_fields_cleared( ebmp, &retval->inode );

  dbmsg(     "alloc_inode(ebmp,%8lld,start=0x%08x,limit=0x%08llx)"
             " ==> 0x%08x", 
             address_range_in_words, start, limit, retval);
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
  long long address_range_in_words;

  assert( (leaf_bytes % MIN_BYTES_PER_OBJECT) == 0 );
  max_leaves = SHIFTED_ADDRESS_SPACE / (BITS_PER_WORD * leaf_words);
  assert( max_leaves > 0 );
  { /* calculate max depth of tree */
    int i = 0;
    long long tot = 1;
    long long addr_range = ADDRS_PER_WORD * leaf_words;
    while (tot < max_leaves) {
      i   += 1;
      tot *= entries_per_node;
      addr_range *= entries_per_node;
    }
    depth = i;
    address_range_in_words = addr_range;
  }

  ebmp = (extbmp_t*)must_malloc( sizeof( extbmp_t ));
  ebmp->gc                = gc;
  ebmp->leaf_words        = CEILDIV(leaf_bytes,sizeof(word));
  ebmp->entries_per_inode = entries_per_node;
  ebmp->depth             = depth;
  ebmp->tree
    = alloc_inode( ebmp, 
                   address_range_in_words, 
                   0, 
                   (((long long)SHIFTED_ADDRESS_SPACE) << BIT_IDX_SHIFT));

  ebmp->mru_cache.leaf    = NULL;
  ebmp->mru_cache.first_addr_for_leaf = 0;

  ebmp->gno_count = gc->gno_count;
  ebmp->gno_to_leaf = (leaf_t**)must_malloc( sizeof(leaf_t*) * ebmp->gno_count );
  { 
    int i;
    for (i = 0; i < ebmp->gno_count; i++) {
      ebmp->gno_to_leaf[i] = NULL;
    }
  }
  ebmp->leaf_count = 0;

  annoyingmsg( "ebmp{gc,leaf_words=%d,entries_per_inode=%d,depth=%d,tree} max_leaves:%d",
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
    word *new_start_addr;
    word *new_addr_limit;

    assert2( tree->metadata.tag == tag_inode );

    inode = &tree->inode;
    idx   = 
      ( ((untagged_w - start_addr_of_node_coverage) / sizeof(word))
        / inode->address_words_per_child );

    assert2( idx >= 0 );
    assert2( idx < ebmp->entries_per_inode );

    new_start_addr = 
      ((word*)start_addr_of_node_coverage
       + idx * inode->address_words_per_child );
    new_addr_limit = 
      (new_start_addr
       + inode->address_words_per_child );
    assert2( new_start_addr <= (word*)untagged_w );
    assert2( (word*)untagged_w  < new_addr_limit );

    if (inode->nodes[ idx ] == NULL) {
      if (alloc_if_unfound) {
        if (depth == 1) {
          assert( (new_start_addr + (ADDRS_PER_WORD*ebmp->leaf_words)) 
                  == new_addr_limit );
          inode->nodes[ idx ] =  alloc_leaf( ebmp,
                                             (word)new_start_addr, 
                                             (word)new_addr_limit );
          assert2( inode->nodes[idx]->metadata.tag == tag_leaf );
        } else {
          inode->nodes[ idx ] = alloc_inode( ebmp, 
                                             inode->address_words_per_child,
                                             (word)new_start_addr, 
                                             (word)new_addr_limit );
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
                                   (word)new_start_addr, 
                                   leaf_recv,
                                   first_addr_for_leaf,
                                   alloc_if_unfound );

    assert2( tree->metadata.tag == tag_inode );
    return retval;
  }
}

static word leaf_wordaddr_lim( extbmp_t *ebmp, word first );
static word strip_tag( word w ) {
  return (word)ptrof(w);
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
  bool retval;

  dbmsg(     "find_leaf_calc_offset"
             "( ebmp, 0x%08x, "
             "leaf_recv, first_addr_for_leaf, "
             "alloc_if_unfound=%s )", 
             untagged_w, (alloc_if_unfound?"TRUE":"FALSE") );

  if (ebmp->mru_cache.leaf != NULL 
      && (ebmp->mru_cache.first_addr_for_leaf 
          <= strip_tag(untagged_w))
      && (strip_tag(untagged_w)
          < leaf_wordaddr_lim( ebmp, ebmp->mru_cache.first_addr_for_leaf))) {
    *leaf_recv                = ebmp->mru_cache.leaf;
    *first_addr_for_leaf_recv = ebmp->mru_cache.first_addr_for_leaf;
    return TRUE;
  }

  tree = ebmp->tree;
  depth = ebmp->depth;
  start_addr_of_node_coverage = 0;

  retval = 
    find_leaf_calc_offset_recur( ebmp, untagged_w, 
                                 tree, depth, start_addr_of_node_coverage,
                                 leaf_recv, first_addr_for_leaf_recv, 
                                 alloc_if_unfound );
  if (retval) {
    ebmp->mru_cache.leaf                = *leaf_recv;
    ebmp->mru_cache.first_addr_for_leaf = *first_addr_for_leaf_recv;
  }
  return retval;
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

/* Returns TRUE iff untagged_w was already in ebmp */
bool extbmp_add_elem( extbmp_t *ebmp, word untagged_w )
{ /* ebmp := ebmp U { untagged_w } */
  leaf_t *leaf;
  word first, lim;
  word entry_word;
  bool retval;
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

  entry_word = leaf->bitmap[ word_idx ];
  retval = entry_word & bit_in_word;
  entry_word |= bit_in_word;
  leaf->bitmap[ word_idx ] = entry_word;

  assert2( extbmp_is_member(ebmp, untagged_w ));

  {
    int gno;
    gno = gen_of( untagged_w );
    assert( gno != 0 );
    if (leaf->gno == 0) {
      /* mixed leaf; do nothing */
    } else if (leaf->gno < 0) {
      insert_leaf_in_list( ebmp, leaf, gno );
    } else if (leaf->gno != gno) {
      move_leaf_to_mixed_list( ebmp, leaf, gno );
    }
  }

  return retval;
}

void extbmp_del_elem( extbmp_t *ebmp, word untagged_w ) 
{
  leaf_t *leaf;
  word first;
  bool found;
  unsigned int bit_idx, word_idx, bit_in_word;
  found = find_leaf_or_fail( ebmp, untagged_w, &leaf, &first );

  if (found) {
    bit_idx     = (untagged_w - first) >> BIT_IDX_SHIFT;
    word_idx    = bit_idx >> BIT_IDX_TO_WORDADDR;
    bit_in_word = 1 << (bit_idx & BIT_IN_WORD_MASK);

    if (leaf->bitmap[ word_idx ] & bit_in_word) {
      leaf->bitmap[ word_idx ] &= ~bit_in_word;
    }
  }
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
  extbmp_enumerate_in( ebmp, FALSE, gno, scan_clear_always, NULL );
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
  extbmp_enumerate_in( ebmp, FALSE, gno, scan_accum_count, &accum );
  return accum;
}

/* Returns TRUE implies entire leaf post-enumeration is clear[ed] (ie all zero bits). 
 * 
 * Note that a low limit_addr may lead to partial inspection of the
 * leaf, which could yield false negative (FALSE when leaf is clear).
 */
static bool tnode_enum_leaf( extbmp_t *ebmp,
                             int gno,
                             bool ignore_gno, 
                             bool gno_is_static_area, 
                             bool need_tagged_ptr, 
                             leaf_t *leaf, 
                             word first_addr_for_leaf,
                             word first_enum_addr,
                             word limit_enum_addr, 
                             bool (*scanner)(word loc, void *data), 
                             void *data )
{
  word *bitmap;
  int leaf_words, leaf_word_limit;
  int word_idx, j, bit_in_word;
  word curr_bmp_word, obj;
  bool scan_retval;
  bool found_nonzero_word;
  word limit_addr_for_leaf;

  leaf_words = ebmp->leaf_words;

  limit_addr_for_leaf = (first_addr_for_leaf 
                         + (2*sizeof(word)*8*sizeof(word))*leaf_words);
  assert( limit_addr_for_leaf == leaf_wordaddr_lim( ebmp, first_addr_for_leaf ));

#if 0
  if (ignore_gno)
    consolemsg( "tnode_enum_leaf( ebmp, gno ignored, need_tagged_ptr=%s, leaf, ..) "
                " leaf_addrs:[0x%08x,0x%08x) enum:[0x%08x,0x%08x)", 
                need_tagged_ptr?"TRUE":"FALSE", 
                first_addr_for_leaf, limit_addr_for_leaf, 
                first_enum_addr, limit_enum_addr );
  else 
    consolemsg( "tnode_enum_leaf( ebmp, gno=%d refd, need_tagged_ptr=%s, leaf, ..) "
                " leaf_addrs:[0x%08x,0x%08x) enum:[0x%08x,0x%08x)", 
                gno, need_tagged_ptr?"TRUE":"FALSE", 
                first_addr_for_leaf, limit_addr_for_leaf, 
                first_enum_addr, limit_enum_addr );
#endif

  bitmap = leaf->bitmap;

  found_nonzero_word = FALSE;

  if (first_enum_addr >= limit_addr_for_leaf) {
    return FALSE;
  } else if (first_enum_addr > first_addr_for_leaf) {
    /* skipping words before enum start addr */
    int delta = (first_enum_addr - first_addr_for_leaf);
    int newidx = (delta >> BIT_IDX_SHIFT) / BITS_IN_WORD;
#if 0
    consolemsg("leaf_words:%d enum:[0x%08x,0x%08x) leaf:[0x%08x,0x%08x) delta:%d newidx:%d",
               leaf_words, first_enum_addr, limit_enum_addr, 
               first_addr_for_leaf, limit_addr_for_leaf, delta, newidx );
#endif
    if (newidx > 0)
      found_nonzero_word = TRUE; /* be conservative since we're skipping words. */
    word_idx = newidx;
  } else {
    word_idx = 0;
  }
  assert( word_idx >= 0 && word_idx < leaf_words );

  if (limit_enum_addr <= first_addr_for_leaf) {
    return FALSE;
  } else if (limit_enum_addr < limit_addr_for_leaf) {
    int delta = (limit_addr_for_leaf - (word)limit_enum_addr);
    int newidx = leaf_words - (delta >> BIT_IDX_SHIFT) / BITS_IN_WORD;
    /* skipping words after enum limit addr */
#if 0
    consolemsg("leaf_words:%d enum:[0x%08x,0x%08x) leaf:[0x%08x,0x%08x) delta:%d newidx:%d",
               leaf_words, first_enum_addr, limit_enum_addr, 
               first_addr_for_leaf, limit_addr_for_leaf, delta, newidx );
#endif
    if (newidx < leaf_words)
      found_nonzero_word = TRUE; /* be conservative since we're skipping words */
    leaf_word_limit = newidx;
  } else {
    leaf_word_limit = leaf_words;
  }
  assert( leaf_word_limit >= 0 && leaf_word_limit <= leaf_words );

  for (; word_idx < leaf_word_limit; word_idx += 1) {
    curr_bmp_word = bitmap[ word_idx ];
    if (curr_bmp_word == 0) {
      continue;
    } else {
      word curr_bmp_word_orig = curr_bmp_word;

      if (ignore_gno) {
        dbmsg(     "tnode_enum_leaf"
                   " first_addr_for_leaf:0x%08x word_idx:%4d word:%8x       "
                   " enum:[0x%08x,0x%08x)", 
                   first_addr_for_leaf, word_idx, curr_bmp_word, 
                   first_enum_addr, limit_enum_addr );
      } else {
        dbmsg(     "tnode_enum_leaf"
                   " first_addr_for_leaf:0x%08x word_idx:%4d word:%8x gno:%d"
                   " enum:[0x%08x,0x%08x)",
                   first_addr_for_leaf, word_idx, curr_bmp_word, gno, 
                   first_enum_addr, limit_enum_addr );
      }

      for (j = 0; j < BITS_IN_WORD; j += 1) {
        bit_in_word = (1 << j);
        if (curr_bmp_word & bit_in_word) {
          obj = (first_addr_for_leaf 
                 + ((word_idx*BITS_IN_WORD + j) << BIT_IDX_SHIFT));
          assert( tagof(obj) == 0 );
          if (! ignore_gno) {
            word obj2 = tagptr( obj, PAIR_TAG );

            if ((gno_is_static_area && (gen_of(obj2) != ebmp->gc->gno_count-1))
                ||
                ((! gno_is_static_area) && (gen_of(obj2) != gno))) {
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
                             bool gno_is_static_area, 
                             tnode_t *tree, 
                             int depth, 
                             word first_addr_for_node, 
                             int *leaves_enumerated, 
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
  *leaves_enumerated = 0;
}

/* Returns TRUE implies node post-enumeration is clear[ed] (ie all zero bits). */
static bool tnode_enumerate( extbmp_t *ebmp,
                             bool incl_tag, 
                             int gno, 
                             bool ignore_gno, 
                             bool gno_is_static_area, 
                             tnode_t *tree, 
                             int depth, 
                             word first_addr_for_node, 
                             word first_enum_addr, 
                             word limit_enum_addr, 
                             int *leaves_enumerated, 
                             int *nodes_enumerated, 
                             bool (*scanner)(word loc, void *data), 
                             void *data )
{
  if (depth == 0) {
    *leaves_enumerated += 1;
    return
      tnode_enum_leaf( ebmp, gno, ignore_gno, gno_is_static_area, incl_tag, 
                       &tree->leaf, 
                       first_addr_for_node, 
                       first_enum_addr, limit_enum_addr, 
                       scanner, data );
  } else {
    int i;
    int entries;
    int idx_start, idx_limit;
    inode_t *inode;
    bool found_nonempty_tree;
    bool subtree_is_empty;
    word limit_addr_for_node;

    *nodes_enumerated += 1;

    inode = &tree->inode;
    entries = ebmp->entries_per_inode;
    found_nonempty_tree = FALSE;
    limit_addr_for_node = 
      (word)
      ((word*)first_addr_for_node) + (inode->address_words_per_child * entries);

#if 1
    if (limit_enum_addr <= first_addr_for_node) {
      return FALSE;
    }
#endif

#if 1
    {
      bool cmpresult;
      long long limit = 
        ((long long)first_addr_for_node
         + sizeof(word)*(long long)inode->address_range_in_words);
      cmpresult = ((long long)first_enum_addr > limit);

#if 0
      if (1 || cmpresult)
        consolemsg("tnode_enumerate "
                   "child:[0x%08x,0x%08llx)  enum:[0x%08x,0x%08x) "
                   "cmpresult %s ",
                   first_addr_for_node, limit, 
                   first_enum_addr, limit_enum_addr,
                   cmpresult?"TRUE":"FALSE");
#endif

      if (cmpresult)
        return FALSE;
    }
#endif

    if (first_enum_addr > first_addr_for_node) {
      /* skipping words before enum start addr */
      unsigned int delta = (first_enum_addr - first_addr_for_node) / sizeof(word);
      unsigned int newidx = (delta / inode->address_words_per_child);
      if (newidx > 0)
        found_nonempty_tree = TRUE; /* be conservative since we're skipping words. */
      idx_start = newidx;
    } else {
      idx_start = 0;
    }
    assert( idx_start >= 0 && idx_start < entries );

    if (limit_enum_addr < limit_addr_for_node) {
      unsigned int delta = (limit_addr_for_node - limit_enum_addr) / sizeof(word);
      unsigned int newidx = entries - (delta / inode->address_words_per_child);
      /* skipping words after enum limit addr */
      if (newidx < entries)
        found_nonempty_tree = TRUE; /* be conservative since we're skipping words */
      idx_limit = newidx;
    } else {
      idx_limit = entries;
    }
    assert( idx_limit >= 0 && idx_limit <= entries );

    for (i=idx_start; i<idx_limit; i++) {
      if (inode->nodes[i] != NULL) {
        if (ignore_gno) {
          dbmsg(      "tnode_enum i:%d depth:%d first_addr:0x%08x limit:0x%08x "
                      "addrs_per_child:%lld", 
                      i, depth, first_addr_for_node, limit_enum_addr, 
                      inode->address_words_per_child );
        } else {
          dbmsg(      "tnode_enum i:%d depth:%d first_addr:0x%08x limit:0x%08x "
                      "addrs_per_child:%lld gno:%d", 
                      i, depth, first_addr_for_node, limit_enum_addr, 
                      inode->address_words_per_child, gno );
        }
        subtree_is_empty =
          tnode_enumerate( ebmp, incl_tag, gno, ignore_gno, gno_is_static_area, 
                           inode->nodes[i], depth-1, 
                           (word)
                           (((word*)first_addr_for_node) + 
                            i * inode->address_words_per_child),
                           first_enum_addr, limit_enum_addr, 
                           leaves_enumerated, 
                           nodes_enumerated, 
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

struct apply_scan_hdr_address_range_data {
  extbmp_t *ebmp;
  bool incl_tag;
  int  gno;
  bool gno_is_for_static_area; 
  bool (*scanner)(word loc, void *data);
  int leaves_enumerated;
  int nodes_enumerated;
  int ranges_enumerated;
  void *data;
};
static void apply_scan_hdr_address_range( word *s, word *l, void *d)
{
  int leaves_enumerated, nodes_enumerated;
  struct apply_scan_hdr_address_range_data *apply_data;
  apply_data = (struct apply_scan_hdr_address_range_data*)d;
  leaves_enumerated = 0;
  nodes_enumerated = 0;
  tnode_enumerate( apply_data->ebmp, 
                   apply_data->incl_tag, 
                   apply_data->gno, FALSE,
                   apply_data->gno_is_for_static_area, 
                   apply_data->ebmp->tree, apply_data->ebmp->depth,
                   0, (word)s, (word)l,
                   &leaves_enumerated, 
                   &nodes_enumerated, 
                   apply_data->scanner, apply_data->data );
  apply_data->leaves_enumerated += leaves_enumerated;
  apply_data->nodes_enumerated += nodes_enumerated;
  apply_data->ranges_enumerated += 1;
}

static void tnode_enumerate_in( extbmp_t *ebmp,
                                bool incl_tag, 
                                int gno, 
                                bool gno_is_static_area, 
                                int *leaves_enumerated, 
                                int *nodes_enumerated, 
                                int *ranges_enumerated, 
                                bool (*scanner)(word loc, void *data), 
                                void *scan_data )
{
  struct apply_scan_hdr_address_range_data apply_data;
  apply_data.ebmp    = ebmp;
  apply_data.incl_tag = incl_tag;
  apply_data.gno     = gno;
  apply_data.gno_is_for_static_area = gno_is_static_area;
  apply_data.scanner = scanner;
  apply_data.data    = scan_data;
  apply_data.leaves_enumerated = 0;
  apply_data.nodes_enumerated = 0;
  apply_data.ranges_enumerated = 0;
  gc_enumerate_hdr_address_ranges( ebmp->gc, 
                                   gno, 
                                   apply_scan_hdr_address_range,
                                   &apply_data );
  *leaves_enumerated = apply_data.leaves_enumerated;
  *nodes_enumerated = apply_data.nodes_enumerated;
  *ranges_enumerated = apply_data.ranges_enumerated;
}

static void tnode_enumerate_in_slow( 
                                extbmp_t *ebmp,
                                bool incl_tag, 
                                int gno, 
                                bool gno_is_static_area, 
                                int *leaves_enumerated, 
                                int *nodes_enumerated, 
                                int *ranges_enumerated, 
                                bool (*scanner)(word loc, void *data), 
                                void *data )
{
  *ranges_enumerated = 0;
  tnode_enumerate( ebmp, incl_tag, gno, FALSE, gno_is_static_area, 
                   ebmp->tree, ebmp->depth, 
                   0, 0, 0xFFFFFFFF,
                   leaves_enumerated, 
                   nodes_enumerated, 
                   scanner, data );
}

static void tnode_enumerate_all( extbmp_t *ebmp,
                                 bool incl_tag, 
                                 tnode_t *tree, 
                                 int depth, 
                                 word first_addr_for_node, 
                                 bool (*scanner)(word loc, void *data), 
                                 void *data )
{
  int leaves_enumerated, nodes_enumerated;
  leaves_enumerated = 0;
  nodes_enumerated = 0;
  tnode_enumerate( ebmp, incl_tag, -66, TRUE, FALSE, 
                   tree, depth, 
                   first_addr_for_node, 0, 0xFFFFFFFF, 
                   &leaves_enumerated, 
                   &nodes_enumerated, 
                   scanner, data );
}

void extbmp_enumerate( extbmp_t *ebmp, 
                       bool incl_tag, 
                       bool (*scanner)(word loc, void *data), 
                       void *data )
{
  /* for each w in ebmp 
   *   invoke scanner( w, data )
   *   if scanner returns FALSE, ebmp := ebmp \ { w }
   */
  /* stupid and slow impl */
  tnode_enumerate_all( ebmp,
                       incl_tag, 
                       ebmp->tree, ebmp->depth, 0, 
                       scanner, data );
}

int count_gno_leaves( extbmp_t *ebmp, int gno ) 
{
  int c;
  leaf_t *l;
  c = 0;
  l = ebmp->gno_to_leaf[gno];
  while (l != NULL ) {
    assert( l->gno == gno );
    c += 1;
    l = l->next_for_gno;
  }
  return c;
}

int count_all_leaves( extbmp_t *ebmp ) 
{
  int c;
  int i;
  c = 0;
  for (i = 0; i < ebmp->gno_count; i++ ) {
    c += count_gno_leaves( ebmp, i );
  }
  return c;
}

void extbmp_enumerate_in( extbmp_t *ebmp, 
                          bool incl_tag, 
                          int gno, 
                          bool (*scanner)(word loc, void *data), 
                          void *data )
{
  int leaves_enumerated;
  int nodes_enumerated;
  int ranges_enumerated;
  bool gno_is_for_static_area = 
    ((ebmp->gc->static_area != NULL) &&
     (gno == ebmp->gc->gno_count-1));

  /* for each w in (ebmp & addresses(gno))
   *   invoke scanner( w, data )
   *   if scanner returns FALSE, ebmp := ebmp \ { w }
   */
  leaves_enumerated = 0;
  nodes_enumerated = 0;
  ranges_enumerated = 0;
  tnode_enumerate_in( ebmp, incl_tag, gno, gno_is_for_static_area,
                      &leaves_enumerated, 
                      &nodes_enumerated, 
                      &ranges_enumerated, 
                      scanner, data );

  dbmsg( "extbmp_enumerate_in gno: %d leaves: %d mixed: %d all: %d "
              "leaf_count: %d "
              "leaves_enumerated: %d "
              "nodes_enumerated: %d "
              "ranges_enumerated: %d", 
              gno, 
              count_gno_leaves( ebmp, gno ), 
              count_gno_leaves( ebmp, 0 ),
              count_all_leaves( ebmp ),
              ebmp->leaf_count, 
              leaves_enumerated, 
              nodes_enumerated,
              ranges_enumerated );
}

void extbmp_expand_remset_gnos( extbmp_t *ebmp, int fresh_gno )
{
  int i;
  int new_gno_count = ebmp->gno_count + 1;
  leaf_t **new_gno_to_leaf;
  leaf_t *leaf;


  new_gno_to_leaf = (leaf_t**)must_malloc( sizeof(leaf_t*) * new_gno_count );
  for (i = 0; i < fresh_gno; i++ ) {
    new_gno_to_leaf[ i ] = ebmp->gno_to_leaf[i];
  }
  new_gno_to_leaf[ fresh_gno ] = NULL;
  for ( i = fresh_gno+1; i < new_gno_count; i++ ) {
    new_gno_to_leaf[ i ] = ebmp->gno_to_leaf[i-1];
    leaf = new_gno_to_leaf[i];
    while (leaf != NULL) {
      leaf->gno += 1;
      leaf = leaf->next_for_gno;
    }
  }

  free( ebmp->gno_to_leaf );

  ebmp->gno_count = new_gno_count;
  ebmp->gno_to_leaf = new_gno_to_leaf;
}
