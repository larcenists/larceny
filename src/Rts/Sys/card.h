/* Copyright 2026.                        -*- indent-tabs-mode: nil -*-
 *
 * Larceny run-time system -- card marking for large objects.
 *
 * The generational collector remembers objects, not slots: an old
 * object that points into a younger generation is in the remembered
 * set and is scanned whole at every minor collection.  For a large
 * vector that is a lot of scanning for a few young pointers.
 *
 * So a large object (in the large object space, of CARD_MIN_OBJECT_BYTES
 * or more) that is remembered has a card table: a bitmap with one bit
 * for each CARD_BYTES of its slots.  The write barrier logs the offset
 * of each slot it records (with the object), and the remembered set
 * marks the card of that slot (rs_add_elems_distribute in remset.c); the
 * remembered set scanner scans only the marked cards and unmarks those
 * that no longer point into a younger generation (remset_scanner_oflo
 * in cheney.c).  The invariant: while the object is remembered, every
 * card that holds a pointer into a younger generation is marked.  An
 * object that is remembered without an offset has all its cards marked.
 *
 * The regional collector (-rrof) marks cards when it adds objects to its
 * minor remembered set (add_elems in uremset_extbmp.c), so its marked
 * cards are those written since the last minor collection, the only
 * ones that can point into the nursery; a minor collection scans and
 * unmarks them, keeping the object remembered for the other regions
 * (remset_scanner_oflo_update_rs in cheney.c).
 *
 * For its other collections and for summarization, the regional
 * collector keeps a second bitmap, the across bits: every card that
 * holds a pointer into another region (or the nursery) has its across
 * bit set, whether or not the object is remembered.  A new table has
 * all its across bits set; a write sets the card's across bit with its
 * other bit, and a collection that finds a pointer across in an object
 * it has moved sets that card's (points_across_callback in memmgr.c).
 * A scan of a card clears its across bit when the card points nowhere
 * else (remset_scanner_oflo_update_rs, scan_object_for_remset_summary
 * in summ_matrix.c).  Pointers into the static area, which the write
 * barrier does not record, are not covered.  Summarization also runs
 * between collections, when the SSBs may hold writes whose cards are
 * not yet marked; processing those entries marks the cards and adds
 * the locations to the summaries, as a whole scan would have.
 *
 * The table hangs off the unused word of the large object's pre-header
 * (los.c); the large object space frees it with the object.
 */

#ifndef INCLUDED_CARD_H
#define INCLUDED_CARD_H

#define CARD_BYTES            512
#define CARD_WORDS            (CARD_BYTES/sizeof(word))
#define CARD_MIN_OBJECT_BYTES (64*1024)

typedef struct card_table card_table_t;
struct card_table {
  word cards;                   /* number of cards */
  word bits[1];                 /* the marked bits, then the across bits;
                                   card_nwords( ct ) words each */
};

/* The card table of the object at p (an untagged pointer), or 0; and
   whether the object gets one.  */
card_table_t *card_table_of( word *p );
bool card_eligible( word obj );

/* Returns the table of obj, making it if necessary, with all cards
   marked or none.  */
card_table_t *card_table_get( word obj, bool all_marked );

/* Mark the card of the slot at byte offset (from the header) or all,
   and set its across bit.  */
void card_mark( card_table_t *ct, word offset );
void card_mark_all( card_table_t *ct );

/* Set the across bit of the card of the slot at byte offset.  */
void card_mark_across( card_table_t *ct, word offset );

/* Called by the large object space when the object at p is freed.  */
void card_table_free( word *p );

/* With CARD_CHECK_ACROSS, the scans check (card_check_across) that no
   card whose bit in across is clear points into another generation
   than static_gen and the object's own.  */
#define CARD_CHECK_ACROSS 0
void card_check_across( word obj, card_table_t *ct, word *across,
                        int static_gen, const char *who );

#define CARD_BITS_PER_WORD (8*sizeof(word))
#define card_nwords( ct ) \
  (((ct)->cards + CARD_BITS_PER_WORD - 1) / CARD_BITS_PER_WORD)
#define card_across_bits( ct ) ((ct)->bits + card_nwords( ct ))

#endif

/* eof */
