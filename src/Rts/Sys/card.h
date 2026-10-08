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
  word bits[1];                 /* roundup(cards, bits per word)/bits per word */
};

/* The card table of the object at p (an untagged pointer), or 0; and
   whether the object gets one.  */
card_table_t *card_table_of( word *p );
bool card_eligible( word obj );

/* Returns the table of obj, making it if necessary, with all cards
   marked or none.  */
card_table_t *card_table_get( word obj, bool all_marked );

/* Mark the card of the slot at byte offset (from the header) or all.  */
void card_mark( card_table_t *ct, word offset );
void card_mark_all( card_table_t *ct );

/* Called by the large object space when the object at p is freed.  */
void card_table_free( word *p );

#define CARD_BITS_PER_WORD (8*sizeof(word))

#endif

/* eof */
