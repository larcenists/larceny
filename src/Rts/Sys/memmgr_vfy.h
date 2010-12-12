/* Copyright 2010 Felix S Klock II.
 * 
 * $Id$ 
 * 
 */
void verify_summaries_via_oracle( gc_t *gc );
void verify_remsets_via_oracle( gc_t *gc );
void verify_nursery_summary_via_oracle( gc_t *gc );
void verify_summaries_via_oracle( gc_t *gc );
void verify_smircy_via_oracle( gc_t *gc );

/* ugh; macro pattern in memmgr led me to pass argument via global
 * rather than use a parameter. :(  */
extern int verify_fwdfree_via_oracle_gen_no; 
void verify_fwdfree_via_oracle( gc_t * gc );
