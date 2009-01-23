typedef struct gc_data gc_data_t;

/* The 'remset' table in the gc structure has one extra element if the
   collector uses the non-predictive dynamic area: that extra element is
   the young->old remembered set for the non-predictive collector.  The
   extra element is always the last one, and the slots in the SSB tables
   for that remset are the last slots.  The index is indicated by the
   np_remset member of gc_t.
   */
struct gc_data {
  bool is_partitioned_system;   /* True if system has partitioned heap */
  bool use_np_collector;	/* True if dynamic area is non-predictive */
  bool shrink_heap;		/* True if heap can be shrunk */
  bool fixed_ephemeral_area;    /* True iff ephemeral_area_count is invariant */

  int  dynamic_min;		/* 0 or lower limit of expandable area */
  int  dynamic_max;		/* 0 or upper limit of expandable area */
  int  nonexpandable_size;	/* Size of nonexpandable areas (but RROF?) */

  word *globals;
  word *handles;               /* array of handles */
  int  nhandles;               /* current array length */
  int  in_gc;                  /* a counter: > 0 means in gc */
  int  generations;            /* number of generations (incl. static) */
  int  generations_after_gc;   /* number of generations in rts after gc complete */
  int  static_generation;	/* Generation number of static area */
  word **ssb_bot;
  word **ssb_top;
  word **ssb_lim;
  word *satb_ssb_bot;
  word *satb_ssb_top;
  word *satb_ssb_lim;

  old_heap_t **ephemeral_area;
    /* In precise collectors: An array of pointers to ephemeral areas;
       the number of entries is held in ephemeral_area_count.  May be NULL.
       */
  int ephemeral_area_count;
    /* The number of entries in the ephemeral_area table.
       */
  int region_count;
    /* For regional collector.  During cycle of collections, the regions
        { ephemeral_area[i] | region_count <= i < ephemeral_area_count }
       are new and should not be processed until cycle completes
       */
  old_heap_t *dynamic_area;
    /* In precise collectors: A pointer to a dynamic area, or NULL.
       */

  bool rrof_currently_minor_gc;
    /* true implies the collector is currently doing a minor collection. */
  int rrof_to_region;
    /* In RROF collector, the to-space for minor collections. */
  int rrof_next_region;
    /* In RROF collector, the next region scheduled for major collection. */
  int rrof_last_tospace;
    /* In RROF collector, the region used as a to-space in the last collect */
  double rrof_load_factor;
    /* Lars put a load factor in each old-heap; RROF needs one load_factor */

  double rrof_sumz_budget;
    /* In RROF collector, B*N/R (where B = budget) is number of
       summaries (and thus major collections) that we have available
       before starting a wave of summary construction to support the
       next major collection cycle. */
  double rrof_sumz_coverage;
    /* In RROF collector, C*N/R (where C = coverage) is initial number
       of summaries that we will try to construct during each heap
       scan during a wave of summary construction. */

  bool   rrof_has_refine_factor; /* With factor R,                         */
  double rrof_refinement_factor; /*   countdown = ceil((R*heap) / nursery) */
  int rrof_refine_mark_period;
  int rrof_refine_mark_countdown;
    /* In RROF collector, #nursery evacuations until refine remset via mark.
       If negative, then that is number of nursery evacuations we've had
       since the mark was scheduled to occur. */

  int rrof_last_live_estimate; 
    /* In RROF collector, gradual approximation of live storage in bytes.
     * (It is continually reset based on marking results and then 
     *  refined by repeated averaging with the sum of major collection
     *  sizes.)
     */
  int rrof_cycle_count;
    /* In RROF collector, number of cycles that have been completed
     * (a cycle is complete when every region present at the start
     *  of the cycle has been considered to take part in a major
     *  collection.)
     */

  summary_t summary;            /* NULL or summarization of remset array */
  bool      use_summary_instead_of_remsets;
  int       next_summary_to_use;
  summ_matrix_t *summaries;

  semispace_t *secondary_space; /* NULL or space for when tospace overflows */

  int stat_last_ms_remset_sumrize;
  int stat_last_ms_remset_sumrize_cpu;
  int stat_last_ms_mark_refinement;
  int stat_last_ms_mark_refinement_cpu;
  int stat_length_minor_gc_run;

  bool print_float_stats_each_cycle;
  bool print_float_stats_each_major;
  bool print_float_stats_each_minor;
  bool print_float_stats_each_refine;

  /* these are precise measures according to heap snapshots */
  int last_live_words;
  int max_live_words;
  int words_promoted_since_snapshot_completed;
  int words_promoted_since_snapshot_began; 
  /* need to track these separately, since storage is allocated
   * concurrently with snapshotting. */
};

#define DATA(gc) ((gc_data_t*)(gc->data))

#define INCLUDE_POP_RGNS_IN_LOADCALC 1
