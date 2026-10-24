# TODO

Things we have thought of doing.  When an item is finished, move it to
DONE.md (with the date and the commit, if any).

## Native AArch64 Larceny

- Performance against Racket 9.3 (CS) on the R6RS benchmarks
  (test/Benchmarking/R6RS, 2026-09-30; about 3 s per run, best of 3,
  startup subtracted, Larceny running the compiled .slfasl as the
  bench script does; runs of the .sch compile it first, which cost the
  compiler benchmark 18 s): geometric mean 0.79 of Racket's time over
  all 68, 1.04 without the 12 I/O-bound ones (Racket's R6RS ports are
  very slow); Larceny faster on 42.  compiler 0.74x, equal 0.46x,
  string 0.99x.  Outliers to investigate, with guesses at causes that
  have not been checked:
  - fibc 1.1x, ctak 1.1x (call/cc; 3.1x and 2.5x before cd5ef179,
    ac48c2a5, 0e0bcb2c, 7d77d125).  What is left: capture (creg_get:
    flush, then restore the top frame) switches to C, about a quarter
    of fibc's time; not worth moving to assembly for now.  Returning
    through flushed frames: see DONE, 2026-10-07 (restoring several
    frames per underflow measured, not worth it).
  - ack 3.0x, divrec 2.9x, deriv 2.3x: deep recursion (see the
    return-prediction item below; not the stack cache or the GC);
  - vecsort 1.4x (2.2x before fxarithmetic-shift-right was in line);
  - normalization 1.7x (4.95 s; Racket 2.90): measured by parts
    (2026-09-30), normalizing takes 3.1 s (Racket 1.0), get-line 0.84
    (1.08), and the benchmark's own parsing 0.8 (0.65).  decompose
    was rewritten (d20ae85b; 50 iterations 3.07 s -> 2.55); what is
    left of normalizing is per-call and per-character overhead on
    strings of a few characters, and compose-2 below.
- compose-2 (unicode4.sch) finds a composition by two binary searches
  (the second character, then the first); a table like those for
  combining classes and decompositions would do it in a few loads.
- Procedures still taking optional arguments as rest lists, which are
  called rarely or are variadic by nature: the open-*-port procedures,
  make-transcoder, make-hashtable, exit, the text procedures
  (text2.sch).  For copies of a few characters a loop is still about
  twice as fast as string-copy! (decompose-copy! in unicode4.sch).
- In code that Petit interprets, case-lambda procedures are named
  case-lambda: .make-case-lambda finds the name in a compiled clause's
  constant vector, which interpreted procedures do not have.
- Return prediction.  Calls use BL/BLR and returns RET, so the CPU's
  return-address stack predicts returns; it holds only a few dozen
  entries, and deeper recursion mispredicts most returns: 1e8 calls
  take 97 ms at depth 10, 329 ms at depth 100, 466 ms at depth 1000
  and above (Racket: about 100 ms at any depth up to 30000).  Returning
  with BR instead (indirect-branch prediction) is flat at 104-112 ms,
  but in an A/B comparison (2026-09-30, BR time / RET time) it is a
  tradeoff: ack 0.30, divrec 0.41, primes 0.68, equal 0.97; compiler,
  deriv, matrix, quicksort, simplex, ray about 1.0; compile-file of an
  11000-line file 1.06, gcbench 1.08, fib 1.10, nucleic 1.10, earley
  1.12.  Kept RET.  Idea: BR only in recursion Twobit can see (non-tail
  self-calls; a call to a global named like the enclosing definition).
  Pushes and pops must stay paired (a BR return leaves its caller's BL
  entry behind and every later RET mispredicts), so a recursive
  procedure would get an internal entry that its recursive calls reach
  with B (no push) and that returns with BR, and an external entry
  that reaches it with B through one extra frame and returns with RET.
  Expect fib (shallow recursion) to lose about 10% even so.
  Prototyped 2026-10-07 and set aside (not committed): in the assembler,
  for an internal procedure (.proc entered at a label) that calls
  itself non-tail and makes no tail call out: its self-calls ADR/store/
  B, its returns BR, and every other entry (setrtn/branch, branch, and
  jump or setrtn/jump from nested lambdas, found through a MAL label
  number derived from the entry's) goes through a stub: a frame whose
  return point does the pop and RET.  (Top-level defines of an R6RS
  program are such internal procedures of the program's code vector,
  and the benchmark harness reaches them with setrtn/jump.)  Results on
  the R6RS benchmarks: ack 1.95 s -> 0.58 (0.30), divrec 3.38 -> 1.41
  (0.42), primes 0.53 -> 0.28, conform/paraffins/graphs 0.88-0.93;
  geometric mean 0.95 over 52; but shallow recursion loses, the RET it
  replaced being perfectly predicted: tak 1.16, fib 1.09, triangl 1.09,
  takl 1.08, ntakl 1.07, gcbench 1.04.  Whether recursion is deep is
  not known statically; revisit with some way to predict it (types or
  size prediction), or as an opt-in compiler switch.  (A mismatched
  push or pop costs only a misprediction: RET goes to its register.)
- Bulk copies, what is left (see f97a0cf2 and the commits after it):
  SRFI 13's `%string-copy!` (lib/SRFI/srfi-13.sch, srfi/%3a13.sls) and
  SRFI 132's `vector-portion-copy!` still copy an element at a time,
  as do the rarely used procedure-copy, io/clone-port, the record and
  enum constructors' vector copies, and lib/Base's string-insert! and
  pp.sch.  list->vector walks the list twice (length, then the fill
  syscall); 1000 elements take 2.2 us.  A stack underflow copies the
  frame a word at a time (fence_stack_underflow, stk_restore_frame),
  but frames are short.
- Dead values in the registers of the running procedure are still
  roots: the collector clears only registers that the code in REG0
  does not use at all (see DONE, 2026-10-07).  A loop that leaves a
  large object in a register it uses, such as an argument register of
  a call or a temporary, keeps the previous iteration's object through
  the next collection, which promotes it (into a region under -rrof);
  300 iterations of `(f6 60 bv 0 (make-vector 100000 0) 0 0)` do 21
  region collections instead of none.  The retention is bounded (one
  object per register, until the code writes that register again,
  usually the next iteration) and no benchmark has shown it, so this
  is low priority.  The fix: a mask of live registers per collection
  point (keyed by the return address), from Twobit's register
  allocation.  Not aging large objects (keeping them young for one more
  collection): that guesses at lifetimes where the cause is liveness,
  and it would make long-lived large objects cost an extra collection
  each (a large pointer vector is scanned whole at every minor
  collection).
- Under `-rrof`, a code collection that does not follow a completed
  mark cycle (with two regions, or when no marking is in progress)
  still keeps code referenced only from garbage until the garbage's
  region is collected.
- Under `-rrof -size1 1M`, a program with 35MB of live data that loads
  a compiled file 100000 times (keeping each procedure briefly) grows
  the heap to 145M words (1.1GB): garbage floats in the regions.  Is
  the policy right for small regions?
- Lighter-weight millicode calls, what is left: the remaining calls
  still switch the whole context (with paired stores).  Mixed-exactness
  arithmetic on ARM-32 (no cant.fixnum->flonum there yet), which cannot
  be tested on this machine.
- More optimization for the AArch64 back end, what is left:
  - Loop-invariant code: a loop over a vector still checks its type and
    loads its length every iteration (Twobit has no code motion: loops
    are lambda-lifted self-tail-recursive procedures, and pass 3's
    commoning stops at calls).  Measured 2026-10-07, not done: with all
    run-time checks off (runtime-safety-checking #f, the most any check
    elimination could give) the R6RS benchmarks are 3.3% faster
    (geometric mean of 55); puzzle 23%, pnpoly 21%, quicksort 14%,
    scheme and triangl 13%, graphs/simplex/conform/nucleic 7-9%, most
    under 5%.  By hand: puzzle's loops (fit, place, puzzle-remove), with
    the global loads, the inner (vector-ref *p* i) and the vectors' type
    checks hoisted and the bounds checks kept, run as fast as unsafe
    (21%).  pnpoly gains nothing from removing all its vector checks:
    its gain is in the flonum checks, partly on loop-invariant
    parameters (x, y; liftable) and partly on elements loaded from the
    vectors (not liftable: a vector's contents have no type).  A narrow
    pass would do: self-tail-recursive loops that make no calls and no
    assignments to the globals involved, hoisting invariant loads and
    the type checks of invariant values, with one test at entry and a
    checked copy of the loop otherwise (versioning), bounds checks kept.
    A further step on the same machinery: a pre-scan at entry that finds
    all of a vector's elements to be (say) flonums, so the fast copy can
    skip the element checks; it costs a pass over the vector (bad for
    loops that exit early, unless the index range is known), and an
    interrupt in the fast copy must continue in the checked copy (a
    handler could store into the vector).  Payoff limited to a few
    numeric benchmarks (pnpoly, simplex, nucleic, fft).
  - The timer check is four instructions per iteration (load, test,
    decrement, store the countdown in globals).  Measured 2026-10-07
    (R6RS benchmarks with ticks left out, upper bounds): all ticks 3.2%
    (geometric mean of 52), backward-branch ticks 2.0% (array1 23%,
    ntakl/takl 15%, puzzle 10%), call ticks 1.5% (fib 16%, tak 9%).
    Tried and not committed: keeping the timer in x24 inside loops that
    make no calls (an assembler pre-pass over the MAL: load at the loop's
    label, back-edges to a label after the load, store at returns, at
    exits through trampolines, and around millicode calls but the write
    barriers; no runtime change).  It works, but gains nothing: best of
    10, a counting loop 656 ms -> 695 (no tick: 602), a vector sum 672
    -> 658 (no tick: 554).  The load and store cost little on this core
    (store-to-load forwarding); the compare-and-branch is the cost, so a
    gain needs fewer checks (unrolled loops ticking once per several
    iterations, say), not cheaper ones.
  - Inlining the generational write barrier's generation test would
    save little: its fast path costs about 0.45 ns per pointer store
    (1e8 stores: 66 ms for fixnums, 110 ms for pointers within a
    generation), and would add about 11 instructions per store site.
    Recording old-to-young stores costs 2.4 ns per store.
- Twobit on enormous single procedures: commoning (pass 3) still
  allocates about 30K words per statement, so a procedure of 7000
  statements, just under `anf-threshold`, peaks at 560 MB.
- The `-np` collector (it does not exist for Petit either).
- FFI on AArch64, what is left: callouts work (native and Petit), but
  not callbacks (C calling Scheme; the README says they were never
  finished off SPARC) or variadic functions (MacOS passes variadic
  arguments on the stack).  `foreign-ctools` compiles its C helpers
  with `-arch armv4t` on ARM, so `src/Build/compile-standard-libraries.sch`
  (used by the nightly builds) fails at the libraries that use it,
  such as `lib/Standard/file-system.sch`.

- Hypothetical, not planned: faster bignums (pi is 6.8x Racket's
  time) and unboxed flonums (mbrot 5.0x, mbrotZ 4.4x, fft 1.9x), unless
  some very local analysis (a flonum loop's variables, say) would do.

## x86-64 Linux

- Native x86-64 (see DONE, 2026-10-10), follow-ups:
  - Short forward branches.  The shared assembler (Asm/Shared/pass5p1.sch)
    makes one pass, so a forward branch's size is chosen before its
    target is known: rel32 (6-byte jcc) for every forward branch,
    including those to the deferred slow paths at the end of the code
    vector; rel8 only backward.  (Sassy relaxed branches over several
    passes.)  Options: assemble forward branches short and reassemble a
    code vector with the overflowing labels marked long (fits the
    one-pass assembler), or relax within the target (alignment and the
    return-point asserts read `here` eagerly, so they would have to
    become symbolic).  Fence's generic code is size-agnostic otherwise.
  - Returns are JMP [STKP+off], which the return-address predictor does
    not see.  With rsp as STKP (as IAssassin had it) calls and returns
    could be CALL/RET; needs signals on an alternate stack.
  - Only 8 MacScheme registers.  TMP2 (rdx) is needed by few sequences
    (IDIV, variable shifts, setrtn/strtn); freeing it would give a ninth.
    Measure first: no benchmarks have been run.
  - Peepholes using memory operands (x86 can add/compare to memory),
    and multi-byte NOPs for alignment padding.
  - No disassembler for x86-64 code in the debugger (`i @`).
  - macOS on Intel: the back end should work; the millicode uses ELF
    names (no leading underscore) and the code space needs no MAP_JIT.
- The 16/32/64-bit bytevector accessors are open-coded as byte loads
  and shifts (bytevector-accessor-rules, Compiler/common.imp.sch;
  lockpick's LARCENY-NOTES.md item 1), follow-ups:
  - Summing 10^7 elements, bytevector-u32-native-ref takes 0.059 s
    against vector-ref's 0.020 s (it was 0.24 s): four byte loads, three
    shifts and three ors, each with its fixnum check.  A primop for an
    aligned 16/32-bit load (and a 64-bit one that falls back when the
    value is not a fixnum), in Fence for both targets, would close the
    gap; so would dropping the fixnum checks of .fxlogior and .fxlsh
    when representation inference has proved their operands fixnums.
  - Not yet tested on AArch64 (native or Petit), nor on a 32-bit
    target, where the 32- and 64-bit accessors only check their
    arguments and call bytevector-uint-ref and friends.
  - bytevector-uint-set! and bytevector-sint-set!, called directly,
    still do not check that the value fits in the given size (they
    store its low bytes); the accessors check before calling them.
  - bytevector-ieee-double-native-ref and -set! are out-of-line calls
    that copy the eight bytes one at a time (into a new flonum, for
    the ref); not measured.
- gcc warns (-Wformat) about debugging messages that print words and
  pointers with %x in Sys/extbmp.c and Sys/uremset_array.c (dead
  dbmsg calls, mostly); use PRIxWORD and %p.
- C23: the runtime's bool is an int-sized enum, so 64-bit builds pass
  -std=gnu17 (gcc 15 defaults to C23, where bool is a keyword).
  Building as C23 would need the functions that return other integers
  as bool to be found first.

## R7RS

- In --r7strict mode (Petit and native x86-64 alike, 2026-10-10) the
  flonum zero misbehaves: `(zero? (string->number "0.0"))` and
  `(= 0.0 0)` are #f, and 0.0 prints as 1e-999 (`(exact 0.0)` is
  1/10^999); in --r7rs mode all is well.  Repro: a program importing
  (scheme base) (scheme write) that writes `(- 1.5 1.5)`.
- SRFI 4 (from upstream, lib/SRFI/srfi/4/impl.scm; found 2026-10-17 on
  native x86-64): `u8vector->list` calls `larceny:numvec->list` on
  the bytevector that `make-u8vector` (= `make-bytevector`) makes, so
  `(u8vector->list (make-u8vector 2 0))` fails with "not a record of
  the correct type" (`bytevector->u8-list` would do).  And
  `(make-u8vector 1 -1)` is accepted (make-bytevector's R6RS fill
  range is -128..255).
- An error in a `--program` program says "Error while loading
  src/Build/<platform>-larceny-heap.sch" instead of naming the
  program (the heap was dumped while loading that file); old heaps do
  the same.
- lib/SRFI/test/srfi-158-test.sps7 prints nothing: it has no
  test-begin/test-end, so SRFI 64 reports no summary.  With them
  around the test groups it reports 80 passed.

## Runtime

- Single objects are limited to 2^31 words (16GB) on 64-bit systems,
  because the collectors keep word counts and indices within an object
  in ints.
- Card marking (see DONE, 2026-10-07): what still scans a large
  vector whole is the collection of its own region (it is forwarded,
  so every slot is scanned; 26 ms for a 50-million-word vector),
  concurrent marking, and the first scan after its card table is made
  (all across bits set).  Per-card "points into its own region" bits
  could cover the first.
- The regional collector's summary oracle (`USE_ORACLE_TO_VERIFY_SUMMARIES`
  in memmgr.c, run with `-oracle_countdown 1`) has rotted: it fails
  without card marking too, on `loc_to_obj` (summary_t.h, which
  asserts) and on an assertion that every SSB is empty
  (summ_matrix.c), though summarization also runs between collections
  (`incremental_rgnl`), when they need not be.
- `lib/Experimental/scheme-dump-heap.sch` (see DONE, 2026-10-24):
  - Test it on AArch64 macOS.
  - Which bytevectors are code is decided by address (the code arena
    is below the data arena, osdep-generic.c), and where the address
    cannot tell, by a pattern in constant vectors (a bytevector followed
    by a vector), which a bytevector literal followed by a vector
    literal also matches.  Such a literal in the text segment can still
    be changed on Linux (the code space is RWX), but on MacOS (MAP_JIT)
    a write to it from Scheme would most likely fault.  The exact fix
    is a runtime query: a syscall answering whether an object is on a
    code page (`is_code_page`), used by the dumper when the runtime has
    it.
  - Under `-rrof` the image's one large bytevector prints "allocate:
    holy cow"; allocating it in pieces would not.
  - Make it loadable with `require` in R5RS mode (R7RS and R6RS code
    can `(import (larceny dump-heap))`, lib/R6RS/larceny/dump-heap.sld),
    and document it in the user manual; or replace the runtime's stub
    with it.

## Debugger

- Petit's heap does not load the debugger (lib/Debugger): the load in
  src/Build/petit-larceny-heap.sch is quoted out ("FIXME: this stuff is
  commented out as well", from upstream), so an error there returns to
  the REPL.  Find out why it was left out, and whether it works now.
- Native: `i @` prints a compiled procedure's machine code as a long
  bytevector; at a breakpoint the current frame is
  debug/enter-debugger's, not the procedure's (FIXME in debug.sch).

## Build process footguns

- Feature markers, follow-ups:
  - A system provides exactly the features its heap's code assumes; it
    could provide several target specs (say, an older calling-convention
    version it still supports), and the C runtime is not versioned on
    its own.
  - The startup check (a heap whose code lacks what the collector needs)
    has not been seen to fire: no such heap can be built now.
  - `.lap` markers are written and compared by the build, but nothing
    loads `.lap` files outside it.
- The first Larceny-hosted build after a Racket-hosted one recompiles
  every library (seen on x86-64 Linux, Petit, 2026-10-10: 183 files,
  9 minutes, with nothing changed); a second Larceny-hosted build
  compiles nothing.  Find out which staleness check fires (tool file
  times or feature markers written by the Racket host).
- `setup`'s `'clean` needs git to find the build products; it could fall
  back to a list of generated file types in a tree without git.
- Out-of-tree builds (a directory per configuration).

## Repository

- Push the 64-bit work on `master` to `origin`.
