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
  - fibc 3.1x, ctak 2.5x (call/cc): capture (creg_get: flush, then
    restore the top frame) and reinstating a continuation (creg_set)
    switch to C; the returns through the flushed frames no longer do
    (930bfa49), which gained only 1-4%.  In-line capture next?
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
- Compiling src/Lib/Common/record.sch prints eight warnings
  `Assertion is false: (.check! #f 198 .obj .rtd)`: a record check
  that Twobit proves false, probably in dead code, as put-char's were
  (be71e240).
- In R5RS mode, with-exception-handler does not catch errors that
  primitives signal (`(car 1)`, a failed inline check): the system
  exception handler reports them and enters the debugger, though it
  does catch `(error ...)`.  The R6RS and R7RS modes raise conditions.
- bytevector-u8-set! (and bytevector-set!) accept -128 through 255:
  the compiler macros in common.imp.sch check (>= x -128).  R6RS
  requires an octet for bytevector-u8-set!.  Native stores (b 0 -1) as
  255; Petit's inline bytevector-set! rejects negative values, and the
  error message is "bytevector-like-set!: confused by ...".
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
- vector-copy and the other vector copies move an element per
  iteration; into a freshly allocated vector a memmove would need no
  barrier (check SATB marking under -rrof first).
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
    loads its length every iteration (Twobit has no code motion).
  - The timer check is four instructions per iteration (load, test,
    decrement, store the countdown in globals).
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

## Runtime

- Single objects are limited to 2^31 words (16GB) on 64-bit systems,
  because the collectors keep word counts and indices within an object
  in ints.
- A large vector in the remembered set is rescanned whole at every
  minor collection (the remset records objects, not slots): a young
  object stored into a 4.8GB vector makes each minor collection take
  about 100 ms.  Card marking for large objects?
- The reader in `#!r6rs` mode accepts `@` and `@b` as symbols, accepts
  a byte-order mark (U+FEFF) after `#!r6rs`, and does not treat
  U+2029, U+202F, U+205F and U+3000 as whitespace; the read0 benchmark
  (test/Benchmarking/R6RS) reports these.  Also, the `#^B"..."`
  codevector syntax of fasl files (`makeCodevector` in `reader.sch`)
  loops at an unterminated end of input, as strings did.

- `timezone-offset` (lib/Standard/time.sch) returns the standard-time
  offset during daylight saving time (-18000 for EDT, not -14400): it
  computes mktime(gmtime(t)) with tm_isdst forced to 0.

## Build process footguns

- `bootstrap.sh --r7rs` reports success when compiling the libraries
  fails; it should check `bootstrap-r7rs.log` for errors.
- Smoke-test `--r6rs` mode routinely.  The test suites run their
  programs in `--r7rs` mode, so a library that strict R6RS syntax
  cannot read (rnrs control, 2a28596a until fixed) went unnoticed until
  a benchmark ran.  `MODE=--r6rs test/R7RS/Lib/testr6rs-larceny` runs
  the R6RS suite in that mode; it could run by default (both modes),
  or the build (`bootstrap.sh --r7rs`) could end by running a small
  `--r6rs` program that imports `(rnrs)`.
- Feature markers, follow-ups:
  - A system provides exactly the features its heap's code assumes; it
    could provide several target specs (say, an older calling-convention
    version it still supports), and the C runtime is not versioned on
    its own.
  - The startup check (a heap whose code lacks what the collector needs)
    has not been seen to fire: no such heap can be built now.
  - `.lap` markers are written and compared by the build, but nothing
    loads `.lap` files outside it.
- `setup`'s `'clean` needs git to find the build products; it could fall
  back to a list of generated file types in a tree without git.
- Out-of-tree builds (a directory per configuration).

## Repository

- Push the 64-bit work on `master` to `origin`.
