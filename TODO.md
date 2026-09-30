# TODO

Things we have thought of doing.  When an item is finished, move it to
DONE.md (with the date and the commit, if any).

## Native AArch64 Larceny

- Performance against Racket 9.3 (CS) on the R6RS benchmarks
  (test/Benchmarking/R6RS, 2026-09-30; about 3 s per run, best of 3,
  startup subtracted): geometric mean 0.92 of Racket's time over all
  68, 1.24 without the 12 I/O-bound ones (Racket's R6RS ports are very
  slow).  The largest gaps, with guesses at causes that have not been
  checked:
  - pi 12x (bignum arithmetic);
  - compiler 7.3x and normalization 7.1x (not yet looked at);
  - mbrot 5.0x, mbrotZ 4.4x, fft 1.9x (flonums are boxed);
  - fibc 3.1x, ctak 2.6x (call/cc); ack 3.0x, divrec 2.9x, deriv
    2.3x (deep recursion, stack overflow into the heap?);
  - equal 3.0x, string 2.8x, vecsort 2.1x.
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
- FFI and the `-np` collector (neither exists for Petit either).  Part
  of the FFI: `foreign-ctools` compiles its C helpers with `-arch armv4t`
  on ARM, so `src/Build/compile-standard-libraries.sch` (used by the
  nightly builds) fails at the libraries that use it, such as
  `lib/Standard/file-system.sch`.

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

## Build process footguns

- `bootstrap.sh --r7rs` reports success when compiling the libraries
  fails; it should check `bootstrap-r7rs.log` for errors.
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
