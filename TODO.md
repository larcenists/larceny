# TODO

Things we have thought of doing.  When an item is finished, move it to
DONE.md (with the date and the commit, if any).

## Native AArch64 Larceny

- Under `-rrof`, collecting code by scanning keeps code that is
  referenced only from garbage until the garbage's region is collected
  (a delay, not a leak: code use levels off).  The marks of a completed
  mark cycle could tell live objects from dead ones, but a dead
  object's code pointer would then have to be cleared, and `sro` can
  return dead objects.
- Lighter-weight millicode calls, what is left: calls still switch the
  whole context (now with paired stores).  Other candidates for inline
  or context-free fast paths: the SATB barrier (only during marking),
  eqv? on flonums, mixed-exactness arithmetic on ARM-32 (no
  cant.fixnum->flonum there yet).
- More peephole optimization for the AArch64 back end (the ARM-32 back
  end has a framework in `src/Asm/Fence/arm-optimizer.sch`), e.g. the
  stack overflow check and timer tick sequences.
- Twobit's memory use explodes on enormous single procedures (a
  procedure of 8000 statements needs over 2 GB); the AArch64 long-branch
  mode for code vectors over 1 MB could only be tested with its
  threshold lowered.
- FFI and the `-np` collector (neither exists for Petit either).  Part
  of the FFI: `foreign-ctools` compiles its C helpers with `-arch armv4t`
  on ARM, so `src/Build/compile-standard-libraries.sch` (used by the
  nightly builds) fails at the libraries that use it, such as
  `lib/Standard/file-system.sch`.

## Runtime

- Single objects are limited to `LARGEST_OBJECT` (0x7FFFFFF0 bytes, in
  `src/Rts/Sys/larceny.h`) on 64-bit systems too; `make-bytevector` of
  3e9 bytes signals "requested allocation exceeds max object size".
  Raising it needs a look at the millicode's `int nwords` and the
  copying collector's `unsigned` word counts.
- Many diagnostic messages print `word`/`s_word` values with `%d`/`%x`.

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
- `setup`'s `'clean` needs git to find the build products; it could fall
  back to a list of generated file types in a tree without git.
- Out-of-tree builds (a directory per configuration).

## Repository

- Push the 64-bit work on `master` to `origin`.
