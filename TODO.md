# TODO

Things we have thought of doing.  When an item is finished, move it to
DONE.md (with the date and the commit, if any).

## Native AArch64 Larceny

- Support the regional collector (`-rrof`) natively.  Needs the SATB
  pre-write barrier in the Fence code generator (`emit-write-barrier` in
  `src/Asm/Fence/pass5p2.sch`), and a way to collect the code space when
  there is no single oldest generation to own it.
- Cheaper allocation: every allocation is a millicode call that saves
  and restores the whole register context (an allocation-heavy loop takes
  85 ms native vs 54 ms Petit).  Inline allocation, or lighter-weight
  millicode entry points.
- Peephole optimization for the AArch64 back end (the ARM-32 back end has
  a framework in `src/Asm/Fence/arm-optimizer.sch`); use the CPU's
  return-address predictor for calls and returns.
- Conditional branches reach only +/-1 MB, so larger code vectors fail an
  assertion; fall back to longer sequences.
- Account for the code space in the heap statistics (`memstats`,
  `gclib_stats`).
- FFI and the `-np` collector (neither exists for Petit either).

## Build process footguns

- Different configurations (Petit vs native, Racket-hosted vs
  Larceny-hosted) write build products to the same paths in the source
  tree, and `make` decides staleness by timestamps only.  Have `setup`
  record a fingerprint of the configuration (host Scheme, target, word
  size, features) and clean everything generated when it changes, or
  refuse without an explicit `'clean`.
- A single `clean-all` that knows every generated artifact (`.lop`,
  `.lap`, `.fasl`, `.slfasl`, `.o`, `.so`, heaps, generated headers and
  tables) and keeps `src/Rts/Shared/arithmetic.c` regenerable.
- Racket-hosted builds write bytevectors in `.lop` files as Racket byte
  strings (`#"..."`), which Larceny's reader cannot read; write them in
  Larceny syntax instead so `.lop` files are host-independent.
- Out-of-tree builds (a directory per configuration).
- A `setup.sch` command for the second stage (building `larceny.heap`
  with the new binary), and have the first stage print what to run next;
  generalize `src/Build/bootstrap-arm64-native.sh` over the host Scheme
  and target.
- Teach the nightly-build scripts in `test/Scripts` about the Racket host,
  `macosx-arm64`, and `'native`.

## Repository

- Push the 64-bit work on `master` to `origin`.
