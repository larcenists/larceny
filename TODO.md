# TODO

Things we have thought of doing.  When an item is finished, move it to
DONE.md (with the date and the commit, if any).

## Native AArch64 Larceny

- Under `-rrof`, the code-scanning pass is skipped while concurrent
  marking is in progress (the mark stack may hold code pointers); it could
  instead update the mark state.  It also copies code referenced only
  from garbage.
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

- `setup`'s `'clean` needs git to find the build products; it could fall
  back to a list of generated file types in a tree without git.
- Out-of-tree builds (a directory per configuration).
- A `setup.sch` command for the second stage (building `larceny.heap`
  with the new binary), and have the first stage print what to run next;
  generalize `src/Build/bootstrap-arm64-native.sh` over the host Scheme
  and target.
- Teach the nightly-build scripts in `test/Scripts` about the Racket host,
  `macosx-arm64`, and `'native`.

## Repository

- Push the 64-bit work on `master` to `origin`.
