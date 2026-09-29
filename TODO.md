# TODO

Things we have thought of doing.  When an item is finished, move it to
DONE.md (with the date and the commit, if any).

## Native AArch64 Larceny

- Under `-rrof`, the code-scanning pass is skipped while concurrent
  marking is in progress (the mark stack may hold code pointers); it could
  instead update the mark state.  It also copies code referenced only
  from garbage.
- Lighter-weight millicode calls: every call saves and restores the whole
  register context (allocation is now in line in the common case, but
  arithmetic on non-fixnums, generic comparisons, etc. still call out).
- Peephole optimization for the AArch64 back end (the ARM-32 back end has
  a framework in `src/Asm/Fence/arm-optimizer.sch`); use the CPU's
  return-address predictor for calls and returns.
- Conditional branches reach only +/-1 MB, so larger code vectors fail an
  assertion; fall back to longer sequences.
- Account for the code space in the heap statistics (`memstats`,
  `gclib_stats`).
- FFI and the `-np` collector (neither exists for Petit either).  Part
  of the FFI: `foreign-ctools` compiles its C helpers with `-arch armv4t`
  on ARM, so `src/Build/compile-standard-libraries.sch` (used by the
  nightly builds) fails at the libraries that use it, such as
  `lib/Standard/file-system.sch`.

## Build process footguns

- `setup`'s `'clean` needs git to find the build products; it could fall
  back to a list of generated file types in a tree without git.
- Out-of-tree builds (a directory per configuration).

## Repository

- Push the 64-bit work on `master` to `origin`.
