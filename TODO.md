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
- More peephole optimization for the AArch64 back end (the ARM-32 back
  end has a framework in `src/Asm/Fence/arm-optimizer.sch`), e.g. the
  stack overflow check and timer tick sequences.
- `-rrof` fails with this tree's `larceny.heap` (built 2026-09-29 15:40
  by `../accomplice-native2`): compiling the test/Lib files corrupts the
  heap ("symbol-hash: #<WEIRD OBJECT>", then a panic at
  `Sys/summ_matrix.c;2212`).  The same runtime with native2's heap
  passes, and native2's runtime with this heap fails, so it is in the
  heap (or a latent collector bug it exposes), not the runtime.
- Native mutation primitives (`set-car!`, `bytevector-u8-set!`, ...)
  leave whatever was in RESULT as their value, so the REPL prints e.g.
  `(set-car! p 2)` as a large fixnum (a heap address); Petit returns
  nothing interesting.  Harmless to the collector (addresses look like
  fixnums), but untidy.
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
- `gc_compute_dynamic_size` (memmgr.c) compares the new heap size in
  bytes against `stats.heap_allocated_max`, which is in words, so "use
  no less than before" is only a quarter as strong as intended on 64-bit.
- Many diagnostic messages print `word`/`s_word` values with `%d`/`%x`.

## Build process footguns

- `setup`'s `'clean` needs git to find the build products; it could fall
  back to a list of generated file types in a tree without git.
- Out-of-tree builds (a directory per configuration).

## Repository

- Push the 64-bit work on `master` to `origin`.
