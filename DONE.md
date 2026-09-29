# DONE

Finished items, moved here from TODO.md.  Newest first.

## 2026-09-29

- Nightly-build scripts (`test/Scripts/nightly-build`, `checkout-git`)
  know macosx-arm64, native and Petit, hosted by Larceny or Racket (175aec4d).
- `(build-larceny-heap)` / `(build-twobit-heap)` for the second build
  stage; `src/Build/bootstrap.sh` over host (Racket or Larceny) and
  target (native or Petit); Larceny hosts of another kind load build
  files from source (38263b70).
- Native `-rrof`: SATB pre-write barrier in the Fence code generator;
  under the regional collector the code space belongs to the static
  generation and is collected by a scanning pass (29a90872).
- `setup` records the build configuration in `build-configuration` and
  refuses to mix configurations; `'clean` removes the build products
  (via git), `'keep-build-products` keeps them (d14b9927).
- Racket-hosted builds write `.lop` and `.fasl` files in Larceny syntax,
  so Larceny can read them (46333896).
- REPL/debugger no longer loop forever ("Read error on port") when the
  console must be reopened but there is no controlling terminal; the
  console falls back to /dev/null (27a26528).
- Native build in the main tree, bootstrapped with the Larceny in
  `../accomplice-native2`; R7RS/R6RS libraries compiled.
- Ignore `arm64.heap` and `bootstrap.log`.
- `src/Build/bootstrap-arm64-native.sh`: one command to bootstrap native
  AArch64 Larceny from Racket (3509d216).
- Code space collects code when much code is installed (0524c5d6).
- Native Larceny for AArch64 on macOS (34a6a685), with the collectable
  code space (f15e8caa), 64-bit support in the Fence code generator
  (2557ab85), and the AArch64 back end (272bbe8b).  Passes test/Lib, the
  compiler tests, and the R7RS (8782) and R6RS (6260) suites with the
  default and stop-and-copy collectors.
- 64-bit Petit Larceny on macOS/arm64, bootstrapped from Racket, with the
  regional collector working (62135dce).
