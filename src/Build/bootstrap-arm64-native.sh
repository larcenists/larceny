#!/bin/sh
# Bootstrap native Larceny for AArch64 (macOS on Apple Silicon) from
# source, using Racket to host the build.  Run it from the root of a
# Larceny tree; see doc/HOWTO-BUILD.
#
#   src/Build/bootstrap-arm64-native.sh [--clean] [path-to-plt-r5rs]
#
# If the tree holds build products of another configuration (such as a
# Petit Larceny build), setup refuses to proceed; --clean removes them.
#
# Afterwards ./larceny runs the native system.  To use the R7RS and R6RS
# modes, compile their libraries once:
#
#   echo "(require 'r7rsmode) (larceny:compile-r7rs-runtime)" | ./larceny

set -e

CLEAN=
if [ "$1" = "--clean" ]; then
  CLEAN="'clean"
  shift
fi
R5RS=${1:-${PLT_R5RS:-"/Applications/Racket v9.3/bin/plt-r5rs"}}

if [ ! -f setup.sch ]; then
  echo "$0: run this from the root of the Larceny tree" >&2
  exit 1
fi

script=`mktemp -t larceny-bootstrap`
trap 'rm -f "$script"' EXIT
cat > "$script" <<EOS
(load "setup.sch")
(setup 'scheme: 'racket 'host: 'macosx-arm64 'native $CLEAN)
(build-config-files)
(load-compiler)
(build-heap)
(build-runtime)
(build-executable)
(build-larceny-files)
(exit)
EOS

echo "Building the bootstrap heap and runtime (log: bootstrap.log)..."
if ! "$R5RS" --no-prim "$script" > bootstrap.log 2>&1 < /dev/null ||
   [ ! -f arm64.heap ]; then
  grep -A3 "set up for another\|unknown configuration" bootstrap.log >&2 ||
    echo "$0: failed; see bootstrap.log" >&2
  exit 1
fi

echo "Building larceny.heap..."
./larceny.bin -stopcopy -heap arm64.heap \
    -- src/Build/arm64-macosx-larceny-heap.sch >> bootstrap.log 2>&1 < /dev/null

if [ ! -f larceny.heap ]; then
  echo "$0: failed; see bootstrap.log" >&2
  exit 1
fi
echo "Done.  Run ./larceny"
