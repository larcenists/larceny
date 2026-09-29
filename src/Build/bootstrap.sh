#!/bin/sh
# Build Larceny from source on macOS/arm64 (Apple Silicon), hosted by
# Racket or by an existing Larceny.  Run it from the root of a Larceny
# tree; see doc/HOWTO-BUILD.
#
#   src/Build/bootstrap.sh [--native | --petit] [--racket [PLT-R5RS]]
#                          [--larceny LARCENY] [--clean] [--r7rs]
#
#   --native   build native Larceny (the default)
#   --petit    build Petit Larceny
#   --racket   host the build with Racket's plt-r5rs (the default);
#              the path defaults to $PLT_R5RS, or the Racket v9.3 install
#   --larceny  host the build with the given larceny script, such as
#              ../other-tree/larceny
#   --clean    remove the build products of another configuration first
#   --r7rs     also compile the R7RS/R6RS/SRFI libraries
#
# Afterwards ./larceny runs the new system.  The log is in bootstrap.log.

set -e

TARGET="'native"
HOST=racket
HOST_CMD=${PLT_R5RS:-"/Applications/Racket v9.3/bin/plt-r5rs"}
CLEAN=
R7RS=

while [ $# -gt 0 ]; do
  case "$1" in
    --native)  TARGET="'native" ;;
    --petit)   TARGET= ;;
    --racket)  HOST=racket
               case "$2" in ''|--*) ;; *) HOST_CMD=$2; shift ;; esac ;;
    --larceny) HOST=larceny; HOST_CMD=$2; shift ;;
    --clean)   CLEAN="'clean" ;;
    --r7rs)    R7RS=yes ;;
    *) echo "$0: unknown option: $1" >&2; exit 2 ;;
  esac
  shift
done

if [ ! -f setup.sch ]; then
  echo "$0: run this from the root of the Larceny tree" >&2
  exit 1
fi

script=`mktemp -t larceny-bootstrap`
trap 'rm -f "$script"' EXIT
cat > "$script" <<EOS
(load "setup.sch")
(setup 'scheme: '$HOST 'host: 'macosx-arm64 $TARGET $CLEAN)
(build-config-files)
(load-compiler)
(build-heap)
(build-runtime)
(build-executable)
(build-larceny-files)
(build-larceny-heap)
(exit)
EOS

if [ -n "$TARGET" ]; then heap=larceny.heap; else heap=petit-larceny.heap; fi
rm -f $heap

echo "Building (log: bootstrap.log)..."
case $HOST in
  racket)  "$HOST_CMD" --no-prim "$script" > bootstrap.log 2>&1 < /dev/null ;;
  larceny) "$HOST_CMD" -- "$script" > bootstrap.log 2>&1 < /dev/null ;;
esac || true

if [ ! -f $heap ]; then
  grep -A3 "set up for another\|unknown configuration" bootstrap.log >&2 ||
    echo "$0: failed; see bootstrap.log" >&2
  exit 1
fi

if [ -n "$R7RS" ]; then
  echo "Compiling the R7RS/R6RS/SRFI libraries (log: bootstrap-r7rs.log)..."
  printf "(require 'r7rsmode)\n(larceny:compile-r7rs-runtime)\n(exit)\n" |
    ./larceny > bootstrap-r7rs.log 2>&1
fi
echo "Done.  Run ./larceny"
