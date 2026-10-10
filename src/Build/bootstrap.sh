#!/bin/sh
# Build Larceny from source on macOS/arm64 (Apple Silicon) or, Petit
# Larceny only, on x86-64 Linux, hosted by Racket or by an existing
# Larceny.  Run it from the root of a Larceny
# tree; see doc/HOWTO-BUILD.
#
#   src/Build/bootstrap.sh [--native | --petit] [--racket [PLT-R5RS]]
#                          [--larceny LARCENY] [--clean] [--r7rs]
#
#   --native   build native Larceny (the default on macOS)
#   --petit    build Petit Larceny (the default on Linux)
#   --racket   host the build with Racket's plt-r5rs (the default);
#              the path defaults to $PLT_R5RS, or the Racket v9.3 install
#              on macOS, or plt-r5rs in the PATH elsewhere
#   --larceny  host the build with the given larceny script, such as
#              ../other-tree/larceny
#   --clean    remove the build products of another configuration first
#   --r7rs     also compile the R7RS/R6RS/SRFI libraries
#
# Afterwards ./larceny runs the new system.  The log is in bootstrap.log.

set -e

case `uname -s`-`uname -m` in
  Darwin-arm64)
    PLATFORM=macosx-arm64
    TARGET="'native"
    DEFAULT_PLT_R5RS="/Applications/Racket v9.3/bin/plt-r5rs" ;;
  Linux-x86_64)
    PLATFORM=linux-x86-64
    TARGET=
    DEFAULT_PLT_R5RS=plt-r5rs ;;
  *)
    echo "$0: unsupported system: `uname -s` `uname -m`" >&2
    exit 1 ;;
esac
HOST=racket
HOST_CMD=${PLT_R5RS:-$DEFAULT_PLT_R5RS}
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

script=`mktemp "${TMPDIR:-/tmp}/larceny-bootstrap.XXXXXX"`
stamp=`mktemp "${TMPDIR:-/tmp}/larceny-bootstrap-stamp.XXXXXX"`
trap 'rm -f "$script" "$stamp" "$stamp.r7rs" "$stamp.sps"' EXIT
cat > "$script" <<EOS
(load "setup.sch")
(setup 'scheme: '$HOST 'host: '$PLATFORM $TARGET $CLEAN)
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
if [ -n "$TARGET" ]; then bin=larceny.bin; else bin=petit-larceny.bin; fi
# The heap is not removed first: the host may be this tree's own Larceny.
# The build succeeded if it wrote a heap newer than the stamp.  (When
# make fails, build-runtime stops the build, so no heap is written; the
# installed binary is a fresh copy in any case, so its time says
# nothing.)
sleep 1

echo "Building (log: bootstrap.log)..."
case $HOST in
  racket)  "$HOST_CMD" --no-prim "$script" > bootstrap.log 2>&1 < /dev/null ;;
  larceny) "$HOST_CMD" -- "$script" > bootstrap.log 2>&1 < /dev/null ;;
esac || true

if [ ! -f $heap ] || [ ! $heap -nt "$stamp" ] || [ ! -f $bin ]; then
  grep -A3 "set up for another\|unknown configuration" bootstrap.log >&2 ||
    grep "error:" bootstrap.log >&2
  echo "$0: failed; see bootstrap.log" >&2
  exit 1
fi

if [ -n "$R7RS" ]; then
  echo "Compiling the R7RS/R6RS/SRFI libraries (log: bootstrap-r7rs.log)..."
  # An error exits with status 1: the debugger would read its commands
  # from the terminal.
  cat > "$stamp.r7rs" <<'EOS'
(require 'r7rsmode)
(call-with-error-handler
 (lambda args
   (decode-error args)
   (exit 1))
 larceny:compile-r7rs-runtime)
(exit)
EOS
  if ! ./larceny -- "$stamp.r7rs" > bootstrap-r7rs.log 2>&1 < /dev/null; then
    grep -A10 "^Error" bootstrap-r7rs.log >&2
    echo "$0: compiling the libraries failed; see bootstrap-r7rs.log" >&2
    exit 1
  fi

  # A smoke test of --r6rs mode, which reads libraries with strict R6RS
  # syntax (the test suites run in --r7rs mode).
  echo "Checking --r6rs mode..."
  cat > "$stamp.sps" <<'EOS'
(import (rnrs)
        (rnrs eval)
        (rnrs mutable-pairs)
        (rnrs mutable-strings)
        (rnrs r5rs))
(define f
  (case-lambda
   ((x) (list 'one x))
   ((x . rest) (list 'more x rest))))
(display (list (f 1) (f 1 2) (eval '(+ 1 2) (environment '(rnrs)))))
(newline)
EOS
  r6=`./larceny --r6rs --program "$stamp.sps" < /dev/null 2>&1`
  if [ "`echo "$r6" | tail -1`" != "((one 1) (more 1 (2)) 3)" ]; then
    echo "$r6" | tail -20 >&2
    echo "$0: an R6RS program failed in --r6rs mode" >&2
    exit 1
  fi
fi
echo "Done.  Run ./larceny"
