#!/bin/sh
# Bootstrap native Larceny for AArch64 (macOS on Apple Silicon) from
# source, using Racket to host the build.  Run it from the root of a
# Larceny tree; see doc/HOWTO-BUILD.
#
#   src/Build/bootstrap-arm64-native.sh [--clean] [path-to-plt-r5rs]
#
# This is src/Build/bootstrap.sh --native --racket; see that script for
# more options (Petit, a Larceny host, compiling the R7RS libraries).

CLEAN=
if [ "$1" = "--clean" ]; then
  CLEAN=--clean
  shift
fi
exec "`dirname "$0"`/bootstrap.sh" --native $CLEAN --racket ${1:+"$1"}
