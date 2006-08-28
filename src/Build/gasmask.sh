#!/bin/sh
#
# Wrapper around the assembler that runs the preprocessor
# Use this if your 'as' is the GNU assembler or other assembler
# that does not run cpp.

if [ $# -lt 3 -o "$1" -ne "-o" ]; then
  echo "Usage: $0 -o OUTPUT-FILE INPUT-FILE FLAGS..."
  exit 1
fi

shift
outfile="$1"
shift
infile="$1"
shift

gcc -E - "$@" < $infile > /tmp/larceny$$
gas -o $outfile /tmp/larceny$$
rm -f /tmp/larceny$$

