#!/bin/sh
#
# Wrapper around the assembler that runs the preprocessor
# Use this if your 'as' is the GNU assembler or other assembler
# that does not run cpp.

if [ $# -ne 3 -o "$1" -ne "-o" ]; then
  echo "Usage: $0 -o output-file input-file"
  exit 1
fi
gcc -E -ISparc -IBuild - < $3 >  /tmp/larceny$$
gas  -o $2 /tmp/larceny$$
rm -f /tmp/larceny$$
