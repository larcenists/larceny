#!/bin/sh
# Checks the AArch64 encoder in src/Asm/Fence/pass5p2-arm64.sch against
# the system assembler.  Run from the Larceny root directory, with a
# working ./larceny (any host Larceny will do).

set -e
tmp=${TMPDIR:-/tmp}/larceny-arm64-encoder.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

echo '(load "test/Asm/arm64/encoder-test.sch") (exit)' | ./larceny > "$tmp/log" 2>&1 || { cat "$tmp/log"; exit 1; }
mv encoder-test.s encoder-test.hex "$tmp"

cc -arch arm64 -c "$tmp/encoder-test.s" -o "$tmp/encoder-test.o"
objdump -d "$tmp/encoder-test.o" |
  awk '/^ *[0-9a-f]+:[ \t]+[0-9a-f]+[ \t]/{print $2}' > "$tmp/expected"
cut -c1-8 "$tmp/encoder-test.hex" > "$tmp/actual"

if cmp -s "$tmp/expected" "$tmp/actual"; then
  echo "arm64 encoder: $(wc -l < "$tmp/actual" | tr -d ' ') instructions OK"
else
  paste "$tmp/expected" "$tmp/encoder-test.hex" | awk -F'\t' '$1 != substr($2,1,8) {print "MISMATCH: expected", $1, "got", $2}'
  wc -l "$tmp/expected" "$tmp/actual"
  exit 1
fi
