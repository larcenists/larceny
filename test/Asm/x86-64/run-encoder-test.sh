#!/bin/sh
# Checks the x86-64 encoder in src/Asm/Fence/pass5p2-x86-64.sch against
# GNU as and objdump.  Run from the Larceny root directory, with a
# Larceny to load the test (any host Larceny will do; LARCENY, default
# ./larceny).

set -e
LARCENY=${LARCENY:-./larceny}
tmp=`mktemp -d "${TMPDIR:-/tmp}/larceny-x86-64-encoder.XXXXXX"`
trap 'rm -rf "$tmp"' EXIT

echo '(load "test/Asm/x86-64/encoder-test.sch") (exit)' > "$tmp/run.sch"
"$LARCENY" -- "$tmp/run.sch" < /dev/null > "$tmp/log" 2>&1 || { cat "$tmp/log"; exit 1; }
grep -q "cases written" "$tmp/log" || { cat "$tmp/log"; exit 1; }
mv encoder-test-expected.s encoder-test-actual.s "$tmp"

# The instructions of each section, without addresses and bytes (and
# without the comments objdump adds to RIP-relative operands).
disassemble () {
  as --64 -o "$1.o" "$1.s"
  objdump -d -M intel -w "$1.o" |
    awk -F'\t' '/^Disassembly of section/ {print; next}
                NF >= 3 {sub(/ *#.*/, "", $3); gsub(/ +/, " ", $3); print $3}'
}

disassemble "$tmp/encoder-test-expected" > "$tmp/expected"
disassemble "$tmp/encoder-test-actual" > "$tmp/actual"

if cmp -s "$tmp/expected" "$tmp/actual"; then
  echo "x86-64 encoder: $(grep -c . "$tmp/actual") lines OK"
else
  diff "$tmp/expected" "$tmp/actual" | head -60
  exit 1
fi
