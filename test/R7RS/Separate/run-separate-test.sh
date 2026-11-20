#!/bin/sh
# Checks that libraries compiled in different runs of Larceny can be
# imported together.  The expander names the environments it saves in
# compiled libraries with a token for the run and a counter, and when
# two runs used the same token (a heap dumped after the expander had
# run kept it, and plain runs within one second shared it), (t a) and
# (t b) below got the same names, and compiling (t c), which imports
# both, failed with "No binding available for rb in library (t b)".
#
# Run from the Larceny root directory, with the Larceny to test
# (LARCENY, default ./larceny).  The test is run with that Larceny and
# with a heap dumped from it.

LARCENY=${LARCENY:-./larceny}
root=`pwd`
tmp=`mktemp -d "${TMPDIR:-/tmp}/larceny-separate.XXXXXX"`
trap 'rm -rf "$tmp"' EXIT
cp -r test/R7RS/Separate/t "$tmp"
cd "$tmp" || exit 1

# A program that compiles the libraries named by its arguments.
cat > compile.sps <<'SCHEME'
(import (scheme base) (scheme write) (scheme process-context)
        (larceny compiler))
(for-each (lambda (name)
            (compile-library (string-append "t/" name ".sld")
                             (string-append "t/" name ".slfasl")))
          (cdr (command-line)))
(display "compiled") (newline)
SCHEME

# A heap whose entry point runs a program, as -program does.
cat > dump.scm <<SCHEME
(import (scheme base) (scheme process-context) (larceny dump-heap)
        (larceny compiler)
        (primitives current-require-path command-line-arguments
                    run-r6rs-program))
(scheme-dump-heap "$tmp/test.heap"
  (lambda (argv)
    (current-require-path (cons "$tmp" (current-require-path)))
    (command-line-arguments (list->vector (cdr (vector->list argv))))
    (run-r6rs-program (vector-ref argv 0))
    (exit 0)))
SCHEME
LARCENY_ROOT="$root" timeout 300 "$root/$LARCENY" -r7rs < dump.scm > dump.log 2>&1
grep -q '^; Done\.' dump.log || { cat dump.log; echo "separate compilation: dumping the heap failed"; exit 1; }

# run-with KIND LIBRARY...: compiles the libraries in a run of KIND.
run_with () {
  kind=$1; shift
  case $kind in
    plain) LARCENY_ROOT="$root" timeout 120 "$root/$LARCENY" -r7rs -I "$tmp" -program compile.sps -- "$@" ;;
    dumped) LARCENY_ROOT="$root" timeout 120 "$root/larceny.bin" -heap test.heap -- compile.sps "$@" ;;
  esac < /dev/null > run.log 2>&1
  if grep -q '^compiled$' run.log && ! grep -q 'Entering debugger' run.log; then
    return 0
  fi
  cat run.log
  echo "separate compilation ($kind): compiling $* failed"
  exit 1
}

for kind in plain dumped; do
  rm -f t/*.slfasl
  run_with $kind a b
  run_with $kind b
  run_with $kind c
done
echo "separate compilation: OK"
