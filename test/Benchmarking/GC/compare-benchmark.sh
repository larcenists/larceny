FILE=$1

function c () {
    CCMD="../../../larceny -stopcopy -nobanner -- cp-run-benchmark.sch prefix-larceny.scm num-iters.scm $FILE -e (dump-bmark-heap) -e (exit)"
    $CCMD > /dev/null
}

function f () {
    RCMD="../../../larceny $@ -nobanner -heap bmark.heap -- -e (main) -e (exit) "
    DCMD="../../../larceny -stopcopy -nobanner -- cp-run-benchmark.sch prefix-larceny.scm num-iters.scm $FILE -e '(dump-bmark-heap)' -e '(exit)' ; \
          ../../../larceny $@ -nobanner -heap bmark.heap -- -e '(main)' -e '(exit)'"
    echo $DCMD
    $RCMD
}

c
# -stopcopy -size0 4M
# -stopcopy -size0 8M
f -stopcopy
f -gen -size1 4M -size2 8M
f -rrof -size0 4M -size1 8M -sumzbudget 2 -sumzcoverage 2 -sumzretries 1 -popularity 8 -infamy 1 -refinement 1.0
# -rrof -size0 4M -size1 8M -sumzbudget 2 -sumzcoverage 2 -sumzretries 1 -popularity 8 -infamy 1 -refinement 1.0 -rhashrep 
f -gen -size1 1M -size2 8M
f -rrof -size0 1M -size1 8M -sumzbudget 2 -sumzcoverage 2 -sumzretries 1 -popularity 8 -infamy 1 -refinement 1.0
f -rrof -size0 1M -size1 4M -sumzbudget 2 -sumzcoverage 2 -sumzretries 1 -popularity 8 -infamy 1 -refinement 1.0
# -rrof -size0 1M -size1 4M -sumzbudget 2 -sumzcoverage 2 -sumzretries 1 -popularity 8 -infamy 1 -refinement 1.0 -rhashrep 
