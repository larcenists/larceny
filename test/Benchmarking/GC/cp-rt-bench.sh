function f() {
    LARCENY="../../../larceny $@" sh bench-gc.crossplat.sh
}

f -stopcopy
f -gen -size1 4M -size2 8M
f -rrof -size0 4M -size1 8M -sumzbudget 2 -sumzcoverage 2 -sumzretries 1 -popularity 8 -infamy 1 -refinement 1.0
# -rrof -size0 4M -size1 8M -sumzbudget 2 -sumzcoverage 2 -sumzretries 1 -popularity 8 -infamy 1 -refinement 1.0 -rhashrep 
f -gen -size1 1M -size2 8M
f -rrof -size0 1M -size1 8M -sumzbudget 2 -sumzcoverage 2 -sumzretries 1 -popularity 8 -infamy 1 -refinement 1.0
f -rrof -size0 1M -size1 4M -sumzbudget 2 -sumzcoverage 2 -sumzretries 1 -popularity 8 -infamy 1 -refinement 1.0
