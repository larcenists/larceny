pushd ~/larcenytest
ls | egrep ^larceny-.*\(0[2-8]\)$ | xargs rm -rvf
popd
