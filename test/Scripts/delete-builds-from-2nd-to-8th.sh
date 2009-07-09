pushd ~/larcenytest
ls | egrep ^larceny-.*\(0[2-8]\)$ | xargs rm -rvf
ls | egrep ^larceny-.*\(0[2-8]\).tar.gz$ | xargs rm -rvf
popd
