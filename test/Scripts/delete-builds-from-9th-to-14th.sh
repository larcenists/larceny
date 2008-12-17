pushd ~/larcenytest
ls | egrep ^larceny-.*\(09\|1[0-4]\)$ | xargs rm -rvf
ls | egrep ^larceny-.*\(09\|1[0-4]\).tar.gz$ | xargs rm -rvf
popd
