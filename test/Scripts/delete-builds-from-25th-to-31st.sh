pushd ~/larcenytest
ls | egrep ^larceny-.*\(2[5-9]\|3[0-1]\)$ | xargs rm -rvf
ls | egrep ^larceny-.*\(2[5-9]\|3[0-1]\).tar.gz$ | xargs rm -rvf
popd
