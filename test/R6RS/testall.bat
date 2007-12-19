

call ..\..\larceny -r6rs < grabbag.sch
call ..\..\larceny -r6rs -program grabbag.sch
call ..\..\larceny -r6rs -program cpoint.sch
call ..\..\larceny -r6rs -program cpointR6.sch

call ..\..\larceny -- -e "(exit)"
call ..\..\larceny -r6rs -program nothing.sch
call ..\..\larceny -r6rs -program nothingBig.sch

call ..\..\larceny -r6rs -program hello.sch
call ..\..\larceny -r6rs -program fib.sch     < fib.input
call ..\..\larceny -r6rs -program earley.sch  < earley.input
call ..\..\larceny -r6rs -program nboyer.sch  < nboyer.input
