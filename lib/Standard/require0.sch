; Copy this into your .larceny to have REQUIRE available without having
; to load REQUIRE and go through its initialization.

(define require 
  (lambda args
    (load "/home/lth/lib/larceny/schemelib/require.sch")
    (apply require args)))

(define require-location
  (lambda args
    (load "/home/lth/lib/larceny/schemelib/require.sch")
    (apply require-location args)))

; eof
