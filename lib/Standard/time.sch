; Simple interface to the system clock.
; 2004-01-11 / lth
;
; The epoch (time = 0) is 1 January 1970 00:00:00 UTC.
;
; (current-utc-time) => [seconds microseconds] 
;    Returns the current time, as an offset from the epoch.
;
; (timezone-offset seconds) => seconds'
;    Given a time in seconds relative to the epoch, returns the offset 
;    of local time from UTC at that time expressed in seconds, with
;    positive offsets to the east of Greenwich.

(require 'srfi-0)
(require 'std-ffi)

(cond-expand 
 ((and larceny unix)

  ;; On Linux the epoch is 1/1/1970 00:00:00 UTC, which is right for
  ;; the date package.  I've no idea if the use of UTC is consistent
  ;; across Unix dialects.
  ;;
  ;; tm:local-tz-offset must return positive values east of Greenwich.
  ;; Unix normally reports negative values east of Greenwitch.

  (define current-utc-time
    (let ((_gettimeofday 
	   (foreign-procedure "gettimeofday" '(boxed boxed) 'int)))
      (lambda ()
	(let ((tv (make-bytevector 8)))
	  (_gettimeofday tv #f)
	  (values (%get32 tv 0) (%get32 tv 4))))))

  (define timezone-offset
    (let ((_localtime 
           (foreign-procedure "localtime" '(boxed) 'uint))
          (_gmtime 
           (foreign-procedure "gmtime"    '(boxed) 'uint))
          (_mktime
           ;; mktime takes a pointer to storage owned by the C runtime, aka a uint.
           ;; (We need to fix our FFI interface to express this directly)
           (foreign-procedure "mktime"    '(uint) 'uint))
          (_difftime
           (foreign-procedure "difftime"  '(uint uint) 'double)))
      (lambda (t)
	(let ((tv (make-bytevector 4)))
          (%set32 tv 0 t)
          ;; A trick from the Apache Portable Runtime: mktime is inverse of localtime,
          ;; so mktime(gmtime(now)) - mktime(localtime(now)) _should_ be offset from utc.
          (let ((gmean-tm (_gmtime tv)))
            ;; set tm->tm_isdst to 0; GMT does not have DST.
            (%poke32 (+ gmean-tm 32) 0)
            (inexact->exact (ceiling (_difftime t (_mktime gmean-tm)))))))))
))

