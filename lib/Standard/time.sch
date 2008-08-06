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
(require 'foreign-ctools)
(require 'foreign-cstructs)
(require 'foreign-cenums)

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
)
 (win32 

  (define current-utc-time 'set-below)
  (define timezone-offset 'set-below)

  (let ()
    (define-c-struct ("SYSTEMTIME" make-systemtime (include<> "Windows.h"))
      ("wYear"   (systemtime-year)   (systemtime-set-year!))
      ("wMonth"  (systemtime-month)  (systemtime-set-month!))
      ("wDayOfWeek" (systemtime-day-of-week))
      ("wDay"    (systemtime-day)    (systemtime-set-day!))
      ("wHour"   (systemtime-hour)   (systemtime-set-hour!))
      ("wMinute" (systemtime-minute) (systemtime-set-minute!))
      ("wSecond" (systemtime-second) (systemtime-set-second!))
      ("wMilliseconds" 
       (systemtime-milliseconds) (systemtime-set-milliseconds!)))

    ;; 100-nanosecond intervals since January 1, 1601.
    (define-c-struct
      ("FILETIME" make-filetime (include<> "Windows.h"))
      ("dwLowDateTime" (filetime-low) (filetime-set-low!))
      ("dwHighDateTime" (filetime-high) (filetime-set-high!)))

    (define get-system-time!
      (foreign-procedure "GetSystemTime" 
                         '(boxed) 'void 'stdcall))
    (define system-time-to-file-time!
      (foreign-procedure "SystemTimeToFileTime" 
                         '(boxed boxed) 'bool 'stdcall))
    (define file-time-to-system-time!
      (foreign-procedure "FileTimeToSystemTime" 
                         '(boxed boxed) 'bool 'stdcall))
    (define get-system-time-as-file-time!
      (foreign-procedure "GetSystemTimeAsFileTime"
                         '(boxed) 'void 'stdcall))

    (define system-time-to-tz-specific-local-time
      (foreign-procedure "SystemTimeToTzSpecificLocalTime"
                         '(boxed boxed boxed) 'bool 'stdcall))

    (define unix-epoch-systemtime
      (let ((epoch (make-systemtime)))
        (systemtime-set-year!  epoch 1970)
        (systemtime-set-month! epoch    1)
        (systemtime-set-day!   epoch    1)
        epoch))
    (define unix-epoch-filetime
      (let ((ft (make-filetime)))
        (system-time-to-file-time! unix-epoch-systemtime ft)
        ft))
    
    (define (filetime->100nanosecond-interval-count ft)
      (+ (filetime-low ft)
         (bitwise-arithmetic-shift-left (filetime-high ft) 
                                        32)))
    (define unix-epoch-100nanosecond-interval-count
      (filetime->100nanosecond-interval-count unix-epoch-filetime))


    (define convert-100nanoseconds->seconds
      (lambda (count)
        (quotient count  10000000)))
      
    (define convert-100nanoseconds->seconds+milliseconds
      (lambda (count)
        (values (quotient count  10000000)
                (quotient (remainder count 10000000) 10))))

    (define my-current-utc-time 
      (lambda ()
        (let ((filetime-bv (make-filetime)))
          (get-system-time-as-file-time! filetime-bv)
          (let ((count-since-unix-epoch
                 (- (filetime->100nanosecond-interval-count filetime-bv)
                    unix-epoch-100nanosecond-interval-count)))
            (convert-100nanoseconds->seconds+milliseconds 
             count-since-unix-epoch)))))

    (define-c-struct 
      ("TIME_ZONE_INFORMATION" make-tz-info (include<> "Windows.h"))
      ("Bias" (tz-info-bias)))
        
    (define get-time-zone-information!
      (let ((p (foreign-procedure "GetTimeZoneInformation" '(boxed) 'int 
                                  'stdcall)))
        (define-c-info (include<> "Windows.h")
          (const time-zone-id-unknown  int "TIME_ZONE_ID_UNKNOWN")
          (const time-zone-id-standard int "TIME_ZONE_ID_STANDARD")
          (const time-zone-id-daylight int "TIME_ZONE_ID_DAYLIGHT"))
        (lambda (t)
          (let ((rtn (p t)))
            (cond 
             ((= rtn time-zone-id-unknown)  'unknown)
             ((= rtn time-zone-id-standard) 'standard)
             ((= rtn time-zone-id-daylight) 'daylight))))))
                  
    (define my-timezone-offset
      (lambda (seconds-since-unix-epoch)
        (let ((cnanos (+ unix-epoch-100nanosecond-interval-count
                         (* 10000000 seconds-since-unix-epoch))))
          (let ((hi (bitwise-arithmetic-shift-right cnanos 32))
                (lo (integer-logand cnanos #xFFFFFFFF))
                (ft1 (make-filetime))
                (sys1 (make-systemtime))
                (sys2 (make-systemtime))
                (ft2 (make-filetime)))
            (filetime-set-high! ft1 hi)
            (filetime-set-low!  ft1 lo)
            (file-time-to-system-time! ft1 sys1)
            (system-time-to-tz-specific-local-time #f sys1 sys2)
            (system-time-to-file-time! sys2 ft2)
            
            (- (convert-100nanoseconds->seconds
                (filetime->100nanosecond-interval-count ft2))
               (convert-100nanoseconds->seconds
                (filetime->100nanosecond-interval-count ft1)))))))
    
    (set! current-utc-time my-current-utc-time)
    (set! timezone-offset my-timezone-offset)

    )))


