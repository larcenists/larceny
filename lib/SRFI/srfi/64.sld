;;; SRFI 64: a Scheme API for test suites
;;;
;;; $Id$
;;;

;; Copyright (c) 2005, 2006 Per Bothner
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (srfi 64 testing)

  (export 
   test-begin ;; must be listed first, since in Kawa (at least) it is "magic".
   test-end test-assert test-eqv test-eq test-equal
   test-approximate test-error test-apply test-with-runner
   test-match-nth test-match-all test-match-any test-match-name
   test-skip test-expect-fail test-read-eval-string
   test-runner-group-path test-group test-group-with-cleanup
   test-result-ref test-result-set! test-result-clear test-result-remove
   test-result-kind test-passed?
   test-log-to-file
   ; Misc test-runner functions
   test-runner? test-runner-reset test-runner-null
   test-runner-simple test-runner-current test-runner-factory test-runner-get
   test-runner-create test-runner-test-name
   ;; test-runner field setter and getter functions - see %test-record-define:
   test-runner-pass-count test-runner-pass-count!
   test-runner-fail-count test-runner-fail-count!
   test-runner-xpass-count test-runner-xpass-count!
   test-runner-xfail-count test-runner-xfail-count!
   test-runner-skip-count test-runner-skip-count!
   test-runner-group-stack test-runner-group-stack!
   test-runner-on-test-begin test-runner-on-test-begin!
   test-runner-on-test-end test-runner-on-test-end!
   test-runner-on-group-begin test-runner-on-group-begin!
   test-runner-on-group-end test-runner-on-group-end!
   test-runner-on-final test-runner-on-final!
   test-runner-on-bad-count test-runner-on-bad-count!
   test-runner-on-bad-end-name test-runner-on-bad-end-name!
   test-result-alist test-result-alist!
   test-runner-aux-value test-runner-aux-value!
   ;; default/simple call-back functions, used in default test-runner,
   ;; but can be called to construct more complex ones.
   test-on-group-begin-simple test-on-group-end-simple
   test-on-bad-count-simple test-on-bad-end-name-simple
   test-on-final-simple test-on-test-end-simple)

  (import (except (scheme base) output-port?)
          (rename (only (scheme base) output-port-open?)
                  (output-port-open? output-port?))
          (only (rnrs io ports)
                open-file-output-port file-options no-fail native-transcoder)
          (scheme read)
          (scheme write)
          (scheme complex)
          (scheme eval))

  (include "64.body.scm"))


(define-library (srfi 64)

  (export 
   test-begin ;; must be listed first, since in Kawa (at least) it is "magic".
   test-end test-assert test-eqv test-eq test-equal
   test-approximate test-error test-apply test-with-runner
   test-match-nth test-match-all test-match-any test-match-name
   test-skip test-expect-fail test-read-eval-string
   test-runner-group-path test-group test-group-with-cleanup
   test-result-ref test-result-set! test-result-clear test-result-remove
   test-result-kind test-passed?
   test-log-to-file
   ; Misc test-runner functions
   test-runner? test-runner-reset test-runner-null
   test-runner-simple test-runner-current test-runner-factory test-runner-get
   test-runner-create test-runner-test-name
   ;; test-runner field setter and getter functions - see %test-record-define:
   test-runner-pass-count test-runner-pass-count!
   test-runner-fail-count test-runner-fail-count!
   test-runner-xpass-count test-runner-xpass-count!
   test-runner-xfail-count test-runner-xfail-count!
   test-runner-skip-count test-runner-skip-count!
   test-runner-group-stack test-runner-group-stack!
   test-runner-on-test-begin test-runner-on-test-begin!
   test-runner-on-test-end test-runner-on-test-end!
   test-runner-on-group-begin test-runner-on-group-begin!
   test-runner-on-group-end test-runner-on-group-end!
   test-runner-on-final test-runner-on-final!
   test-runner-on-bad-count test-runner-on-bad-count!
   test-runner-on-bad-end-name test-runner-on-bad-end-name!
   test-result-alist test-result-alist!
   test-runner-aux-value test-runner-aux-value!
   ;; default/simple call-back functions, used in default test-runner,
   ;; but can be called to construct more complex ones.
   test-on-group-begin-simple test-on-group-end-simple
   test-on-bad-count-simple test-on-bad-end-name-simple
   test-on-final-simple test-on-test-end-simple)

  (import (srfi 64 testing)))

; eof
