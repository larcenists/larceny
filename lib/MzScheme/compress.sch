;;; -*-Mode: Scheme; coding: iso-8859-1 -*-

;;;===============================================================================
;;;
;;; Simple Hygienic Macros and Simple Modules:
;;;
;;;   Copyright (c) 2005 André van Tonder
;;;
;;;   Permission is hereby granted, free of charge, to any person obtaining a
;;;   copy of this software and associated documentation files (the ``Software''),
;;;   to deal in the Software without restriction, including without limitation
;;;   the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;   and/or sell copies of the Software, and to permit persons to whom the
;;;   Software is furnished to do so, subject to the following conditions:
;;;
;;;   The above copyright notice and this permission notice shall be included in
;;;   all copies or substantial portions of the Software.
;;;
;;;   THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;;   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;   DEALINGS IN THE SOFTWARE.
;;;
;;;===============================================================================
($$trace "compress")

;; Simple-minded compression algorithm for environments to make
;; the expanded module size manageable

(define (compress-envs envs)

  ;; Compresses a list by eliminating most duplicate blocks
  ;; of the given size.  Since this will be imperfect at boundaries
  ;; of repeated sequences, blocksize should be small compared to
  ;; the length of the average repeated sequence, which in our case
  ;; is typically large - see for example SCHEME-ENV below.
  ;; The original list is assumed not to contain numbers.
  (define (compress ls blocksize count)
    (safe-split ls blocksize
                (lambda (block rest)
                  (if rest
                      (cons (cons count block)
                            (compress (replace block count rest)
                                      blocksize
                                      (+ count 1)))
                      ls))))

  (define (safe-split ls n k)
    (cond ((= n 0)
           (k '() ls))
          ((and (> n 0)
                (null? ls))
           (k ls #f))
          (else
           (safe-split (cdr ls) (- n 1)
                       (lambda (initial rest)
                         (k (cons (car ls) initial)
                            rest))))))

  (define (replace block count ls)
    (cond ((pair? ls) (let ((maybe-rest (compare-initial block ls)))
                        (if maybe-rest
                            (cons count (replace block count maybe-rest))
                            (cons (car ls)
                                  (replace block count (cdr ls))))))
          ((null? ls) '())
          (else (error "replace: improper list"))))

  (define (compare-initial block ls)
    (cond ((pair? block) (cond ((pair? ls) (and (equal? (car block)
                                                        (car ls))
                                                (compare-initial (cdr block)
                                                                 (cdr ls))))
                               ((null? ls) #f)
                               (else (error "Compare initial: improper list"))))
          ((null? block) ls)
          (else (error "Compare initial:  improper list"))))

  (define (filter-current-module envs)

    (define (filter predicate list)
      (define (loop scan accepted)
        (cond ((pair? scan) (loop (cdr scan)
                                  (if (predicate (car scan))
                                      (cons (car scan) accepted)
                                      accepted)))
              ((null? scan) (reverse! accepted))
              (else (error "filter: Improper list " list))))
      (loop list '()))

    (filter (let ((name (current-module-name)))
              (lambda (env)
                (eq? (cadr env) name)))
            envs))

  (define (alist-remove-duplicates alist)
    (define (memq item list)
      (cond ((pair? list) (if (eq? item (car list))
                              list
                              (memq item (cdr list))))
            ((null? list) #f)
            (else (error "memq: Improper list " list))))

    (define (rem alist already)
      (cond ((pair? alist) (if (memq (caar alist) already)
                               (rem (cdr alist) already)
                               (cons (car alist)
                                     (rem (cdr alist)
                                          (cons (caar alist)
                                                already)))))
            ((null? alist)               '())
            (else (error "alist-remove-duplicates: improper-list"))))
    (rem alist '()))

  ;; Flattens environments into a list that can be fed
  ;; to COMPRESS above.  #f is used a a separator.
  (define (flatten-envs envs)
    (cond ((pair? envs) (append (cons #f (car envs))
                                (flatten-envs (cdr envs))))
          ((null? envs) '())
          (else (error "flatten-envs: Improper list"))))

  (compress (flatten-envs (alist-remove-duplicates (filter-current-module envs)))
            ;; 5 is pretty much optimal blocksize.
            ;; 8 is not so bad and quite faster.
            ;; Increase for better time, worse space.
            ;; For matcher module below, compresses envs of 210K to 15K
            ;; which is a ratio of 14.
            8
            0))

(define (uncompress-envs compressed)
  (define (uncompress ls)
    (define (recur ls seen)
      (cond ((pair? ls) (cond ((number? (car ls))
                               (append (cdr (assv (car ls) seen))
                                       (recur (cdr ls) seen)))
                              ((and (pair? (car ls))
                                    (number? (caar ls)))
                               (let ((seen* (cons (cons (caar ls)
                                                        (recur (cdar ls) seen))
                                                  seen)))
                                 (append (recur (cdar ls) seen*)
                                         (recur (cdr ls) seen*))))
                              (else
                               (cons (car ls)
                                     (recur (cdr ls) seen)))))
            ((null? ls) '())
            (else (error "uncompress:  improper list"))))
    (recur ls '()))

  (define (unflatten-envs flattened)
    (cond ((pair? flattened) (if (eq? (car flattened) #f)
                                 (let loop ((renv '())
                                            (ls (cdr flattened)))
                                   (cond ((or (null? ls)
                                              (eq? (car ls) #f))
                                          (cons (reverse renv)
                                                (unflatten-envs ls)))
                                         (else
                                          (loop (cons (car ls) renv)
                                                (cdr ls)))))
                                 (error "Unflatten-envs")))
          ((null? flattened) '())
          (else (error "unflatten-envs: improper-list"))))

  (unflatten-envs (uncompress compressed)))
