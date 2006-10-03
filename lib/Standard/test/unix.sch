(require 'unix)

(define (backtick command)
  (let ((x (process (string-append "exec " command))))
    (let ((in (car x))) 
      (do ((c (read-char in) (read-char in))
           (l '() (cons c l)))
          ((eof-object? c)
           (unix/wait)
           (list->string (reverse l)))))))
    
(define (test)
  (backtick "/bin/ls"))



