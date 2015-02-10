(write-string
 "WHEN I wrote the following pages, or rather the bulk of them, ")

(string-for-each write-char
                 "I lived alone, in the woods, a mile from any ")

(for-each write-char
          (string->list
           "neighbor, in a house which I had built myself, on "))

;;; the shore of Walden Pond, in Concord, Massachusetts, and earned 

(write-string
 (utf8->string
  '#u8(116 104 101 32 115 104 111 114 101 32 111 102 32 87 97
       108 100 101 110 32 80 111 110 100 44 32 105 110 32 67 111
       110 99 111 114 100 44 32 77 97 115 115 97 99 104 117 115
       101 116 116 115 44 32 97 110 100 32 101 97 114 110 101 100 32)))

(let ((p (open-input-string "my living by the labor of my hands only.")))
  (define (loop)
    (let ((c (read-char p)))
      (if (char? c)
          (begin (write-char c)
                 (loop))
          (flush-output-port (current-output-port)))))
  (loop))
