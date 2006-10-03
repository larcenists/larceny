; Note, because the web server currently just returns raw content
; without headers or anything, make sure your response does not look
; like a response header.  It ought not start with any digits.

(register-cgi 
 "/eval" 
 (lambda (req out) 
   (let ((req (read (open-input-string (decode-url req))))) 
     (display "; " out)
     (display/crlf req out)
     (display "=> " out) 
     (display/crlf (eval req) out))))

(register-cgi "/echo" display/crlf)
