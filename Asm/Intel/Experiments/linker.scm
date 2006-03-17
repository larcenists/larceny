;;
;; This is designed to be called from a custom loader :-p
;;
(sassy-make-bin "test.i"
                (sassy
                  '((entry start)
                    (macro prelude
                           (begin (push ebp)
                                  (mov ebp esp)))
                    (macro postlude
                           (begin (mov esp ebp)
                                  (pop ebp)
                                  (ret)))
                    (macro link
                           (begin
                             (locals (here)
                               (call here)
                               (label here)
                               (pop ebx)
                               (sub ebx here))
                             (add ebx start)
                             (mov eax (arg 3))
                             (mov (& ebx putchar) eax)
                             (mov eax (arg 4))
                             (mov (& ebx getchar) eax)))
                    (macro save-size
                           (lambda (size)
                             (* 4 (+ 1 size))))
                    (macro save
                           (lambda (size)
                             `(sub esp (save-size ,size))))
                    (macro unsave
                           (lambda (size)
                             `(add esp (save-size ,size))))
                    (macro arg
                           (lambda (n)
                             `(& ebp ,(* 4 (+ 2 n)))))
                    (macro loc
                           (lambda (n)
                             `(& esp ,(* 4 n))))
                    (macro defun
                           (lambda (name . body)
                             `(label ,name
                                     prelude
                                     ,@body
                                     postlude)))

                    (data
                      (label putchar (dwords 0))
                      (label getchar (dwords 0)))

                    (text
                      (defun main
                        (push ebx)
                        link

                        (save 2)

                        (mov ecx (arg 1))

                        (leap
                          (iter
                            (begin
                              (mov (loc 1) ecx)
                              (mov (loc 0) eax)
                              (call putstr)
                              (mov (loc 0) #\space)
                              (call (& ebx putchar))
                              (mov ecx (loc 1))
                              (mark
                                (add ecx 4))
                              (mov eax (& ecx))
                              (!= eax 0))))

                        (mov (loc 0) #\newline)
                        (call (& ebx putchar))

                        (unsave 2)

                        (pop ebx)
                        (xor eax eax))

                      (defun putstr
                        (save 2)
                        (mov ecx (arg 0))
                        (leap
                          (iter
                            (begin
                              (mov (loc 1) ecx)
                              (mov (loc 0) eax)
                              (call (& ebx putchar))
                              (mov ecx (loc 1))
                              (inc ecx)
                              (mark
                                (xor eax eax))
                              (mov al (& ecx))
                              (!= al 0))))
                        (unsave 2))))))

