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
                    (macro defun
                           (lambda (name . body)
                             `(label ,name
                                     prelude
                                     ,@body
                                     postlude)))

                    (macro save
                           (lambda (size)
                             `(sub esp ,(* 4 size))))
                    (macro unsave
                           (lambda (size)
                             `(add esp ,(* 4 size))))
                    (macro arg
                           (lambda (n)
                             `(& ebp ,(* 4 (+ 2 n)))))
                    (macro loc
                           (lambda (n)
                             `(& esp ,(* 4 n))))

                    (macro syscall
                           (lambda (name)
                             (let ((params
                                     (case name
                                       ((exit)      '(1 1))
                                       ((fork)      '(2 0))
                                       ((read)      '(3 3))
                                       ((write)     '(4 3))
                                       ((open)      '(5 3))
                                       ((close)     '(6 1))
                                       ((waitpid)   '(7 3))
                                       ((creat)     '(8 2))
                                       ((link)      '(9 2))
                                       ((unlink)   '(10 1))
                                       ((execve)   '(11 1))
                                       ((chdir)    '(12 1))
                                       ((time)     '(14 1))
                                       (else (error "unknown syscall")))))
                               `(begin
                                  (save 2)
                                  (mov (loc 1) ,(car params))
                                  (mov (loc 0) ,(cadr params))
                                  (call _syscall)
                                  (unsave 2)
                                  ))))

                    (data)

                    (text
                      (defun main
                        (push ebx)

                        (save 2)

                        (mov (loc 0) 0)
                        (syscall fork)

                        (mov ecx (arg 1))

                        (leap
                          (iter
                            (begin
                              (mov (loc 1) ecx)
                              (mov (loc 0) eax)
                              (call putstr)
                              (mov (loc 0) #\space)
                              (call putchar)
                              (mov ecx (loc 1))
                              (mark
                                (add ecx 4))
                              (mov eax (& ecx))
                              (!= eax 0))))

                        (mov (loc 0) #\newline)
                        (call putchar)

                        (mov (loc 0) 0)
                        (syscall exit)

                        (unsave 2)

                        (pop ebx)
                        (xor eax eax))

                      (defun _syscall
                        (push ebx)
                        (push esi)
                        (push edi)
                        (mov eax (arg 0))
                        (seq
                          (>= eax 1) 
                          (mov ebx (arg 2))
                          (>= eax 2)
                          (mov ecx (arg 3))
                          (>= eax 3)
                          (mov edx (arg 4))
                          (>= eax 4)
                          (mov edx (arg 5))
                          (>= eax 5)
                          (mov edx (arg 6)))
                        (mov eax (arg 1))
                        (int #x80)
                        (pop edi)
                        (pop esi)
                        (pop ebx))

                      (defun putstr
                        (save 2)
                        (mov ecx (arg 0))
                        (leap
                          (iter
                            (begin
                              (mov (loc 1) ecx)
                              (mov (loc 0) eax)
                              (call putchar)
                              (mov ecx (loc 1))
                              (inc ecx)
                              (mark
                                (xor eax eax))
                              (mov al (& ecx))
                              (!= al 0))))
                        (unsave 2))
                      
                      (defun putchar
                        (mov eax (arg 0))
                        (push eax)
                        (mov eax esp)
                        (push 1)
                        (push eax)
                        (push 0)
                        (syscall write)
                        (pop eax)
                        (pop eax)
                        (pop eax))
                      ))))

