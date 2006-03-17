;;
;; This is designed to be called from a custom loader :-p
;;
(sassy-make-bin "test.i"
                (sassy
                  '((entry start)
                    (macro enter
                           (begin (push ebp)
                                  (mov ebp esp)))
                    (macro leave
                           (begin (mov esp ebp)
                                  (pop ebp)))
                    (macro defun
                           (lambda (name . body)
                             `(label ,name
                                     enter
                                     ,@body
                                     leave
                                     (ret))))

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
                           (lambda (name . tail)
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
                                  (push ,(car params))
                                  (push ,(cadr params))
                                  ,@(if (null? tail)
                                     `((call _syscall))
                                     `((push ,(car tail))
                                       (jmp _syscall)))))))

                    (data)

                    (text
                      (defun main
                        (push ebx)
                        (push esi)
                        (push edi)

                        (save 2)

                        (push #\a)
                        (call putchar)

                        (mov ecx (arg 1))
                        (mov ecx (& ecx))

                        (xor eax eax)
                        (mov al (& ecx))
                        (push eax)
                        (call putchar)


                        (mov al (& 1 ecx))
                        (push eax)
                        (call putchar)

                        (push ecx)
                        (call putstr)

                        (push 0)
                        (syscall exit)

                        (leap
                          (iter
                            (begin
                              (mov (loc 1) ecx)
                              (mov (loc 0) eax)
                              (call putstr)
                              (push #\space)
                              (call putchar)
                              (mov ecx (loc 1))
                              (mark
                                (add ecx 4))
                              (mov eax (& ecx))
                              (!= eax 0))))

                        (push #\newline)
                        (call putchar)

                        (unsave 2)

                        (pop edi)
                        (pop esi)
                        (pop ebx)
                        (xor eax eax))

                      (label _syscall
                        (push ebp)
                        (mov ebp esp)
                        (mov eax (& 8 ebp)) ; number of arguments
                        (add esp 16)
                        (seq
                          (>= eax 1) 
                          (pop ebx)
                          (>= eax 2)
                          (pop ecx)
                          (>= eax 3)
                          (pop edx)
                          (>= eax 4)
                          (pop esi)
                          (>= eax 5)
                          (pop edi))
                        (mov eax (& 12 ebp)) ; first argument
                        (int #x80)
                        (mov ebx (& 4 ebp))
                        (mov ebp (& ebp))
                        (push ebx)
                        (ret))

                      (label putstr
                        (push ebp)
                        (mov ebp esp)
                        (mov ecx (arg 0))
                        (leap
                          (iter
                            (begin
                              (push ecx)
                              (push eax)
                              (call putchar)
                              (pop ecx)
                              (inc ecx)
                              (mark
                                (xor eax eax))
                              (mov al (& ecx))
                              (!= al 0))))
                        (mov esp ebp)
                        (pop ebp)
                        (ret))
                      
                      ;; putchar is a dynamic leaf procedure
                      (label putchar
                        (pop edx)
                        (mov eax esp)
                        (add esp 4)
                        (push 1)
                        (push eax)
                        (push 0)
                        (syscall write edx)

                        )))))

