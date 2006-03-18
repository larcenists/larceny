;;
;; This is designed to be called from a custom loader :-p
;; Calling convention:
;;   Arguments in EAX, EBX, ECX, EDX, ESI, EDI
;;   Tail call with jmp; non-tail call with funcall
;;   Return with funret
;;   Use ESP at your leisure
;;
(sassy-make-bin "test.i"
                (sassy
                  '((macro save
                           (lambda (size)
                             `(sub esp ,(* 4 size))))
                    (macro unsave
                           (lambda (size)
                             `(add esp ,(* 4 size))))
                    (macro loc
                           (lambda (n)
                             `(& esp ,(* 4 n))))
                    (macro funcall
                           (lambda (f)
                             `(begin
                                (push ebp)
                                (mov ebp esp)
                                (sub ebp 4)
                                (call ,f)
                                (pop ebp)
                                )))
                    (macro funret
                           (lambda ()
                             '(begin
                                (mov esp ebp)
                                (ret))))
                    (macro syscall
                           (lambda (which)
                             `(begin
                                (mov eax ,which)
                                (int #x80))))
                    (macro c-arg
                           (lambda (n)
                             `(& ebp ,(* 4 (+ 2 n)))))

                    (macro sys_exit      1)
                    (macro sys_fork      2)
                    (macro sys_read      3)
                    (macro sys_write     4)
                    (macro sys_open      5)
                    (macro sys_close     6)
                    (macro sys_waitpid   7)
                    (macro sys_creat     8)
                    (macro sys_link      9)
                    (macro sys_unlink   10)
                    (macro sys_execve   11)
                    (macro sys_chdir    12)
                    (macro sys_time     14)

                    (data)

                    (text
                      (label main
                        (push ebp)
                        (mov ebp esp)

                        (push ebx)
                        (push esi)
                        (push edi)

                        (mov ecx (c-arg 1))

                        (leap
                          (iter
                            (begin
                              (push ecx)
                              (funcall putstr)
                              (mov eax #\space)
                              (funcall putchar)
                              (pop ecx)
                              (mark
                                (add ecx 4))
                              (mov eax (& ecx))
                              (!= eax 0))))

                        (mov eax #\newline)
                        (funcall putchar)

                        (pop edi)
                        (pop esi)
                        (pop ebx)

                        (xor eax eax)

                        (mov esp ebp)
                        (pop ebp)
                        (ret))

                      (label putstr
                        (mov ecx eax)
                        (leap
                          (iter
                            (begin
                              (push ecx)
                              (funcall putchar)
                              (pop ecx)
                              (inc ecx)
                              (mark
                                (xor eax eax))
                              (mov al (& ecx))
                              (!= al 0))))
                        (funret))
                      
                      ;; putchar is a dynamic leaf procedure
                      (label putchar
                        (push eax)
                        (xor ebx ebx)
                        (mov ecx esp)
                        (mov edx 1)
                        (syscall sys_write)
                        (funret)
                        )))))

