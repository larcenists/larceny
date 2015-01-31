(define-library (scheme file)

  (export

   call-with-input-file
   call-with-output-file
   delete-file
   file-exists?
   open-binary-input-file
   open-binary-output-file
   open-input-file
   open-output-file
   with-input-from-file
   with-output-to-file
   )

  (import

   (only (rnrs io simple)
         call-with-input-file
         call-with-output-file
         open-input-file
         open-output-file
         with-input-from-file
         with-output-to-file)

   (rnrs files)

   (only (larceny r7rs primitives)
         open-binary-input-file
         open-binary-output-file)))

