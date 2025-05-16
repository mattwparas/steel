(provide walk-files
         file->string)
(define (for-each func lst)
  (if (null? lst)
      void
      (begin
        (func (car lst))
        (when (null? lst)
          (return! void))
        (for-each func (cdr lst)))))

(define (file->string path)
  (let ([file (open-input-file path)]) (read-port-to-string file)))

(define (consume-iter iter func)
  (define next (read-dir-iter-next! iter))
  (when next
    (if (read-dir-entry-is-dir? next)
        (begin
          (consume-iter (read-dir-iter (read-dir-entry-path next)) func)
          (consume-iter iter func))
        (begin
          ; (vector-push! files (read-dir-entry-path next))
          (func (read-dir-entry-path next))
          (consume-iter iter func)))))

;; Walk the file system, applying a function to each file found
(define (walk-files cr func)
  (consume-iter (read-dir-iter cr) func))
