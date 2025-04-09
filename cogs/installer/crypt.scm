(require-builtin steel/hashes)

(provide walk-files
         hash-file
         hash-directory
         directories-equal?)

(define (for-each func lst)
  (if (null? lst)
      void
      (begin
        (func (car lst))
        (when (null? lst)
          (return! void))
        (for-each func (cdr lst)))))

;; Walk the file system, applying a function to each file found
(define (walk-files path func)
  (cond
    [(is-file? path) (func path)]
    [(is-dir? path) (for-each (lambda (x) (walk-files x func)) (read-dir path))]
    [else void]))

(define (hash-file-impl path #:hasher [hasher (md5-hasher)] #:buffer [buffer (bytevector)])
  (define input-file (open-input-file path))
  (define (loop)
    ;; Need to just read into a buffer instead?
    (define line (read-bytes 8192 input-file))
    (cond
      [(eof-object? line) hasher]
      [else
       (md5-hasher-update! hasher line)
       (loop)]))

  (loop))

(define (hash-file path)
  (~> path hash-file-impl md5-hasher->bytes))

(define (hash-directory path)
  (define hasher (md5-hasher))
  (walk-files path (lambda (p) (hash-file-impl p #:hasher hasher)))
  (md5-hasher->bytes hasher))

(define (directories-equal? a b)
  (equal? (hash-directory a) (hash-directory b)))
