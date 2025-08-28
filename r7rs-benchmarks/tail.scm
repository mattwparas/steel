(require "common.scm")

(define file-exists? path-exists?)
(define delete-file delete-file!)

(define (tail-r-aux port file-so-far)
  (let ([x (read-line port)])
    (if (eof-object? x)
        file-so-far
        (tail-r-aux port (cons x file-so-far)))))

(define (echo-lines-in-reverse-order in out)
  (for-each (lambda (line)
              (write-string line out)
              (newline out))
            (tail-r-aux in '())))

(define (go input output)
  (call-with-input-file
   input
   (lambda (in)
     (when (file-exists? output)
       (delete-file output))
     (call-with-output-file output (lambda (out) (echo-lines-in-reverse-order in out))))))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [input2 (read)]
         [output (read)]
         [s3 (number->string count)]
         [s2 input2]
         [s1 input1]
         [name "tail"])
    (run-r7rs-benchmark (string-append name ":" s3)
                        count
                        (lambda () (go (hide count input1) (hide count input2)))
                        (lambda (result) #t))))

(with-input-from-file "r7rs-benchmarks/small-inputs/tail.input" run-benchmark)
