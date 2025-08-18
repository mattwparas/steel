;;; CAT -- One of the Kernighan and Van Wyk benchmarks.
;;; Rewritten by Will Clinger into more idiomatic Scheme.

(require "common.scm")

(define file-exists? path-exists?)
(define delete-file delete-file!)

(define (catport in out)
  (let ([x (read-char in)])
    (unless (eof-object? x)
      (write-char x out)
      (catport in out))))

(define (go input-file output-file)
  (when (file-exists? output-file)
    (delete-file output-file))
  (call-with-input-file input-file
                        (lambda (in)
                          (call-with-output-file output-file (lambda (out) (catport in out))))))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [input2 (read)]
         [output (read)]
         [s3 (number->string count)]
         [s2 input2]
         [s1 input1]
         [name "cat"])
    (run-r7rs-benchmark (string-append name ":" s3)
                        count
                        (lambda () (go (hide count input1) (hide count input2)))
                        (lambda (result) #t))))

(with-input-from-file "r7rs-benchmarks/inputs/cat.input" run-benchmark)
