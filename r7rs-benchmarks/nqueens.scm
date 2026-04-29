(provide nqueens)

(require "common.scm")

(define trace? #f)

(define (ok? row dist placed)
  (if (null? placed)
      #t
      (and (not (= (car placed) (+ row dist)))
           (not (= (car placed) (- row dist)))
           (ok? row (+ dist 1) (cdr placed)))))

(define (iota1 n)
  (let loop ([i n]
             [l '()])
    (if (= i 0)
        l
        (loop (- i 1) (cons i l)))))

(define (my-try x y z)
  (if (null? x)
      (if (null? y)
          (begin
            (when trace?
              (begin
                (write z)
                (newline)))
            1)
          0)
      (+ (if (ok? (car x) 1 z)
             ; (let ([head (car x)]) (my-try (append (cdr x) y) '() (cons head z)))
             (my-try (append (cdr x) y) '() (cons (car x) z))
             0)
         (my-try (cdr x) (cons (car x) y) z))))

(define (nqueens n)

  ; (define (iota1 n)
  ;   (let loop ([i n]
  ;              [l '()])
  ;     (if (= i 0)
  ;         l
  ;         (loop (- i 1) (cons i l)))))

  ; (define (my-try x y z)
  ;   (if (null? x)
  ;       (if (null? y)
  ;           (begin
  ;             (when trace?
  ;               (begin
  ;                 (write z)
  ;                 (newline)))
  ;             1)
  ;           0)
  ;       (+ (if (ok? (car x) 1 z)
  ;              ; (let ([head (car x)]) (my-try (append (cdr x) y) '() (cons head z)))
  ;              (my-try (append (cdr x) y) '() (cons (car x) z))
  ;              0)
  ;          (my-try (cdr x) (cons (car x) y) z))))

  ; (define (ok? row dist placed)
  ;   (if (null? placed)
  ;       #t
  ;       (and (not (= (car placed) (+ row dist)))
  ;            (not (= (car placed) (- row dist)))
  ;            (ok? row (+ dist 1) (cdr placed)))))

  (my-try (iota1 n) '() '()))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [output (read)]
         [s2 (number->string count)]
         [s1 (number->string input1)]
         [name "nqueens"])
    (run-r7rs-benchmark (string-append name ":" s1 ":" s2)
                        count
                        (lambda () (nqueens (hide count input1)))
                        (lambda (result) (= result output)))))

(with-input-from-file "r7rs-benchmarks/inputs/nqueens.input" run-benchmark)
