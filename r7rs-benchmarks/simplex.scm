;;; SIMPLEX -- Simplex algorithm.
; #lang r7rs
; (import (scheme base)
;         (only (racket base) version))
; (define (this-scheme-implementation-name)
;   (string-append "racket-" (version) "/r7rs"))

; (import (scheme base)
;         (scheme read)
;         (scheme write)
;         (scheme time))

; (require "steel/mutable-vectors")

(define vector-ref mut-vector-ref)
; (define vector-set! mutable-vector-set!)
(define vector-length mut-vec-len)
(define (make-vector n)
  (apply mutable-vector (map (lambda (x) 0) (range 0 n))))

(define vector-length mut-vec-len)
(define vector mutable-vector)

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define-syntax do
  (syntax-rules ()
    [(do ((var init step ...) ...) (test expr ...) command ...)
     (letrec* ([loop
                (lambda (var ...)
                  (if test
                      (begin
                        expr ...)
                      (begin
                        command ...
                        (loop (do "step" var step ...) ...))))])
              (loop init ...))]
    [(do "step" x) x]
    [(do "step" x y) y]))

(define (matrix-rows a)
  (vector-length a))
(define (matrix-columns a)
  (vector-length (vector-ref a 0)))
(define (matrix-ref a i j)
  (vector-ref (vector-ref a i) j))
(define (matrix-set! a i j x)
  (vector-set! (vector-ref a i) j x))

(define (complain)
  (error #f "This shouldn't happen"))

(define (simplex a m1 m2 m3)
  ;(define *epsilon* 1e-6)
  (define *epsilon* 0.000001)
  (unless (and (>= m1 0) (>= m2 0) (>= m3 0) (= (matrix-rows a) (+ m1 m2 m3 2)))
    (complain))
  (let* ([m12 (+ m1 m2 1)]
         [m (- (matrix-rows a) 2)]
         [n (- (matrix-columns a) 1)]
         ; [nl1 n]
         [l1 (make-vector n)]
         [l2 (make-vector m)]
         [l3 (make-vector m2)]
         [nl1 n]
         [iposv (make-vector m)]
         [izrov (make-vector n)]
         [ip 0]
         [kp 0]
         [bmax 0.0]
         [one? #f]
         [pass2? #t])

    (define (simp1 mm abs?)
      (set! kp (vector-ref l1 0))
      (set! bmax (matrix-ref a mm kp))
      (do ((k 1 (+ k 1)))
          ((>= k nl1))
          (begin
            (when (positive? (if abs?
                                 (- (abs (matrix-ref a mm (vector-ref l1 k))) (abs bmax))
                                 (- (matrix-ref a mm (vector-ref l1 k)) bmax)))
              (begin
                (set! kp (vector-ref l1 k))
                (set! bmax (matrix-ref a mm (vector-ref l1 k))))))))
    (define (simp2)
      (set! ip 0)
      (let ([q1 0.0] [flag? #f])
        (do ((i 0 (+ i 1)))
            ((= i m))
            (if flag?
                (when (< (matrix-ref a (vector-ref l2 i) kp) (- *epsilon*))
                  (let ([q (/ (- (matrix-ref a (vector-ref l2 i) 0))
                              (matrix-ref a (vector-ref l2 i) kp))])
                    (cond
                      [(< q q1)
                       (set! ip (vector-ref l2 i))
                       (set! q1 q)]
                      [(= q q1)
                       (let ([qp 0.0] [q0 0.0])
                         (let loop ([k 1])
                           (when (<= k n)
                             (begin
                               (set! qp (/ (- (matrix-ref a ip k)) (matrix-ref a ip kp)))
                               (set! q0
                                     (/ (- (matrix-ref a (vector-ref l2 i) k))
                                        (matrix-ref a (vector-ref l2 i) kp)))
                               (when (= q0 qp)
                                 (loop (+ k 1))))))
                         (when (< q0 qp)
                           (set! ip (vector-ref l2 i))))])))
                (when (< (matrix-ref a (vector-ref l2 i) kp) (- *epsilon*))
                  (set! q1
                        (/ (- (matrix-ref a (vector-ref l2 i) 0))
                           (matrix-ref a (vector-ref l2 i) kp)))
                  (set! ip (vector-ref l2 i))
                  (set! flag? #t))))))
    (define (simp3 one?)
      (let ([piv (/ (matrix-ref a ip kp))])
        (do ((ii 0 (+ ii 1)))
            ((= ii (+ m (if one? 2 1))))
            (unless (= ii ip)
              (matrix-set! a ii kp (* piv (matrix-ref a ii kp)))
              (do ((kk 0 (+ kk 1)))
                  ((= kk (+ n 1)))
                  (unless (= kk kp)
                    (matrix-set! a
                                 ii
                                 kk
                                 (- (matrix-ref a ii kk)
                                    (* (matrix-ref a ip kk) (matrix-ref a ii kp))))))))
        (do ((kk 0 (+ kk 1)))
            ((= kk (+ n 1)))
            (unless (= kk kp)
              (matrix-set! a ip kk (* (- piv) (matrix-ref a ip kk)))))
        (matrix-set! a ip kp piv)))
    (do ((k 0 (+ k 1))) ((= k n)) (vector-set! l1 k (+ k 1)) (vector-set! izrov k k))
    (do ((i 0 (+ i 1)))
        ((= i m))
        (when (negative? (matrix-ref a (+ i 1) 0))
          (complain))
        (vector-set! l2 i (+ i 1))
        (vector-set! iposv i (+ n i)))
    (do ((i 0 (+ i 1))) ((= i m2)) (vector-set! l3 i #t))
    (when (positive? (+ m2 m3))
      (do ((k 0 (+ k 1)))
          ((= k (+ n 1)))
          (do ((i (+ m1 1) (+ i 1)) (sum 0.0 (+ sum (matrix-ref a i k))))
              ((> i m) (matrix-set! a (+ m 1) k (- sum)))))
      (let loop ()
        (simp1 (+ m 1) #f)
        (cond
          [(<= bmax *epsilon*)
           (cond
             [(< (matrix-ref a (+ m 1) 0) (- *epsilon*)) (set! pass2? #f)]
             [(<= (matrix-ref a (+ m 1) 0) *epsilon*)
              (let loop ([ip1 m12])
                (if (<= ip1 m)
                    (cond
                      [(= (vector-ref iposv (- ip1 1)) (+ ip n -1))
                       (simp1 ip1 #t)
                       (cond
                         [(positive? bmax)
                          (set! ip ip1)
                          (set! one? #t)]
                         [else (loop (+ ip1 1))])]
                      [else (loop (+ ip1 1))])
                    (do ((i (+ m1 1) (+ i 1)))
                        ((>= i m12))
                        (when (vector-ref l3 (- i (+ m1 1)))
                          (do ((k 0 (+ k 1)))
                              ((= k (+ n 1)))
                              (matrix-set! a i k (- (matrix-ref a i k))))))))]
             [else
              (simp2)
              (if (zero? ip) (set! pass2? #f) (set! one? #t))])]
          [else
           (simp2)
           (if (zero? ip) (set! pass2? #f) (set! one? #t))])
        (when one?
          (set! one? #f)
          (simp3 #t)
          (cond
            [(>= (vector-ref iposv (- ip 1)) (+ n m12 -1))
             (let loop ([k 0])
               (cond
                 [(and (< k nl1) (not (= kp (vector-ref l1 k)))) (loop (+ k 1))]
                 [else
                  (set! nl1 (- nl1 1))
                  (do ((is k (+ is 1))) ((>= is nl1)) (vector-set! l1 is (vector-ref l1 (+ is 1))))
                  (matrix-set! a (+ m 1) kp (+ (matrix-ref a (+ m 1) kp) 1.0))
                  (do ((i 0 (+ i 1)))
                      ((= i (+ m 2)))
                      (matrix-set! a i kp (- (matrix-ref a i kp))))]))]
            [(and (>= (vector-ref iposv (- ip 1)) (+ n m1))
                  (vector-ref l3 (- (vector-ref iposv (- ip 1)) (+ m1 n))))
             (vector-set! l3 (- (vector-ref iposv (- ip 1)) (+ m1 n)) #f)
             (matrix-set! a (+ m 1) kp (+ (matrix-ref a (+ m 1) kp) 1.0))
             (do ((i 0 (+ i 1))) ((= i (+ m 2))) (matrix-set! a i kp (- (matrix-ref a i kp))))])
          (let ([t (vector-ref izrov (- kp 1))])
            (vector-set! izrov (- kp 1) (vector-ref iposv (- ip 1)))
            (vector-set! iposv (- ip 1) t))
          (loop))))

    (and pass2?
         (let loop ()
           (simp1 0 #f)
           (cond
             [(positive? bmax)
              (simp2)
              (cond
                [(zero? ip) #t]
                [else
                 (simp3 #f)
                 (let ([t (vector-ref izrov (- kp 1))])
                   (vector-set! izrov (- kp 1) (vector-ref iposv (- ip 1)))
                   (vector-set! iposv (- ip 1) t))
                 (loop)])]
             [else (list iposv izrov)])))))

(define (test input)
  (simplex (vector (vector 0.0 1.0 1.0 3.0 -0.5)
                   (vector 740.0 -1.0 0.0 -2.0 0.0)
                   (vector 0.0 0.0 -2.0 0.0 7.0)
                   (vector 0.5 0.0 -1.0 1.0 -2.0)
                   (vector 9.0 -1.0 -1.0 -1.0 -1.0)
                   (vector 0.0 0.0 0.0 0.0 0.0))
           2
           1
           1))

(assert! (equal? (list (vector 4 1 3 2) (vector 0 5 7 6)) (test 740.0)))

(let loop ([i 0])
  ; (when (< i 1000000)
  ; (when (< i 1000000)
  (when (< i 1)
    (begin
      ; (displayln i)
      (assert! (equal? (list (vector 4 1 3 2) (vector 0 5 7 6)) (test 740.0)))

      (loop (+ i 1)))))

; 1000000
; 740.0
; (#(4 1 3 2) #(0 5 7 6))

; (define (run-benchmark)
;   (let* ((count (read))
;          (input1 (read))
;          (output (read))
;          (s2 (number->string count))
;          (s1 "")
;          (name "simplex"))
;     (run-r7rs-benchmark
;      (string-append name ":" s2)
;      count
;      (lambda () (test (hide count input1)))
;      (lambda (result) (equal? result output)))))
