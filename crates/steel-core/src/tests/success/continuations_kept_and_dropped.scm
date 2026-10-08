;; A continuation is only copied out of the stack if something will still
;; have it once the frame it was captured in is gone. These are the ways one
;; can be kept, each called after that frame has returned, and the ways it is
;; merely passed around and let go of.

;; Each of these is one function, since a continuation captured at the top
;; level only goes back over the rest of the form it was captured in.

;; Kept in a box, and called again and again.
(define (kept-in-a-box)
  (let* ([saved (box #f)]
         [count (box 0)]
         [v (+ 1 (call/cc (lambda (k) (set-box! saved k) 1)))])
    (set-box! count (+ (unbox count) 1))
    (if (< (unbox count) 5)
        ((unbox saved) (* 10 (unbox count)))
        (list (unbox count) v))))
(assert! (equal? (kept-in-a-box) '(5 41)))

;; Kept in a global by `set!`.
(define kept #f)
(define trail '())
(define (kept-in-a-global)
  (let ([v (call/cc (lambda (k) (set! kept k) 'first))])
    (set! trail (cons v trail))
    (if (< (length trail) 3)
        (kept (length trail))
        trail)))
(assert! (equal? (kept-in-a-global) '(2 1 first)))

;; Returned as the value of the call that captured it.
(define (returned-as-a-value)
  (let* ([hits (box 0)]
         [returned (call/cc (lambda (k) k))])
    (set-box! hits (+ (unbox hits) 1))
    (if (= (unbox hits) 1)
        (returned 'done)
        (list returned (unbox hits)))))
(assert! (equal? (returned-as-a-value) '(done 2)))

;; Kept by a closure that is called later.
(define (kept-by-a-closure)
  (let* ([out (box #f)]
         [v (call/cc (lambda (k) (set-box! out (lambda (x) (k x))) 0))])
    (if (< v 3)
        ((unbox out) (+ v 1))
        v)))
(assert! (= (kept-by-a-closure) 3))

;; Kept in a vector and in a struct field.
(struct holder (k) #:mutable)
(define (kept-in-cells)
  (let* ([slots (mutable-vector #f)]
         [held (holder #f)]
         [rounds (box 0)]
         [got (call/cc (lambda (k) (vector-set! slots 0 k) (set-holder-k! held k) 0))])
    (set-box! rounds (+ (unbox rounds) 1))
    (cond
      [(= got 0) ((vector-ref slots 0) 1)]
      [(= got 1) ((holder-k held) 2)]
      [else (list got (unbox rounds))])))
(assert! (equal? (kept-in-cells) '(2 3)))

;; Kept past the function that captured it, and called after that function
;; has returned, more than once.
(define (capture-and-return saved)
  (+ 100 (call/cc (lambda (k) (set-box! saved k) 1))))
(define (kept-past-its-function)
  (let* ([saved (box #f)]
         [seen (box '())]
         [v (capture-and-return saved)])
    (set-box! seen (cons v (unbox seen)))
    (if (< (length (unbox seen)) 4)
        ((unbox saved) (length (unbox seen)))
        (unbox seen))))
(assert! (equal? (kept-past-its-function) '(103 102 101 101)))

;; The same from the bottom of a deep stack, so that there is a lot of stack
;; to have kept.
(define (down d saved)
  (if (= d 0)
      (call/cc (lambda (k) (set-box! saved k) 0))
      (+ 1 (down (- d 1) saved))))
(define (kept-under-a-deep-stack)
  (let* ([saved (box #f)]
         [times (box 0)]
         [v (down 200 saved)])
    (set-box! times (+ (unbox times) 1))
    (if (< (unbox times) 3)
        ((unbox saved) (* 1000 (unbox times)))
        (list v (unbox times)))))
(assert! (equal? (kept-under-a-deep-stack) '(2200 3)))

;; An inner continuation that is kept, passed on the way out through an
;; outer one, and gone back into afterwards.
(define (inner-kept-outer-escaped)
  (let* ([inner-kept (box #f)]
         [order (box '())]
         [through (call/cc (lambda (out)
                             (let ([v (call/cc (lambda (in) (set-box! inner-kept in) 'inner-first))])
                               (set-box! order (cons v (unbox order)))
                               (out v))))])
    (if (eq? through 'inner-first)
        ((unbox inner-kept) 'inner-again)
        (list through (unbox order)))))
(assert! (equal? (inner-kept-outer-escaped) '(inner-again (inner-again inner-first))))

;; Only passed around: escaping from the function it was handed to, from
;; functions further up, past other captures, and not used at all. Deep
;; enough that copying the stack each time would be seen in how long it took.
(define (pass k v) (k v))
(define (escape-loop n acc)
  (if (= n 0)
      acc
      (escape-loop (- n 1) (+ acc (call/cc (lambda (k) (pass k 1)))))))
(define (unused-loop n acc)
  (if (= n 0) acc (unused-loop (- n 1) (+ acc (call/cc (lambda (k) 1))))))
(define (nested-escape n)
  (call/cc (lambda (out)
             (let loop ([i n])
               (if (= i 0)
                   (out 'bottom)
                   (+ 1 (call/cc (lambda (in) (loop (- i 1))))))))))
(define (at-depth d thunk) (if (= d 0) (thunk) (+ 0 (at-depth (- d 1) thunk))))

(assert! (= (at-depth 300 (lambda () (escape-loop 20000 0))) 20000))
(assert! (= (at-depth 300 (lambda () (unused-loop 20000 0))) 20000))
(assert! (eq? (nested-escape 200) 'bottom))

;; The two from the r7rs suite, small.
(define (ctak x y z) (call/cc (lambda (k) (ctak-aux k x y z))))
(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (call/cc (lambda (k)
                 (ctak-aux k
                           (call/cc (lambda (k) (ctak-aux k (- x 1) y z)))
                           (call/cc (lambda (k) (ctak-aux k (- y 1) z x)))
                           (call/cc (lambda (k) (ctak-aux k (- z 1) x y))))))))
(assert! (= (ctak 14 10 4) 5))

(define (addc x y k) (if (zero? y) (k x) (addc (+ x 1) (- y 1) k)))
(define (fibc x c)
  (if (zero? x)
      (c 0)
      (if (zero? (- x 1))
          (c 1)
          (addc (call/cc (lambda (c) (fibc (- x 1) c))) (call/cc (lambda (c) (fibc (- x 2) c))) c))))
(assert! (= (fibc 18 (lambda (n) n)) 2584))

;; A generator, which goes in and out of two continuations many times.
(define (make-generator items)
  (define return #f)
  (define (resume-point)
    (for-each (lambda (item)
                (call/cc (lambda (next)
                           (set! resume-point (lambda () (next #f)))
                           (return item))))
              items)
    (return 'done))
  (lambda () (call/cc (lambda (r) (set! return r) (resume-point)))))
(define (drain)
  (let* ([next (make-generator (list 1 2 3))]
         [a (next)]
         [b (next)]
         [c (next)]
         [d (next)])
    (list a b c d)))
(assert! (equal? (drain) '(1 2 3 done)))
