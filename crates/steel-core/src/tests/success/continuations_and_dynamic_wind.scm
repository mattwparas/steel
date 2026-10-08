;; Continuations going out of and back into the extent of a `dynamic-wind`,
;; which has to run the right guards in the right order each time.

(define (note! log x) (set-box! log (cons x (unbox log))))
(define (seen log) (reverse (unbox log)))

;; Leaving through a continuation runs the after guard once.
(define (escape-runs-after)
  (let ([log (box '())])
    (call/cc (lambda (k)
               (dynamic-wind (lambda () (note! log 'in))
                             (lambda () (note! log 'body) (k 'left) (note! log 'not-reached))
                             (lambda () (note! log 'out)))))
    (seen log)))
(assert! (equal? (escape-runs-after) '(in body out)))

;; A normal return runs both guards once.
(define (return-runs-both)
  (let ([log (box '())])
    (dynamic-wind (lambda () (note! log 'in)) (lambda () (note! log 'body)) (lambda () (note! log 'out)))
    (seen log)))
(assert! (equal? (return-runs-both) '(in body out)))

;; Nested extents are left innermost first.
(define (nested-escape)
  (let ([log (box '())])
    (call/cc (lambda (k)
               (dynamic-wind
                (lambda () (note! log 'in-1))
                (lambda ()
                  (dynamic-wind (lambda () (note! log 'in-2))
                                (lambda ()
                                  (dynamic-wind (lambda () (note! log 'in-3))
                                                (lambda () (k 'out))
                                                (lambda () (note! log 'out-3))))
                                (lambda () (note! log 'out-2))))
                (lambda () (note! log 'out-1)))))
    (seen log)))
(assert! (equal? (nested-escape) '(in-1 in-2 in-3 out-3 out-2 out-1)))

;; Escaping from an inner extent to a point inside an outer one leaves only
;; the inner.
(define (escape-part-way)
  (let ([log (box '())])
    (dynamic-wind
     (lambda () (note! log 'in-outer))
     (lambda ()
       (call/cc (lambda (k)
                  (dynamic-wind (lambda () (note! log 'in-inner))
                                (lambda () (k 'left))
                                (lambda () (note! log 'out-inner)))))
       (note! log 'between))
     (lambda () (note! log 'out-outer)))
    (seen log)))
(assert! (equal? (escape-part-way) '(in-outer in-inner out-inner between out-outer)))

;; Going back into an extent that was left runs the before guard again.
(define (reenter)
  (let ([log (box '())]
        [saved (box #f)]
        [times (box 0)])
    (dynamic-wind (lambda () (note! log 'in))
                  (lambda () (call/cc (lambda (k) (set-box! saved k))) (note! log 'body))
                  (lambda () (note! log 'out)))
    (set-box! times (+ (unbox times) 1))
    (if (< (unbox times) 3)
        ((unbox saved) 'again)
        (seen log))))
(assert! (equal? (reenter) '(in body out in body out in body out)))

;; The guards themselves can capture and escape.
(define (escape-from-the-after-guard)
  (let ([log (box '())])
    (call/cc (lambda (k)
               (dynamic-wind (lambda () (note! log 'in))
                             (lambda () (note! log 'body))
                             (lambda () (note! log 'out) (k 'from-guard)))))
    (seen log)))
(assert! (equal? (escape-from-the-after-guard) '(in body out)))

;; A loop that leaves an extent on every trip.
(define (leave-many n)
  (let ([ins (box 0)]
        [outs (box 0)])
    (let loop ([i 0])
      (when (< i n)
        (call/cc (lambda (k)
                   (dynamic-wind (lambda () (set-box! ins (+ (unbox ins) 1)))
                                 (lambda () (k i))
                                 (lambda () (set-box! outs (+ (unbox outs) 1))))))
        (loop (+ i 1))))
    (list (unbox ins) (unbox outs))))
(assert! (equal? (leave-many 5000) '(5000 5000)))

;; An error raised inside an extent runs the after guard on the way to the
;; handler.
(define (error-runs-after)
  (let ([log (box '())])
    (with-handler (lambda (e) (note! log 'handled))
                  (dynamic-wind (lambda () (note! log 'in))
                                (lambda () (error "inside"))
                                (lambda () (note! log 'out))))
    (seen log)))
(assert! (equal? (error-runs-after) '(in out handled)))
