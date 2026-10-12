(define started (box #f))
(define (nothing)
  #t)
(define (spin n)
  (set-box! started #t)
  (spin (+ n 1)))
(define (wait-for b)
  (unless (unbox b)
    (wait-for b)))
(define (raised? thread)
  (eq? 'raised (with-handler (lambda (e) 'raised) (thread-join! thread))))

;; The way out of an interrupted thread runs its exits
(define exits (box '()))
(define (leaving name)
  (lambda () (set-box! exits (cons name (unbox exits)))))
(define winding
  (spawn-native-thread
   (lambda () (dynamic-wind nothing (lambda () (dynamic-wind nothing (lambda () (spin 0)) (leaving 'inner))) (leaving 'outer)))))
(wait-for started)
(thread-interrupt winding)
(assert! (raised? winding))
(assert! (equal? (unbox exits) '(outer inner)))

;; A handler sees the interrupt once, and the thread goes on
(set-box! started #f)
(define handling
  (spawn-native-thread (lambda ()
                         (let ([first (with-handler (lambda (e) 'interrupted) (spin 0))])
                           (list first (+ 1 2))))))
(wait-for started)
(thread-interrupt handling)
(assert! (equal? (thread-join! handling) '(interrupted 3)))

;; A suspended thread that is interrupted still runs its exits
(set-box! started #f)
(set-box! exits '())
(define suspended
  (spawn-native-thread (lambda () (dynamic-wind nothing (lambda () (spin 0)) (leaving 'suspended)))))
(wait-for started)
(thread-suspend suspended)
(thread-interrupt suspended)
(assert! (raised? suspended))
(assert! (equal? (unbox exits) '(suspended)))
