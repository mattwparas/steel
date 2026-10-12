;; Suspending, resuming and interrupting threads that are in loops, mixed with
;; collections, which ask the same threads to stop for another reason.
(require-builtin steel/time)

(define counter (box 0))
(define (spin n)
  (set-box! counter n)
  (spin (+ n 1)))
(define (ping n) (pong (+ n 1)))
(define (pong n) (ping (+ n 1)))
(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(define (recurse) (fib 20) (recurse))

(define (wait-until-past n)
  (unless (> (unbox counter) n)
    (wait-until-past n)))

(define (raised? thread)
  (eq? 'raised (with-handler (lambda (e) 'raised) (thread-join! thread))))

;; A suspended thread stops at the next place it looks, which it can be a
;; while getting to when the machine is busy. Once it has, a collection
;; doesn't start it again. Gives what the counter stopped at, or #f if it
;; never held still.
(define (stopped-at tries)
  (let ([seen (unbox counter)])
    (#%gc-collect)
    (time/sleep-ms 5)
    (cond
      [(= seen (unbox counter)) seen]
      [(> tries 0) (stopped-at (- tries 1))]
      [else #f])))

;; Stopped while suspended, moving again once resumed, each time around.
(define spinner (spawn-native-thread (lambda () (spin 0))))
(wait-until-past 0)
(define (cycle n)
  (when (> n 0)
    (thread-suspend spinner)
    (let ([seen (stopped-at 50)])
      (assert! (int? seen))
      (thread-resume spinner)
      (wait-until-past seen))
    (cycle (- n 1))))
(cycle 20)

;; Suspending twice is undone by resuming once.
(thread-suspend spinner)
(thread-suspend spinner)
(time/sleep-ms 5)
(define twice (unbox counter))
(thread-resume spinner)
(wait-until-past twice)

;; An interrupt gets through to a suspended thread.
(thread-suspend spinner)
(time/sleep-ms 5)
(thread-interrupt spinner)
(assert! (raised? spinner))

;; Several at once, each in a different kind of loop.
(define several
  (list (spawn-native-thread (lambda () (spin 0)))
        (spawn-native-thread (lambda () (ping 0)))
        (spawn-native-thread (lambda () (recurse)))
        (spawn-native-thread (lambda () (ping 0)))))
(time/sleep-ms 20)
(#%gc-collect)
(for-each thread-interrupt several)
(#%gc-collect)
(for-each (lambda (thread) (assert! (raised? thread))) several)

;; Interrupted again and again right after being started.
(define (start-and-interrupt n)
  (when (> n 0)
    (let ([thread (spawn-native-thread (lambda () (ping 0)))])
      (thread-interrupt thread)
      (assert! (raised? thread)))
    (start-and-interrupt (- n 1))))
(start-and-interrupt 20)

;; In a call that isn't compiled code, and noticed once it returns.
(define sleeper
  (spawn-native-thread (lambda ()
                         (time/sleep-ms 50)
                         (ping 0))))
(time/sleep-ms 5)
(thread-interrupt sleeper)
(assert! (raised? sleeper))

;; A thread that finishes is not disturbed by any of it.
(define finishing (spawn-native-thread (lambda () (fib 22))))
(thread-suspend finishing)
(#%gc-collect)
(thread-resume finishing)
(assert! (= (thread-join! finishing) 17711))
