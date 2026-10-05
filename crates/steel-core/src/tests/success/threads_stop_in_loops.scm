;; A thread in a loop that calls nothing still has to stop for a collection, a
;; suspend and an interrupt, and so does one going around a cycle of tail calls.
(require-builtin steel/time)

(define counter (box 0))
(define (spin n)
  (set-box! counter n)
  (spin (+ n 1)))
(define (ping n) (pong (+ n 1)))
(define (pong n) (ping (+ n 1)))

(define (wait-until-past n)
  (unless (> (unbox counter) n)
    (wait-until-past n)))

(define (interrupted? thread)
  (thread-interrupt thread)
  (eq? 'raised (with-handler (lambda (e) 'raised) (thread-join! thread))))

(define spinner (spawn-native-thread (lambda () (spin 0))))
(wait-until-past 0)

;; Both of these stop every other thread.
(#%gc-collect)
(define after-collection (unbox counter))
(wait-until-past after-collection)

(thread-suspend spinner)
(time/sleep-ms 50)
(define while-suspended (unbox counter))
(time/sleep-ms 50)
(assert! (= while-suspended (unbox counter)))
(#%gc-collect)
(thread-resume spinner)
(wait-until-past while-suspended)

(assert! (interrupted? spinner))

(define pinger (spawn-native-thread (lambda () (ping 0))))
(time/sleep-ms 50)
(#%gc-collect)
(assert! (interrupted? pinger))

;; Asked before the thread has started anything.
(define late (spawn-native-thread (lambda () (ping 0))))
(assert! (interrupted? late))
