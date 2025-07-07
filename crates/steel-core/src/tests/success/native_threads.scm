;;;;;;;;;;;; Native threads ;;;;;;;;;;;;;;;;
(require-builtin steel/time)
(require "steel/sync")

;; Create work for the thread-pool

(define tp (make-thread-pool 10))

(define (make-task)
  (submit-task tp
               (lambda ()
                 (time/sleep-ms 1000)
                 10)))

(define all-tasks (map (lambda (_) (make-task)) (range 0 10)))

(define results (map block-on-task all-tasks))

(displayln results)

;; Could do some timing tests - but for now this will work
(assert! (= (sum results) 100))

(define foobar 100)

(thread-join! (spawn-native-thread (lambda () (set! foobar 200))))

(assert! (= foobar 200))
