(require-builtin steel/time)
(require "steel/result")

(require "steel/tests/unit-test.scm"
         (for-syntax "steel/tests/unit-test.scm")
         "threads.scm")

(require (for-syntax "../time/time.scm"))

(require "steel/iterators")

(provide __module__)

(define __module__ 'thread-test-module)

; (define (spawn-concurrent-tasks)

;   (let ([tasks (time! (map (lambda (_)
;                              (spawn-thread! (lambda ()
;                                               (time/sleep-ms 2000)
;                                               (displayln (thread::current/id)))))
;                            (range 0 10)))])
;     (map (lambda (x) (thread-join! x)) tasks)))

; ; (error "HELLO WORLD")

; (test-module
;  "Basic threads works"
;  (check-equal? "spawn-threads" (spawn-concurrent-tasks) (map (lambda (x) void) (range 0 10))))

; (spawn-concurrent-tasks)

; (define/generator (foo-bar-baz) (yield 10) (yield 20) (yield 30))
