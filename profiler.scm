(require-builtin steel/time)
(require "r7rs-benchmarks/lattice.scm")

(define profiler (make-callstack-profiler 100))
(define (profile-loop p)
  (time/sleep-ms 10)
  (#%snapshot-stacks p)
  (profile-loop p))
(define profiler-thread (spawn-native-thread (lambda () (profile-loop profiler))))

(define (report p)

  (callstack-hydrate-names p)
  (dump-profiler p))

;; Do the thing
(with-input-from-file "r7rs-benchmarks/inputs/lattice.input" run-benchmark)

(report profiler)
