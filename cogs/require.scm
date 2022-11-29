(require "std::option")
(provide (contract/out foo (->/c string? any/c)))

(define *internal* (None))

(define foo (lambda (x) x))