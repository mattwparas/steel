


(define (fake-map func lst)
    (define (loop func lst accum)
        (if (empty? lst)
            accum
            (loop func (cdr lst) (cons (func (car lst)) accum))))
    (loop func lst '()))




(define (loop iterator)
    (let ((next (iter-next! iterator)))
        (if next
            (begin 
                (displayln next)
                (loop iterator))
            void)))

; (define my-list (range 0 100))

(define my-iter (value->iterator "hello world"))

(loop my-iter)


(struct Mapping (func))

(struct MappingMultiple (funcs))

(struct Filtering (func))
(struct FilterMapping (func))
(struct FlatMapping (func))

;; Define mechanism to accumulate from an iterator -- otherwise, this will be tough
;; (define (accumulator state next))

;; Push back
(define build-list cons)

(struct Transducer )

;; Iterator protocol
;; For a transducer, we need to more or less decide - is this thing going to yield something?
;; lets for a second assume we can just adopt the style of Rust...

; (define (next! ))

; (define (stream-first iter))

;; Define an abstraction for tying a struct -> iterable method