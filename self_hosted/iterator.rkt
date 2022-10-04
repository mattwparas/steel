


; (define (fake-map func lst)
;     (define (loop func lst accum)
;         (if (empty? lst)
;             accum
;             (loop func (cdr lst) (cons (func (car lst)) accum))))
;     (loop func lst '()))




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


(make-struct Mapping (func))

(make-struct MappingMultiple (funcs))

(make-struct Filtering (func))
(make-struct FilterMapping (func))
(make-struct Flatten)

;; Define mechanism to accumulate from an iterator -- otherwise, this will be tough
;; (define (accumulator state next))

;; Push back
(define build-list cons)

(make-struct Transducer (operations))

; (%iterator? my-iter )

;; When yielding a sequence, we should just construct 
; (define (flattening iter)

; )

(define (todo! message) (error! ""))

(define (apply-operations value operations)
    (let ((next (car operations)))
        (cond [(Mapping? next) (error! "Should return a single value here")]
              [(Filtering? next) (error! "Should skip returning a value here")]
              [(Flattening? next) (error! "Sould return a flatten object here")]

        
        )
    
    )

)

;; Iterator protocol
;; For a transducer, we need to more or less decide - is this thing going to yield something?
;; lets for a second assume we can just adopt the style of Rust...

; (define (next! ))

; (define (stream-first iter))

;; Define an abstraction for tying a struct -> iterable method