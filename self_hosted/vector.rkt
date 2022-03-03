(require "std::option")

; (define (position-from-back vec func)
;     ;; Get the last index of the vector
;     (define last-index (- (mut-vec-len vec) 1))
;     (when (< last-index 0) 
;           (return! (None)))
;     (define (looper vec idx)
;         (cond 
;             ;; If we're outside of the range of the vector, return None
;             [(< idx 0) => (None)]
;             [(func (mut-vector-ref vec idx)) => (Some idx)]
;             [else => (looper vec (- idx 1))]))
;     (looper vec last-index))

; (displayln None)

; (define test (mutable-vector 1 2 3 4 5))

; (displayln
;     (unwrap-some (position-from-back test (lambda (x) (equal? x 3)))))

; (displayln
;     (unwrap-some (position-from-back test (lambda (x) (equal? x 5)))))