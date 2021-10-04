(define input-collection (range 0 10))

;; passing nothing just does the reduction
; (test-transduce 
;     input-collection
;     (into-vector))

(test-transduce
    (range 0 1000)
    (into-reducer + 0))

(define (for-each func col)
    (test-transduce col (into-for-each func)))
