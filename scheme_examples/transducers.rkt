(define input-collection (range 0 10))

;; passing nothing just does the reduction
; (test-transduce 
;     input-collection
;     (into-vector))

(define input-collection (range 0 10000))

(test-transduce
    input-collection
    (into-reducer + 0))

(test-transduce
    input-collection
    (into-sum))

(define (for-each func col)
    (test-transduce col (into-for-each func)))
