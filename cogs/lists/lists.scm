(provide split-last
         flatten
         for-each
		 repeat)

(define (split-last-loop accum lst)
  (if (empty? (cdr lst))
      (list (reverse accum) (car lst))
      (split-last-loop (cons (car lst) accum) (cdr lst))))

;; @doc
;; Given a list, splits off the last argument, returns as a pair
(define (split-last lst)
  (split-last-loop '() lst))

;;@doc
;; Flattens the incoming list one level
;; ```scheme
;; (flatten (list (list 10 20) (list 30 40))) ;; => '(10 20 30 40)
;; ```
(define (flatten lst)
  (transduce lst (flattening) (into-list)))

;;@doc
;; Iterate over each item in the lst, calling the function on it.
;; ```scheme
;; (for-each displayln (list 1 2 3))
;; ```
;; Will print:
;; ```
;; 1
;; 2
;; 3
;; ```
(define (for-each func lst)
  (if (null? lst)
      void
      (begin
        (func (car lst))
        (when (null? lst)
          (return! void))
        (for-each func (cdr lst)))))

;; Need default arguments here
; (define (remove v lst [proc ]))

;;@doc
;; Repeat a value a number of times
;; ```scheme
;; (repeat "a" 4)
;; ```
;; will print:
;; ```
;; '("a" "a" "a" "a")
;; ```
(define (repeat val times)
  (cond
	[(and (integer? times) (> times 0))
		(map (λ (_) val) (range 0 times))]
	[else
	  (error 'invalid-arg "'times' must be a positive nonzero integer.")]))
