;-- type Numbered a = (Int,a)
(define (make-numbered-value tag val) (cons tag val))
; accessors of the components of the value
(define (nvalue-tag tv) (car tv))
(define (nvalue-val tv) (cdr tv))



;-- return:: a -> NumberedM a
(define (return val)
  (lambda (curr_counter)
    (make-numbered-value curr_counter val)))


;-- (>>=):: NumberedM a -> (a -> NumberedM b) -> NumberedM b
(define (>>= m f)
  (lambda (curr_counter)
    (let* ((m_result (m curr_counter))
           (n1 (nvalue-tag m_result))   ; result of the delayed computation
           (v  (nvalue-val m_result))   ; represented by m

           (m1 (f v))                   ; feed the result to f, get another m1
           )
      (m1 n1))))                        ; The result of the bigger monad


(define incr
  (lambda (n)
    (make-numbered-value (+ 1 n) n)))


;-- run_numberedM:: NumberedM a -> Int -> Numbered a
;-- run_numberedM (NumberedM m) init_count = m init_count

(define (runM m init-counter)
  (m init-counter))


;; (define computation (>>= (incr) ()))


;; (runM (make-numbered-value "lmao" 0) 0)
