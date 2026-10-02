;; A struct predicate or getter on a captured variable reads the capture
;; without cloning it. Run hot so the jitted versions are the ones checked.
(struct wrap (items))
(struct other (items))

(define (make-reader s)
  (lambda (i)
    (if (wrap? s)
        (vector-ref (wrap-items s) i)
        'not-wrapped)))

(define (make-getter s)
  (lambda () (wrap-items s)))

(define w (wrap (vector 10 20 30)))
(define read-w (make-reader w))
(define read-str (make-reader "abc"))
(define read-other (make-reader (other (vector 1 2 3))))
(define get-w (make-getter w))
(define get-other (make-getter (other (vector 1 2 3))))

(define (loop n acc)
  (if (= n 0)
      acc
      (loop (- n 1)
            (+ acc (read-w (modulo n 3)) (vector-length (get-w))))))

(assert! (equal? (loop 3000 0) 69000))
(assert! (equal? (read-str 0) 'not-wrapped))
(assert! (equal? (read-other 0) 'not-wrapped))

;; The wrong struct takes the getter's slow arm, which raises.
(define (try-get-other)
  (with-handler (lambda (e) 'raised) (get-other)))
(define (loop-other n)
  (if (= n 0)
      (try-get-other)
      (begin
        (try-get-other)
        (loop-other (- n 1)))))
(assert! (equal? (loop-other 2000) 'raised))
(assert! (equal? (vector-ref (get-w) 2) 30))
