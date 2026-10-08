;; One thread reads values out of cells, another keeps replacing them, and
;; this one collects. A value that a read has taken out of its cell, and
;; that the writer has since replaced, is held by nothing a collection can
;; see until the read hands it back. It still has to be whole afterwards.
(struct holder (current) #:mutable)

(define in-box (box (box (list 0 0))))
(define in-vector (mutable-vector (box (list 0 0))))
(define in-struct (holder (box (list 0 0))))
(define stop (box #f))

(define (write n)
  (unless (unbox stop)
    (set-box! in-box (box (list n n)))
    (vector-set! in-vector 0 (box (list n n)))
    (set-holder-current! in-struct (box (list n n)))
    (write (+ n 1))))

(define (whole? inner)
  (let ([contents (unbox inner)])
    (and (list? contents) (= (length contents) 2))))
(define (read n ok)
  (if (= n 0)
      ok
      (read (- n 1)
            (and ok
                 (whole? (unbox in-box))
                 (whole? (vector-ref in-vector 0))
                 (whole? (holder-current in-struct))))))

;; What a write hands back is in the same position.
(define (swap n ok)
  (if (= n 0)
      ok
      (swap (- n 1) (and ok (whole? (set-box! in-box (box (list n n))))))))

(define writer (spawn-native-thread (lambda () (write 1))))
(define readers
  (list (spawn-native-thread (lambda () (read 60000 #t)))
        (spawn-native-thread (lambda () (swap 60000 #t)))))
(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))
(collect 150)
(for-each (lambda (reader) (assert! (thread-join! reader))) readers)
(set-box! stop #t)
(thread-join! writer)
