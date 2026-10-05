;; Boxes shared by several threads, holding values of every kind that needs
;; releasing. Whatever a reader finds has to be a whole value of one of the
;; kinds, and each is dropped by whichever thread replaces it.
(struct tagged (n twice) #:mutable)

(define (payload n)
  (let ([kind (modulo n 6)])
    (cond
      [(= kind 0) (list n n n)]
      [(= kind 1) (string-append (number->string n) ":" (number->string n))]
      [(= kind 2) (lambda () (list n n))]
      [(= kind 3) (mutable-vector n n)]
      [(= kind 4) (tagged n (* 2 n))]
      [else (box (list n n))])))

(define (whole? v)
  (cond
    [(list? v) (and (= (length v) 3) (equal? (car v) (caddr v)))]
    [(string? v) (> (string-length v) 2)]
    [(function? v)
     (let ([made (v)])
       (equal? (car made) (cadr made)))]
    [(vector? v) (and (= (vector-length v) 2) (equal? (vector-ref v 0) (vector-ref v 1)))]
    [(tagged? v) (= (tagged-twice v) (* 2 (tagged-n v)))]
    ;; Only a box is left.
    [else
     (let ([inner (unbox v)])
       (equal? (car inner) (cadr inner)))]))

(define first-box (box (payload 0)))
(define second-box (box (payload 1)))
;; A box whose contents are themselves a box that gets replaced.
(define outer (box (box (payload 2))))

(define (write n)
  (when (> n 0)
    (set-box! first-box (payload n))
    (set-box! second-box (unbox first-box))
    (set-box! outer (box (payload n)))
    (write (- n 1))))
(define (read n ok)
  (if (= n 0)
      ok
      (read (- n 1)
            (and ok (whole? (unbox first-box)) (whole? (unbox second-box)) (whole? (unbox (unbox outer)))))))

;; Run alone first, so the compiled code is older than the threads.
(write 2000)
(assert! (read 2000 #t))

(define writers
  (list (spawn-native-thread (lambda () (write 20000)))
        (spawn-native-thread (lambda () (write 20000)))
        (spawn-native-thread (lambda () (write 20000)))))
(define readers
  (list (spawn-native-thread (lambda () (read 30000 #t)))
        (spawn-native-thread (lambda () (read 30000 #t)))))
(assert! (read 30000 #t))
(write 2000)
(for-each thread-join! writers)
(for-each (lambda (reader) (assert! (thread-join! reader))) readers)
(#%gc-collect)
(assert! (read 100 #t))
