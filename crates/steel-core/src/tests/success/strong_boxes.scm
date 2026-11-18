;; Strong boxes, which the jit reads and writes inline (alone without the
;; lock, with other threads under it)

(define (count-up b n)
  (if (= n 0)
      (unbox-strong b)
      (begin
        (set-strong-box! b (+ (unbox-strong b) 1))
        (count-up b (- n 1)))))

(define (sum-boxes boxes acc)
  (if (null? boxes)
      acc
      (sum-boxes (cdr boxes) (+ acc (unbox-strong (car boxes))))))

(define (run)
  (define b (box-strong 0))
  (define boxes (map box-strong (range 0 100)))
  (list (count-up b 100000)
        (let loop ([i 0]
                   [t 0])
          (if (= i 100) t (loop (+ i 1) (+ t (sum-boxes boxes 0)))))))

(assert! (equal? (run) (list 100000 495000)))

;; Heap values in a box, replaced over and over
(define s (box-strong (list 1 2 3)))
(let loop ([i 0])
  (when (< i 10000)
    (set-strong-box! s (cons i (cdr (unbox-strong s))))
    (loop (+ i 1))))
(assert! (equal? (unbox-strong s) (list 9999 2 3)))

;; Anything else is the primitive's error
(assert! (equal? (with-handler (lambda (e) 'raised) (unbox-strong 5)) 'raised))
(assert! (equal? (with-handler (lambda (e) 'raised) (set-strong-box! 'x 1)) 'raised))
(assert! (void? (set-strong-box! (box-strong 1) 2)))

;; Once a second thread has run, every access takes the lock. Each thread
;; counts in a box of its own and reads one they all share.
(define shared (box-strong 7))
(define (worker)
  (define own (box-strong 0))
  (let loop ([i 0])
    (when (< i 1000)
      (set-strong-box! own (+ (unbox-strong own) (unbox-strong shared)))
      (loop (+ i 1))))
  (unbox-strong own))
(define threads (map (lambda (_) (spawn-native-thread worker)) (range 0 4)))
(assert! (equal? (map thread-join! threads) (list 7000 7000 7000 7000)))
(assert! (equal? (run) (list 100000 495000)))
