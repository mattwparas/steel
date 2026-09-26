;; <, <=, > and >= of a local against an int share one lowering: an inline
;; compare when the local holds an int (against an immediate when the int is a
;; constant), and a helper for anything else, which has to give the same answer
;; as the interpreter or raise the same error.

(define (lt-c x) (< x 5))
(define (lte-c x) (<= x 5))
(define (gt-c x) (> x 5))
(define (gte-c x) (>= x 5))

(define (lt-neg x) (< x -3))
(define (gt-neg x) (> x -3))

;; the int is a let bound register
(define (lt-let x) (let ([k 5]) (< x k)))
(define (gte-let x) (let ([k 5]) (>= x k)))

;; the int is computed
(define (lt-len x s) (< x (string-length s)))
(define (gt-len x s) (> x (string-length s)))

(define (warm n)
  (if (= n 0)
      0
      (begin
        (lt-c n) (lte-c n) (gt-c n) (gte-c n)
        (lt-neg n) (gt-neg n)
        (lt-let n) (gte-let n)
        (lt-len n "abcde") (gt-len n "abcde")
        (warm (- n 1)))))

(warm 1000)

(define (check f inputs expected)
  (assert! (equal? (map f inputs) expected)))

(define ints '(4 5 6 -4 -3 -2))

(check lt-c ints '(#true #false #false #true #true #true))
(check lte-c ints '(#true #true #false #true #true #true))
(check gt-c ints '(#false #false #true #false #false #false))
(check gte-c ints '(#false #true #true #false #false #false))
(check lt-neg ints '(#false #false #false #true #false #false))
(check gt-neg ints '(#true #true #true #false #false #true))
(check lt-let ints '(#true #false #false #true #true #true))
(check gte-let ints '(#false #true #true #false #false #false))
(check (lambda (x) (lt-len x "abcde")) ints '(#true #false #false #true #true #true))
(check (lambda (x) (gt-len x "abcde")) ints '(#false #false #true #false #false #false))

;; not an int: floats and big integers go to the helper
(define others (list 4.5 5.0 5.5 -3.0 100000000000000000000 -100000000000000000000))

(check lt-c others '(#true #false #false #true #false #true))
(check lte-c others '(#true #true #false #true #false #true))
(check gt-c others '(#false #false #true #false #true #false))
(check gte-c others '(#false #true #true #false #true #false))
(check gt-neg others '(#true #true #true #false #true #false))
(check lt-let others '(#true #false #false #true #false #true))

;; not comparable: the helper raises, and the handler sees it
(define (caught f x) (with-handler (lambda (e) 'caught) (f x)))

(assert! (equal? (caught lt-c "s") 'caught))
(assert! (equal? (caught lte-c "s") 'caught))
(assert! (equal? (caught gt-c "s") 'caught))
(assert! (equal? (caught gte-c "s") 'caught))
(assert! (equal? (caught gt-neg 'a) 'caught))
(assert! (equal? (caught lt-let #\a) 'caught))
