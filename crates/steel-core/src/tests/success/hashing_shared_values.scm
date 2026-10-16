;; A value that holds the same value twice at every level, as a key
(define (doubled-with n make)
  (let loop ([i 0]
             [value (make 7 7)])
    (if (= i n) value (loop (+ i 1) (make value value)))))
(struct two (x y) #:transparent)
(define (found-again? make)
  (let ([key (doubled-with 50 make)]
        [same (doubled-with 50 make)])
    (and (eq? 'found (hash-try-get (hash key 'found) same)) (hashset-contains? (hashset key) same))))
(assert! (found-again? list))
(assert! (found-again? immutable-vector))
(assert! (found-again? mutable-vector))
(assert! (found-again? cons))
(assert! (found-again? two))
(assert! (found-again? (lambda (x y) (hash 'a x 'b y))))

;; Hash sets and hash maps that hold the same one twice, as members and keys
(define (told-apart? make)
  (let ([key (doubled-with 50 make)]
        [same (doubled-with 50 make)]
        [shorter (doubled-with 49 make)])
    (and (equal? key same)
         (not (equal? key shorter))
         (eq? 'found (hash-try-get (hash key 'found) same))
         (not (hash-try-get (hash key 'found) shorter)))))
(assert! (told-apart? (lambda (x y) (hashset x (list y)))))
(assert! (told-apart? (lambda (x y) (hashset (hashset x) (hashset y 1)))))
(assert! (told-apart? (lambda (x y) (hash x 'a (list y) 'b))))
(assert! (told-apart? (lambda (x y) (hash (list x) y))))
(assert! (equal? (hashset (hashset 1 2) (hashset 3)) (hashset (hashset 3) (hashset 2 1))))
(assert! (not (equal? (hashset (hashset 1 2) (hashset 3)) (hashset (hashset 3) (hashset 2 4)))))
(assert! (not (equal? (hash (hashset 1) 'a) (hash (hashset 1) 'b))))
(assert! (not (equal? (hash (hashset 1) 'a) (hash (hashset 2) 'a))))

;; Keys hash by what they hold
(assert! (eq? 'found
              (hash-try-get (hash (list 1 (list 2 3) (vector 4 "a")) 'found)
                            (list 1 (list 2 3) (vector 4 "a")))))
(assert! (eq? 'found (hash-try-get (hash (mutable-vector 1 2) 'found) (immutable-vector 1 2))))
(assert! (not (hash-try-get (hash (list 1 2) 'found) (list 1 3))))
(assert! (eq? 'found (hash-try-get (hash (two 1 (list 2)) 'found) (two 1 (list 2)))))
(assert! (eq? 'found (hash-try-get (hash '() 'found) (list))))

;; A serialized value written by hand
(define (le n k)
  (if (= k 0) '() (cons (modulo n 256) (le (quotient n 256) (- k 1)))))
(define (u32 n) (le n 4))
(define (u64 n) (le n 8))
(define (ids l) (append (u64 (length l)) (apply append (map u32 l))))
(define (node-list l) (append (u32 9) (ids l)))
(define (node-set l) (append (u32 8) (ids l)))
(define (node-int n) (append (u32 3) (u64 n)))
(define (node-bignum sign digits)
  (append (u32 14) (list sign) (u64 (length digits)) (apply append (map u32 digits))))
(define format (bytes-ref (serialized->bytes (serialize-value 1)) 0))
(define (read-table nodes value)
  (deserialize-value (bytes->serialized (list->bytes (append (list format)
                                                             (u64 12345)
                                                             (u64 (length nodes))
                                                             (apply append nodes)
                                                             (u32 value)
                                                             (u64 0)
                                                             (u64 0)
                                                             (u64 0))))))

;; A hash set whose member is forty levels of the same list twice
(define levels (map (lambda (i) (node-list (list (+ i 1) (+ i 1)))) (range 1 41)))
(define shared-member (read-table (append (list (node-set (list 1))) levels (list (node-int 7))) 0))
(assert! (= 1 (hashset-length shared-member)))

;; A big number that fits is the number
(define five (read-table (list (node-bignum 1 (list 5))) 0))
(assert! (= five 5))
(assert! (equal? five 5))
(assert! (eq? 'found (hash-try-get (hash 5 'found) five)))
(define zero (read-table (list (node-bignum 0 '())) 0))
(assert! (zero? zero))
(assert! (eq? 'raised (with-handler (lambda (e) 'raised) (quotient 7 zero))))
(assert! (eq? 'raised (with-handler (lambda (e) 'raised) (/ 1 zero))))
