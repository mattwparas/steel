(provide base64-encode
         base64-decode)

(define *alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(define *values-by-char*
  (let loop ([at 0]
             [table (hash)])
    (if (>= at 64)
        table
        (loop (+ at 1) (hash-insert table (string-ref *alphabet* at) at)))))

(define (digit at)
  (string-ref *alphabet* at))

;;@doc
;; Encodes bytes, or the utf8 bytes of a string, as base64 with padding.
;;
;; ```scheme
;; (base64-encode "user:password") ;; => "dXNlcjpwYXNzd29yZA=="
;; ```
(define (base64-encode input)
  (define data
    (if (string? input)
        (string->bytes input)
        input))
  (define len (bytes-length data))
  (define out (open-output-string))
  (let loop ([at 0])
    (when (< at len)
      (let* ([first (bytes-ref data at)]
             [second (if (< (+ at 1) len)
                         (bytes-ref data (+ at 1))
                         0)]
             [third (if (< (+ at 2) len)
                        (bytes-ref data (+ at 2))
                        0)]
             [packed (+ (* first 65536) (* second 256) third)])
        (display (digit (quotient packed 262144)) out)
        (display (digit (modulo (quotient packed 4096) 64)) out)
        (display (if (< (+ at 1) len)
                     (digit (modulo (quotient packed 64) 64))
                     #\=)
                 out)
        (display (if (< (+ at 2) len)
                     (digit (modulo packed 64))
                     #\=)
                 out)
        (loop (+ at 3)))))
  (get-output-string out))

;;@doc
;; Decodes base64 into bytes. Padding and whitespace are optional, and anything
;; outside the alphabet is an error.
(define (digits-of str)
  (let loop ([chars (string->list str)]
             [digits '()]
             [padded #f])
    (cond
      [(empty? chars) (reverse digits)]
      [(char=? (car chars) #\=) (loop (cdr chars) digits #t)]
      [(char-whitespace? (car chars)) (loop (cdr chars) digits padded)]
      ;; Padding ends the input, so `QQ=A` is not two digits and a stray byte.
      [padded (error "base64: a digit after the padding:" str)]
      [else
       (let ([value (hash-try-get *values-by-char* (car chars))])
         (unless value
           (error "base64: not a base64 digit:" (car chars)))
         (loop (cdr chars) (cons value digits) padded))])))

(define (base64-decode str)
  (define digits (digits-of str))
  (define out (bytes))
  ;; The digits are walked rather than indexed, because asking a list its
  ;; length once per group of four is quadratic in the length of the input.
  (let loop ([rest digits])
    (cond
      [(empty? rest) out]
      [(empty? (cdr rest)) (error "base64: the input ends in the middle of a byte")]
      [else
       (let* ([after-two (cdr (cdr rest))]
              [third (if (empty? after-two)
                         #f
                         (car after-two))]
              [after-three (if third
                               (cdr after-two)
                               '())]
              [fourth (if (empty? after-three)
                          #f
                          (car after-three))]
              [packed
               (+ (* (car rest) 262144) (* (car (cdr rest)) 4096) (* (or third 0) 64) (or fourth 0))])
         (bytes-push! out (quotient packed 65536))
         (when third
           (bytes-push! out (modulo (quotient packed 256) 256)))
         (when fourth
           (bytes-push! out (modulo packed 256)))
         (loop (if fourth
                   (cdr after-three)
                   '())))])))
