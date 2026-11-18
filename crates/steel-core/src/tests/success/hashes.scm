(require-builtin steel/hashes)

(define (assert-equal! expected actual)
  (unless (equal? expected actual)
    (error "Expected value" expected "but got" actual)))

(define (hex bytes)
  (apply string-append
         (map (lambda (b)
                (let ([digits (number->string b 16)])
                  (if (< b 16)
                      (string-append "0" digits)
                      digits)))
              (bytes->list bytes))))

(define (md5 value)
  (define hasher (md5-hasher))
  (md5-hasher-update! hasher value)
  (hex (md5-hasher->bytes hasher)))

;; Known answers (RFC 1321)
(assert-equal! (md5 "") "d41d8cd98f00b204e9800998ecf8427e")
(assert-equal! (md5 "abc") "900150983cd24fb0d6963f7d28e17f72")
(assert-equal! (md5 "message digest") "f96b697d7cb7938d525a2f31aaf161d0")

(assert-equal! (bytes-length (md5-hasher->bytes (md5-hasher))) 16)

;; Input fed in pieces, strings and bytevectors mixed, hashes the same as all at once
(let ([hasher (md5-hasher)])
  (md5-hasher-update! hasher "ab")
  (md5-hasher-update! hasher (bytes 99))
  (assert-equal! (hex (md5-hasher->bytes hasher)) "900150983cd24fb0d6963f7d28e17f72")
  ;; Taking the digest resets the hasher
  (assert-equal! (hex (md5-hasher->bytes hasher)) "d41d8cd98f00b204e9800998ecf8427e"))

;; Only strings and bytevectors can be hashed
(assert-equal! 'err (with-handler (lambda (e) 'err) (md5-hasher-update! (md5-hasher) 10)))
