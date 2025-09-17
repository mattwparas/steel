(define (assert-equal! expected actual)
  (unless (equal? expected actual)
    (error "expected" expected "but got" actual)))

(assert-equal! (string-append) "")
(assert-equal! (string-append "foo") "foo")
(assert-equal! (string-append "foo" "bar") "foobar")

;; string-length should count chars
(assert-equal! (string-length "one two") 7)
(assert-equal! (string-length "αβγ") 3)
(assert-equal! (string-length "aλ") 2)
(assert-equal! (string-length "✅") 1)
(assert-equal! (string-length "") 0)

;; string->bytes should return utf-8 encoding, so it's length should
;; be the length in utf-8 encoded bytes
(assert-equal! (bytes-length (string->bytes "one two")) 7)
(assert-equal! (bytes-length (string->bytes "αβγ")) 6)
(assert-equal! (bytes-length (string->bytes "aλ")) 3)
(assert-equal! (bytes-length (string->bytes "✅")) 3)
(assert-equal! (bytes-length (string->bytes "")) 0)

;; utf8-length
(assert-equal! (utf8-length "one two") 7)
(assert-equal! (utf8-length "αβγ") 6)
(assert-equal! (utf8-length "aλ") 3)
(assert-equal! (utf8-length "✅") 3)
(assert-equal! (utf8-length "") 0)
