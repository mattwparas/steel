(define haystack '("Zig" "Zag" "Wally" "Ronald" "Bush" "Krusty" "Charlie" "Bush" "Bozo"))

(define index-of
  (lambda (needle hackstack)
    (let ([tail (member needle haystack)])
      (if tail (- (length haystack) (length tail)) (error 'needle-missing)))))

(define last-index-of
  (lambda (needle hackstack)
    (let ([tail (member needle (reverse haystack))])
      (if tail (- (length tail) 1) (error 'needle-missing)))))

(assert! (equal? (index-of "Bush" haystack) 4))
(assert! (equal? (last-index-of "Bush" haystack) 7))
