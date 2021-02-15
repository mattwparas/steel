(struct Tokenizer (buffer delimiter))
(struct Result (token tokenizer))
(struct End (token))

(define (reverse lst)
  (define (reverse-help lst init)
    (if (null? lst)
        init
        (reverse-help (cdr lst) (cons (car lst) init))))
  (reverse-help lst '()))

(define (next-token tokenizer)
  (define delim (Tokenizer-delimiter tokenizer))
  (define (consume-until-delimiter lst buffer)
    (cond [(null? lst) (End (list->string (reverse buffer)))]
          [(equal? (car lst) delim)
           (begin
             (displayln "found a delimiter")
             (Result (list->string (reverse buffer))
                     (Tokenizer (list->string (cdr lst)) delim)))]
          [else
           (consume-until-delimiter (cdr lst) (cons (car lst) buffer))]))
  (displayln (Tokenizer-buffer tokenizer))
  (consume-until-delimiter (string->list (Tokenizer-buffer tokenizer)) '()))

;; (split-single-token input delimiter)

(next-token (Tokenizer "hello world my name is matthew" #\SPACE))

(define (split-single-space input)
  (define (loop result lst)
    (if (Result? result)
        (loop (next-token (Result-tokenizer result))
              (cons (Result-token result) input))
        (cons (End-token result) input)))
  (reverse (loop
            (next-token
             (Tokenizer input #\SPACE))
            '())))


(split-single-space "hello world my name is matthew")




;; (define (levenshtein s t)
;;   (define (*levenshtein s sl t tl)
;;     (displayln s)
;;     (cond ((zero? sl) tl)
;;           ((zero? tl) sl)
;;           (else
;;            (min (+ (*levenshtein (cdr s) (- sl 1) t tl) 1)
;;                 (min
;;                  (+ (*levenshtein s sl (cdr t) (- tl 1)) 1)
;;                  (+ (*levenshtein (cdr s) (- sl 1) (cdr t) (- tl 1))
;;                     (if (equal? (car s) (car t)) 0 1)))))))
;;   (*levenshtein (string->list s)
;;                 (string-length s)
;;                 (string->list t)
;;                 (string-length t)))


;; (levenshtein "cat" "data")

;; (levenshtein "rosettacode" "raisethysword")

;; TODO make generalized memoization library based off of this form
(define *cache* (box (hash)))
(define (update-cache! key value)
  (set-box! *cache* (hash-insert (unbox *cache*) key value)))
(define (cache-contains? key)
  (define inner (unbox *cache*))
  (if (hash-contains? inner key)
      (hash-get inner key)
      #f))


;; TODO make generalized memoization library based off of this form
(define (fib n)
  (when (not (cache-contains? n))
    (update-cache! n (*fib n)))
  (cache-contains? n))

(define (*fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; (fib 100)


;; (define a (unbox *cache*))
;; (define b (update-cache! 'apple 'sauce))
;; (define c (unbox *cache*))



(define (levenshtein s t)
  (define (levenshtein* s sl t tl)
    (define args (list s sl t tl))
    (when (not (cache-contains? args))
      (update-cache! args (*levenshtein s sl t tl)))
    (cache-contains? args))


  (define (*levenshtein s sl t tl)
    (cond ((zero? sl) tl)
          ((zero? tl) sl)
          (else
           (min (+ (levenshtein* (cdr s) (- sl 1) t tl) 1)
                (min
                 (+ (levenshtein* s sl (cdr t) (- tl 1)) 1)
                 (+ (levenshtein* (cdr s) (- sl 1) (cdr t) (- tl 1))
                    (if (equal? (car s) (car t)) 0 1)))))))
  (levenshtein* (string->list s)
                (string-length s)
                (string->list t)
                (string-length t)))

(levenshtein "cat" "dataaaa")
(levenshtein "kitten" "sitting") ;; 3
(levenshtein "rosettacode" "raisethysword")
