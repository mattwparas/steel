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
