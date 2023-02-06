; #lang racket
(struct trie (char children end-word? word-up-to))

;; Rename functions for the sake of compatibility
(define empty (list))
(define empty-trie (trie void empty #f empty))

;; Throw in a mediocre flatten definition
(define (flatten lst)
  (cond ((null? lst) empty)
        ((list? lst)
         (append (flatten (car lst)) (flatten (cdr lst))))
        (else (list lst))))

(define/contract (create-children char-list lst prefix-chars)
  (->/c (listof char?) (listof trie?) (listof char?) (listof trie?))
  (cond [(= (length char-list) 1)
         (handle-last-letter char-list lst prefix-chars)]
        [else ;; you are in the middle of the word
         (handle-intern-letter char-list lst prefix-chars)]))

(define/contract (handle-last-letter char-list lst prefix-chars)
  (->/c (listof char?) (listof trie?) (listof char?) (listof trie?))
  (define char (first char-list))
  (define next-prefix (push-back prefix-chars char))
  (cond [(empty? lst) ;; children are empty, return list of empty children
         (list (trie char empty #t next-prefix))]
        [(< char (trie-char (first lst))) ;; less than, put it to the left
         (cons (trie char empty #t next-prefix) lst)]
        [(= char (trie-char (first lst))) ;; equal, step down a level
         (cons (trie char (trie-children (first lst)) #t next-prefix) (rest lst))]
        [else ;; move to the right
         (cons (first lst)
               (create-children char-list (rest lst) prefix-chars))]))

(define (handle-intern-letter char-list lst prefix-chars)
  (->/c (listof char?) (listof trie?) (listof char?) (listof trie?))
  (define char (first char-list))
  (define next-prefix (push-back prefix-chars char))
  (cond [(empty? lst) ;; no children, pop off front and step down
         (list (trie char (create-children
                           (rest char-list) empty next-prefix) #f next-prefix))]
        [(< char (trie-char (first lst))) ;; place where it is, pop off front and go
         (cons (trie char (create-children
                           (rest char-list) empty next-prefix) #f next-prefix) lst)]
        [(= char (trie-char (first lst))) ;; equal, step down
         (cons (trie char (create-children (rest char-list) (trie-children (first lst)) next-prefix)
                     (trie-end-word? (first lst))
                     (trie-word-up-to (first lst)))
               (rest lst))]
        [else ; move to the right
         (cons (first lst)
               (create-children char-list (rest lst) prefix-chars))]))


(define/contract (insert root-trie word)
  (->/c trie? string? trie?)
  (define char-list (string->list word))
  (trie
   (trie-char root-trie)
   (create-children char-list (trie-children root-trie) empty)
   (trie-end-word? root-trie)
   (trie-word-up-to root-trie)))

(define/contract (trie<? trie-node1 trie-node2)
  (->/c trie? trie? boolean?)
  (< (trie-char trie-node1) (trie-char trie-node2)))

(define/contract (pre-order-traverse trie-node)
  (->/c trie? any/c)
  (displayln (list (trie-char trie-node) (trie-end-word? trie-node) (trie-word-up-to trie-node)))
  (map pre-order-traverse (trie-children trie-node))
  "finished")

(define/contract (build-trie-from-list-of-words trie list-of-words)
  (->/c trie? (listof string?) trie?)
  (cond
    [(= (length list-of-words) 1)
     (insert trie (first list-of-words))]
    [else
     (build-trie-from-list-of-words
      (insert trie (first list-of-words))
      (rest list-of-words))]))


(define/contract (trie-sort list-of-words)
  (->/c (listof string?) (listof string?))
  (pre-order (build-trie-from-list-of-words empty-trie list-of-words)))

(define/contract (pre-order trie-node)
  (->/c trie? (listof string?))
  (if (trie-end-word? trie-node)
    (cons (list->string (trie-word-up-to trie-node))
      (flatten (map pre-order (trie-children trie-node))))
    (flatten (map pre-order (trie-children trie-node)))))


(define test-list
  (list
   "suppose"
   "believe"
   "changeable"
   "absent"
   "busy"
   "float"
   "debonair"
   "throat"
   "grey"
   "use"
   "measure"
   "van"
   "thirsty"
   "notify"
   "star"
))


(define/contract (generate-trie list-of-words)
  (->/c (listof string?) trie?)
  (build-trie-from-list-of-words empty-trie list-of-words))


(displayln (pre-order (generate-trie test-list)))

