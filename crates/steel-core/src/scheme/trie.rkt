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

;; contract: (listof char?) (listof tries?) integer? -> (listof trie?)
(define (create-children char-list lst prefix-chars)
  (cond [(= (length char-list) 1)
         (handle-last-letter char-list lst prefix-chars)]
        [else ;; you are in the middle of the word
         (handle-intern-letter char-list lst prefix-chars)]))

;; contract: (listof char?) (listof trie?) integer? -> (listof trie?)
(define (handle-last-letter char-list lst prefix-chars)
  (define char (first char-list))
  ; (define next-prefix (append prefix-chars (list char)))
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

;; contract: (listof char?) (listof trie?) integer? -> (listof trie?)
(define (handle-intern-letter char-list lst prefix-chars)
  (define char (first char-list))
  ; (define next-prefix (append prefix-chars (list char)))
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

;; contract: trie? string? integer? -> trie?
(define (insert root-trie word)
  (define char-list (string->list word))
  (trie
   (trie-char root-trie)
   (create-children char-list (trie-children root-trie) empty)
   (trie-end-word? root-trie)
   (trie-word-up-to root-trie)))

; contract: trie? trie? -> boolean?
(define (trie<? trie-node1 trie-node2)
  (< (trie-char trie-node1) (trie-char trie-node2)))


;; contract: trie? (listof string?) -> trie?
(define (build-trie-from-list-of-words trie list-of-words)
  (cond
    [(= (length list-of-words) 1)
     (insert trie (first list-of-words))]
    [else
     (build-trie-from-list-of-words
      (insert trie (first list-of-words))
      (rest list-of-words))]))

;; ------------------ SORTING ---------------------- ;;

(define (trie-sort list-of-words)
  (define new-trie (build-trie-from-list-of-words empty-trie list-of-words))
  (pre-order new-trie))

; THIS ONE WORKS (using con and flatten)
;; contract: trie? -> (listof string?)
(define (pre-order trie-node)
  (if (trie-end-word? trie-node)
    (cons (list->string (trie-word-up-to trie-node))
      (flatten (map pre-order (trie-children trie-node))))
    (flatten (map pre-order (trie-children trie-node)))))
