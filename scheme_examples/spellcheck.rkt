;; ----------- Levenshtein Distance Stuff ------------


(define *cache* (box (hash)))
(define (update-cache! key value)
  (set-box! *cache* (hash-insert (unbox *cache*) key value)))
(define (cache-contains? key)
  (define inner (unbox *cache*))
  (if (hash-contains? inner key)
      (hash-get inner key)
      #f))


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


;; ------------------- utils ------------------------

(define (reduce op z lst)
  (cond ((null? lst) z)    ; just pass it in as another argument
    (else (reduce op
                  (op z (car lst))  ; NB! `z` as first arg
                  (cdr lst)))))

(define (flatten lst)
  (cond ((null? lst) '())
        ((list? lst)
         (append (flatten (car lst)) (flatten (cdr lst))))
        (else (list lst))))

(define (hash-adjust m key func)
  (define value (hash-try-get m key))
  (if value
      (hash-insert m key (func value))
      m))

;; ------------------- BK Tree ----------------------


(struct BKTree (s children))

(define (bk-empty? bktree)
  (and (equal? "" (BKTree-s bktree))
       (zero? (hash-length (BKTree-children bktree)))))

(define empty-bk-tree (BKTree "" (hash)))

;; BKTree -> String -> BKTree
(define (insert-word bktree new-word)
  (define children-map (BKTree-children bktree))
  (define root-word (BKTree-s bktree))
  (define dist (levenshtein root-word new-word))
  (cond [(bk-empty? bktree) (BKTree new-word (hash))]
        [(hash-try-get children-map dist)
         (BKTree root-word (hash-adjust children-map dist
                                        (lambda (x) (insert-word x new-word))))]
        [else
         (BKTree root-word (hash-insert children-map dist (BKTree new-word (hash))))]))



;; Int -> String -> BKTree -> [String]
(define (query n query-word bktree)
  (define root-word (BKTree-s bktree))
  (define ts (BKTree-children bktree))
  (define d (levenshtein root-word query-word))
  (define lower (let ((res (- d n)))
                  (if (< res 0)
                      (* -1 res)
                      res)))
  (define upper (+ d n))
  (define cs (filter (lambda (x) (if x #t #f))
                     (map (lambda (y) (hash-try-get ts y))
                          (range lower upper))))
  (define ms (flatten (map (lambda (x) (query n query-word x)) cs)))
  (if (<= d n)
      (cons root-word ms)
      ms))


(define *bktree* (reduce insert-word empty-bk-tree '("hell" "help" "shel" "smell" "fell" "felt" "oops" "pop" "oouch" "halt")))
(define *edit-distance* 2)

(define (suggest word)
  (query *edit-distance* word *bktree*))


;; (define (q? word)
;;   (query *edit-distance* word *bktree*))
