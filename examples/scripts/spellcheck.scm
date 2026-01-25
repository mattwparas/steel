;; ----------- Levenshtein Distance Stuff ------------

;; Get a globally accessibly levenshtein object for holding edit distance
(define *levenshtein-obj* (new-levenshtein))
(define (levenshtein l r)
  (edit-distance *levenshtein-obj* l r))


;; ------------------- utils ------------------------

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
;; TODO make this part tail recursive
(define (insert-word bktree new-word)
  (define children-map (BKTree-children bktree))
  (define root-word (BKTree-s bktree))
  ;; (displayln "calculating...")
  (define dist (levenshtein root-word new-word))
  ;; (displayln "done")
  (cond [(bk-empty? bktree) (BKTree new-word (hash))]
        [(hash-try-get children-map dist)
         (define children (hash-adjust children-map dist
                                        (lambda (x) (insert-word x new-word))))
         (BKTree root-word children)]
        [else
         (define children  (hash-insert children-map dist (BKTree new-word (hash))))
         (BKTree root-word children)]))



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


;; (define *bktree* (reduce insert-word empty-bk-tree '("hell" "help" "shel" "smell" "fell" "felt" "oops" "pop" "oouch" "halt")))
(define *edit-distance* 2)
(define *corpus-path* "/usr/share/dict/words")
(define *corpus-port* (open-input-file *corpus-path*))

;; returns false if can't get the next word
;; otherwise returns the trimmed word
(define (get-next-word!)
  (define line (read-line-from-port *corpus-port*))
  (if (or (eof-object? line) (symbol? line))
      #f
      (trim line)))

;; For now just exclude anything longer than 6 letters for the sake of time
(define (generate bktree func)
    (define next-word (func))
    ; (displayln next-word)
    (if next-word
        (if (> 6 (string-length next-word))
            (generate (insert-word bktree next-word) func)
            (generate bktree func))
        bktree))

(define (read-to-list lst)
  (define next-word (get-next-word!))
  (if next-word
      (read-to-list (cons next-word lst))
      lst))

(displayln "Generating the bk tree")
(define *bktree* (generate empty-bk-tree get-next-word!))
(displayln "Done!")



(define (suggest word)
  (query *edit-distance* word *bktree*))
