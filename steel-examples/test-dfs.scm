;; implementation of a dfs in steel

;; Given a node and a graph
;; node -> 'a
;; graph -> '((a b c) (b d) (e f))
;;
;; returns the neighbors
;; ex.
;; (get-neighbors 'a graph) => '(b c)
;;
(define (get-neighbors node graph)
  (define found-neighbors (assoc node graph))
  (if found-neighbors
      (cdr found-neighbors)
      '()))

(define (longest  lst)
  (foldr (λ (a b) (if (> (length a) (length b)) a b))
         '()
         lst))

; (define (reverse ls)
;   (define (my-reverse-2 ls acc)
;     (if (null? ls)
;       acc
;       (my-reverse-2 (cdr ls) (cons (car ls) acc))))
;   (my-reverse-2 ls '()))

(define (first-step curr end graph)
  (let ((neighbors (get-neighbors curr graph)))
    (longest (map (lambda (x) (dfs x end '() '() graph)) neighbors))))


(define (member? x los)
  (cond
    ((null? los) #f)
    ((equal? x (car los)) #t)
    (else (member? x (cdr los)))))

;; iteratively tries each neighbor
;; quits when the length is worse
(define (try-all-neighbors neighbors best-path end new-path graph)
  (if (not (empty? neighbors))
      (let* ((next-neighbor (car neighbors))
             (found-path (dfs next-neighbor end new-path best-path graph)))
        (if (> (length found-path) (length best-path))
            (try-all-neighbors (cdr neighbors) found-path end new-path graph)
            (try-all-neighbors (cdr neighbors) best-path end new-path graph)))
      best-path))

(define (dfs curr end path best-path graph)
;   (define neighbors (get-neighbors curr graph))
;   (define new-path (cons curr path))
  (let ((neighbors (get-neighbors curr graph))
        (new-path (cons curr path)))
            (cond ((equal? curr end)
                (cons curr path))
                ((member? curr path)
                '())
                (neighbors
                (try-all-neighbors neighbors best-path end (cons curr path) graph))
                (else '()))))


(define (longest-path start end graph)
  (define found-path (reverse (first-step start end graph)))
  (cond ((empty? found-path)
         (if (equal? start end)
             (list start)
             '()))
        ((and (equal? (car found-path) start) (not (equal? start end)))
         found-path)
        (else (cons start found-path))))


(longest-path 'a 'c '((a b) (b c))) ;; '(a b c)
(longest-path 'a 'c '((a b) (b a c))) ;; '(a b c)
(longest-path 'a 'c '((a d e f g b) (b a c))) ;; '(a b c)
(longest-path 'a 'a '((a b) (b a c))) ;; '(a b a)
(longest-path 'a 'c '((a b) (b a) (c))) ;; '()
(longest-path 'a 'f '((a b c) (b f) (c d) (d e) (e f))) ;; '(a c d e f)
(longest-path 'a 'f '((a b c a) (b c d) (c e a) (d e f) (e d f))) ;; '(a b c e d f)
(longest-path 'a 'a '((a b c a) (b c d) (c e a) (d e f) (e d f))) ;; '(a b c a)
(longest-path 'a 'a '((a b) (b c))) ;; '(a)
(longest-path 'a 'a '((a a b) (b c))) ;; '(a a)
(longest-path 'a 'a '((a b a) (b c))) ;; '(a a)
(longest-path 'a 'b '((a b) (b c) (c b))) ;; '(a b)
(longest-path 'a 'b '((a b c) (b c) (c b))) ;; '(a c b)

(equal? (longest-path 'a 'c '((a b) (b c))) '(a b c))
(equal? (longest-path 'a 'c '((a b) (b a c))) '(a b c))
(equal? (longest-path 'a 'c '((a d e f g b) (b a c))) '(a b c))
(equal? (longest-path 'a 'a '((a b) (b a c))) '(a b a))
(equal? (longest-path 'a 'c '((a b) (b a) (c))) '())
(equal? (longest-path 'a 'f '((a b c) (b f) (c d) (d e) (e f))) '(a c d e f))
(equal? (longest-path 'a 'f '((a b c a) (b c d) (c e a) (d e f) (e d f))) '(a b c e d f))
(equal? (longest-path 'a 'a '((a b c a) (b c d) (c e a) (d e f) (e d f))) '(a b c a))
(equal? (longest-path 'a 'a '((a b) (b c))) '(a))
(equal? (longest-path 'a 'a '((a a b) (b c))) '(a a))
(equal? (longest-path 'a 'a '((a b a) (b c))) '(a a))
(equal? (longest-path 'a 'b '((a b) (b c) (c b))) '(a b))
(equal? (longest-path 'a 'b '((a b c) (b c) (c b))) '(a c b))
