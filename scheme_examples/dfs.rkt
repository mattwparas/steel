#lang racket
;; implementation of a dfs in steel

;; (define (shortest-path start end net)
;;   (dfs end (list (list start)) net))


;; (defun bfs (end queue net)
;;   (if (null queue)
;;       nil
;;       (let ((path (car queue)))
;;         (let ((node (car path)))
;;           (if (eql node end)
;;               (reverse path)
;;               (bfs end
;;                    (append (cdr queue)
;;                            (new-paths path node net))
;;                    net))))))

;;
;; (defun new-paths (path node net)
;;   (mapcar #'(lambda (n)
;;               (cons n path))
;;           (cdr (assoc node net))))

;; (define (longest-path start end graph)
;;   (bfs ...))


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

;; (get-neighbors 'a '((a b c) (b d) (e f)))

;; (define (loop neighbors my-best-path next-neighbor end new-path graph))


(define (longest  lst)
  (foldr (λ (a b) (if (> (length a) (length b)) a b))
         '()
         lst))

(define (first-step curr end graph)
  (define neighbors (get-neighbors curr graph))
  (longest (map (lambda (x) (dfs x end '() '() graph)) neighbors)))


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
  (define neighbors (get-neighbors curr graph))
  (define new-path (cons curr path))
  (cond ((equal? curr end)
         (cons curr path))
        ((member curr path)
         '())
        (neighbors
         (try-all-neighbors neighbors best-path end (cons curr path) graph))
        (else '())))

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
