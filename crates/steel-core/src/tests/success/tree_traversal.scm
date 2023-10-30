(define (preorder tree)
  (if (null? tree) '() (append (list (car tree)) (preorder (cadr tree)) (preorder (caddr tree)))))

(define (inorder tree)
  (if (null? tree) '() (append (inorder (cadr tree)) (list (car tree)) (inorder (caddr tree)))))

(define (postorder tree)
  (if (null? tree) '() (append (postorder (cadr tree)) (postorder (caddr tree)) (list (car tree)))))

(define (level-order tree)
  (define lst '())
  (define (traverse nodes)
    (when (pair? nodes)
      (let ([next-nodes '()])
        (let loop ([p nodes])
          (when (not (null? p))

            (set! lst (cons (caar p) lst))
            (let* ([n '()]
                   [n (if (null? (cadar p)) n (cons (cadar p) n))]
                   [n (if (null? (caddar p)) n (cons (caddar p) n))])
              (set! next-nodes (append n next-nodes)))

            (loop (cdr p))))

        (traverse (reverse next-nodes)))))
  (if (null? tree)
      '()
      (begin
        (traverse (list tree))
        (reverse lst))))

(define (demonstration tree)
  (define (display-values lst)
    (let loop ([p lst])
      (when (not (null? p))
        (display (car p))
        (when (pair? (cdr p))
          (display " "))

        (loop (cdr p))))
    (newline))
  (display "preorder:    ")
  (display-values (preorder tree))
  (display "inorder:     ")
  (display-values (inorder tree))
  (display "postorder:   ")
  (display-values (postorder tree))
  (display "level-order: ")
  (display-values (level-order tree)))

(define the-task-tree '(1 (2 (4 (7 () ()) ()) (5 () ())) (3 (6 (8 () ()) (9 () ())) ())))

(demonstration the-task-tree)
